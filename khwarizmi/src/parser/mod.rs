//! The `latex` module contains a parser which produces LaTeX tokens
//!
//! We then convert those into `Expression`s

pub mod latex;
pub mod mac;
use self::latex::{Token, Special};
use std::num;
use std::str::FromStr;
use {Equation, Expression, Symbol, Atom, StandaloneSymbol, OperatorSymbol};
use self::mac::{PostMac, UniOp, Operator, KnownCS, Numeric};

const UNREACH: &'static str = "An option/result that was expected to be Some/Ok was not.\n\
                               This is a bug!";

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    UnknownSpecialChar(Special),
    FracNotFollowedByTwoExprs,
    UnknownControlSequence(String),
    LoneControlSequence(KnownCS),
    EmptyList,
    LoneOperator(UniOp),
    OperatorError,
    LatexError(String),
    WrongNumberOfEqualSigns,
    DoubleUnderscore,
    UnmatchGrouping(UniOp),
    DoubleCaret,
    UnimplementedCharacter(char),
    DoubleDecimalPoint,
    InvalidNumericLiteral,
    FPError(num::ParseFloatError),
}

fn parse_operators(input: PostMac) -> Result<Expression, ParseError> {
    match input {
        PostMac::Sqrt(radical) => {
            Ok(Expression::Power(box parse_operators(*radical)?,
                                 box Expression::Atom(Atom::Floating(0.5))))
        }
        PostMac::Char(c) => Ok(Expression::Atom(Atom::PlainVariable(c))),
        PostMac::Standalone(sym) => Ok(Expression::Atom(Atom::Symbol(Symbol::Standalone(sym)))),
        PostMac::Op(o) => Err(ParseError::LoneOperator(o)),
        PostMac::Frac(top, bottom) => {
            let parsed_top = parse_operators(*top)?;
            let parsed_bottom = parse_operators(*bottom)?;
            Ok(Expression::Division(box parsed_top, box parsed_bottom))
        }
        PostMac::Num(Numeric(natural, None)) => {
            i64::from_str_radix(natural.as_str(), 10)
                .ok()
                .ok_or(ParseError::InvalidNumericLiteral)
                .map(Atom::Natural)
                .map(Expression::Atom)
        }
        PostMac::Num(Numeric(natural, Some(dec))) => {
            let s = format!("{}.{}", natural, dec);
            f64::from_str(s.as_str())
                .map_err(|e| ParseError::FPError(e))
                .map(Atom::Floating)
                .map(Expression::Atom)
        }
        PostMac::List(mut tokens) => {
            // We maintain an invariant that the `operator_stack` is non-empty
            let mut operator_stack = vec![UniOp::Std(Operator::Begin)];
            let mut expression_stack = vec![];
            tokens.push(PostMac::Op(UniOp::Std(Operator::End)));
            tokens.reverse();
            while let Some(token) = tokens.pop() {
                println!("New Round\n\tOps:   {:?}\n\tExprs: {:?}",
                         operator_stack,
                         expression_stack);
                match token {
                    PostMac::Op(next_op) => {
                        while operator_stack.last().expect(UNREACH).right_precedence() >
                              next_op.left_precedence() {
                            let combinator = operator_stack.pop().expect(UNREACH);
                            let second = expression_stack.pop().ok_or(ParseError::OperatorError)?;
                            let new_expr = if combinator.arity()
                                .ok_or_else(|| ParseError::UnmatchGrouping(combinator.clone()))? ==
                                              1 {
                                combine1(second, combinator)?
                            } else {
                                let first = expression_stack.pop()
                                    .ok_or(ParseError::OperatorError)?;
                                combine2(first, combinator, second)
                            };
                            expression_stack.push(new_expr);
                        }
                        operator_stack.push(next_op);
                    }
                    PostMac::List(mut next_list) => {
                        next_list.reverse();
                        // FIXME(aozdemir): Here, we treat latex {} as grouping operators. This
                        // may not be correct, but it helps us handle ^{} and _{}.
                        tokens.push(PostMac::Op(UniOp::Std(Operator::RGroup)));
                        tokens.extend(next_list);
                        tokens.push(PostMac::Op(UniOp::Std(Operator::LGroup)));
                    }
                    expr => expression_stack.push(parse_operators(expr)?),
                };
                if let &[_.., UniOp::Std(Operator::LGroup), UniOp::Std(Operator::RGroup)] =
                    operator_stack.as_slice() {
                    operator_stack.pop();
                    operator_stack.pop();
                }
            }
            debug_assert!(&operator_stack[..] ==
                          &[UniOp::Std(Operator::Begin), UniOp::Std(Operator::End)]);
            if expression_stack.len() == 1 {
                Ok(expression_stack.pop().expect(UNREACH))
            } else {
                Err(ParseError::OperatorError)
            }
        }
    }
}

macro_rules! combine_associative {
    ($left:ident, $operation:path, $right: ident) => {
        {
            match ($left, $right) {
                ($operation(mut a), $operation(b)) => {
                    a.extend(b);
                    $operation(a)
                },
                ($operation(mut a), b) => {
                    a.push(b);
                    $operation(a)
                },
                (a, $operation(mut b)) => {
                    b.insert(0, a);
                    $operation(b)
                },
                (a, b) => $operation(vec![a, b]),
            }
        }
    };
}

fn combine1(expr: Expression, op: UniOp) -> Result<Expression, ParseError> {
    use self::Operator::*;
    Ok(match op {
        UniOp::Std(Neg) => Expression::Negation(box expr),
        UniOp::LimitOp(sym, sub, sup) => {
            let sub_expr = match sub { // I don't use map because `?` doesn't work in || {}
                Some(box sub) => Some(parse_operators(sub)?),
                None => None,
            };
            let sup_expr = match sup { // I don't use map because `?` doesn't work in || {}
                Some(box sup) => Some(parse_operators(sup)?),
                None => None,
            };
            Expression::LimitOp(sym,
                                sub_expr.map(Box::new),
                                sup_expr.map(Box::new),
                                box expr)
        }
        _ => panic!("Combine1 called with non-unary operator"),
    })
}

fn combine2(left: Expression, op: UniOp, right: Expression) -> Expression {
    use self::Operator::*;
    use self::UniOp::*;
    match op {
        Std(Times) => combine_associative!(left, Expression::Product, right),
        Std(Plus) => combine_associative!(left, Expression::Sum, right),
        Std(Minus) => combine2(left, Std(Operator::Plus), Expression::Negation(box right)),
        Std(Div) => Expression::Division(box left, box right),
        Std(Caret) => Expression::Power(box left, box right),
        Std(Underscore) => Expression::Subscript(box left, box right),
        _ => panic!("Combine2 called with non-binary operator"),
    }
}

pub fn parse_equation(input: &str) -> Result<Equation, ParseError> {
    let mut sides = input.split("=").collect::<Vec<_>>();
    if sides.len() != 2 {
        return Err(ParseError::WrongNumberOfEqualSigns);
    }
    let right = parse_expr(sides.pop().expect(UNREACH))?;
    let left = parse_expr(sides.pop().expect(UNREACH))?;
    Ok(Equation {
        left: left,
        right: right,
    })
}

pub fn parse_expr(input: &str) -> Result<Expression, ParseError> {
    let latex_tokens = latex::parse_tokens(input).map_err(ParseError::LatexError)?;
    println!("{:#?}", latex_tokens);
    let expanded = mac::to_known(latex_tokens)?;
    println!("{:#?}", expanded);
    parse_operators(expanded)
}

#[cfg(test)]
mod tests;
