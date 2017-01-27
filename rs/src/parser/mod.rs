//! The `latex` module contains a parser which produces LaTeX tokens
//!
//! We then convert those into `Expression`s

pub mod latex;
use self::latex::{Token, Special};
use std::fmt;
use nom::IResult;
use {Equation, Expression, Symbol, Atom, StandaloneSymbol, OperatorSymbol};


/// The Control Sequences that our system handles. A small set of macros along with a *bunch* of
/// symbols.
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum KnownCS {
    // TODO: Greek letters
    frac,
    dfrac,
    sqrt,
    cdot,
    times,
    div,
    Preserved(Symbol),
}

impl KnownCS {
    fn from_str(s: &str) -> Option<KnownCS> {
        match s {
            "frac" => Some(KnownCS::frac),
            "dfrac" => Some(KnownCS::dfrac),
            "cdot" => Some(KnownCS::cdot),
            "times" => Some(KnownCS::times),
            "sqrt" => Some(KnownCS::sqrt),
            "div" => Some(KnownCS::div),
            s => Symbol::from_str(s).map(KnownCS::Preserved)
        }
    }
}


#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Operator {
    Times,
    Plus,
    Minus,
    Neg,
    Div,
    Underscore,
    Caret,
    LGroup,
    RGroup,
    /// Used only within the Operator Parser. Indicates beginning of expression
    Begin,
    /// Used only within the Operator Parser. Indicates end of expression
    End,
}

impl Operator {
    fn left_precedence(&self) -> u8 {
        use self::Operator::*;
        match *self {
            Begin => panic!("We should never look left of Begin"),
            End => u8::min_value(),
            RGroup => u8::min_value()+1,
            Plus | Minus => 15,
            Times | Div=> 25,
            Neg => 35,
            Caret => 45,
            Underscore => 55,
            LGroup => u8::max_value()-1,
        }
    }
    fn right_precedence(&self) -> u8 {
        use self::Operator::*;
        match *self {
            Begin => u8::min_value(),
            End => panic!("We should never look right of End"),
            LGroup => u8::min_value()+1,
            Plus | Minus => 16,
            Times | Div => 26,
            Neg => 34,
            Caret => 44,
            Underscore => 54,
            RGroup => u8::max_value()-1,
        }
    }
    fn arity(&self) -> u8 {
        use self::Operator::*;
        match *self {
            Begin | End | LGroup | RGroup => panic!("No arity for these"),
            Plus | Minus | Times | Div | Caret | Underscore => 2,
            Neg => 1,
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum KnownLatex {
    Frac(Box<KnownLatex>, Box<KnownLatex>),
    Sqrt(Box<KnownLatex>),
    List(Vec<KnownLatex>),
    Standalone(StandaloneSymbol),
    Operator(UniOp),
    Char(char),
    Natural(u64),
}

#[derive(PartialEq, Eq, Debug)]
pub enum UniOp {
    Std(Operator),
    Symbol(OperatorSymbol),
}

impl UniOp {
    fn arity(&self) -> u8 {
        match self {
            &UniOp::Std(op) => op.arity(),
            &UniOp::Symbol(_) => 1,
        }
    }
    fn left_precedence(&self) -> u8 {
        match self {
            &UniOp::Std(op) => op.left_precedence(),
            &UniOp::Symbol(_) => 20,
        }
    }
    fn right_precedence(&self) -> u8 {
        match self {
            &UniOp::Std(op) => op.right_precedence(),
            &UniOp::Symbol(_) => 19,
        }
    }
}

impl KnownLatex {
    /// Return whether there should be an operator after this type of LaTeX construct.
    fn expects_op_after(&self) -> bool {
        match self {
            &KnownLatex::List(ref list) => list.last().expect("No empty lists!").expects_op_after(),
            &KnownLatex::Frac(_, _) => true,
            &KnownLatex::Sqrt(_) => true,
            &KnownLatex::Operator(UniOp::Std(Operator::RGroup)) => true,
            &KnownLatex::Standalone(_) => true,
            &KnownLatex::Operator(_) => false,
            &KnownLatex::Char(_) => true,
            &KnownLatex::Natural(_) => true,
        }
    }
    /// Return whether there should be an operator before this type of LaTeX construct.
    fn expects_op_before(&self) -> bool {
        match self {
            &KnownLatex::Frac(_, _) => true,
            &KnownLatex::Sqrt(_) => true,
            &KnownLatex::List(ref list) => list.first().expect("No empty lists!").expects_op_before(),
            &KnownLatex::Operator(UniOp::Std(Operator::LGroup)) => true,
            &KnownLatex::Operator(_) => false,
            &KnownLatex::Standalone(_) => true,
            &KnownLatex::Char(_) => true,
            &KnownLatex::Natural(_) => true,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    UnknownSpecialChar(Special),
    FracNotFollowedByTwoExprs,
    UnknownControlSequence(String),
    ControlSequenceCannotStandalone(KnownCS),
    OperatorSymbolCannotStandalone(OperatorSymbol),
    EmptyList,
    LoneOperator(UniOp),
    OperatorError,
    LatexError(String),
    WrongNumberOfEqualSigns,
}

pub fn to_known(input: latex::Token) -> Result<KnownLatex, ParseError> {
    match input {
        Token::Special(Special::Star) => Ok(KnownLatex::Operator(UniOp::Std(Operator::Times))),
        Token::Special(Special::LParen) => Ok(KnownLatex::Operator(UniOp::Std(Operator::LGroup))),
        Token::Special(Special::RParen) => Ok(KnownLatex::Operator(UniOp::Std(Operator::RGroup))),
        Token::Special(Special::Plus) => Ok(KnownLatex::Operator(UniOp::Std(Operator::Plus))),
        Token::Special(Special::LSquareBracket) => Ok(KnownLatex::Operator(UniOp::Std(Operator::LGroup))),
        Token::Special(Special::RSquareBracket) => Ok(KnownLatex::Operator(UniOp::Std(Operator::RGroup))),
        Token::Special(Special::Dash) => Ok(KnownLatex::Operator(UniOp::Std(Operator::Minus))),
        Token::Special(Special::Divide) => Ok(KnownLatex::Operator(UniOp::Std(Operator::Div))),
        Token::Special(Special::Caret) => Ok(KnownLatex::Operator(UniOp::Std(Operator::Caret))),
        Token::Special(Special::Underscore) => Ok(KnownLatex::Operator(UniOp::Std(Operator::Underscore))),
        Token::Special(x) => Err(ParseError::UnknownSpecialChar(x)),
        Token::Char(c) => Ok(KnownLatex::Char(c)),
        Token::Natural(n) => Ok(KnownLatex::Natural(n)),
        Token::ControlSequence(cs) => {
            let known_cs = KnownCS::from_str(cs.as_str()).ok_or(ParseError::UnknownControlSequence(cs))?;
            match known_cs {
                KnownCS::div => Ok(KnownLatex::Operator(UniOp::Std(Operator::Div))),
                KnownCS::cdot | KnownCS::times => Ok(KnownLatex::Operator(UniOp::Std(Operator::Times))),
                KnownCS::Preserved(Symbol::Standalone(sym)) => Ok(KnownLatex::Standalone(sym)),
                KnownCS::Preserved(Symbol::Operator(sym)) => Ok(KnownLatex::Operator(UniOp::Symbol(sym))),
                x => Err(ParseError::ControlSequenceCannotStandalone(x)),
            }
        }
        Token::List(mut list) => {
            list.reverse();
            let mut result_list = vec![];
            while let Some(next) = list.pop() {
                let mut next = match to_known(next) {
                    Err(ParseError::ControlSequenceCannotStandalone(KnownCS::dfrac)) | 
                    Err(ParseError::ControlSequenceCannotStandalone(KnownCS::frac)) => {
                        let (first, second) = two_expressions(&mut list)?;
                        KnownLatex::Frac(box first, box second)
                    }
                    Err(ParseError::ControlSequenceCannotStandalone(KnownCS::sqrt)) => {
                        let argument = one_expression(&mut list)?;
                        KnownLatex::Sqrt(box argument)
                    }
                    e@Err(_) => return e,
                    Ok(x) => x,
                };
                let op_expected = result_list.last()
                                             .map(KnownLatex::expects_op_after)
                                             .unwrap_or(false);
                let implicit_times = op_expected && next.expects_op_before();
                if implicit_times {
                    result_list.push(KnownLatex::Operator(UniOp::Std(Operator::Times)));
                }
                if next == KnownLatex::Operator(UniOp::Std(Operator::Minus)) && !op_expected {
                    next = KnownLatex::Operator(UniOp::Std(Operator::Neg));
                }
                result_list.push(next);
            }
            match result_list.len() {
                0 => Err(ParseError::EmptyList),
                1 => Ok(result_list.pop().unwrap()),
                _ => Ok(KnownLatex::List(result_list)),
            }
        }
    }
}

fn one_expression(input: &mut Vec<latex::Token>) -> Result<KnownLatex, ParseError> {
    let first = to_known(input.pop().ok_or(ParseError::FracNotFollowedByTwoExprs)?)?;
    Ok(first)
}

fn two_expressions(input: &mut Vec<latex::Token>) -> Result<(KnownLatex, KnownLatex), ParseError> {
    let first = to_known(input.pop().ok_or(ParseError::FracNotFollowedByTwoExprs)?)?;
    let second = to_known(input.pop().ok_or(ParseError::FracNotFollowedByTwoExprs)?)?;
    Ok((first, second))
}

fn parse_operators(input: KnownLatex) -> Result<Expression, ParseError> {
    match input {
        KnownLatex::Sqrt(radical) => Ok(Expression::Power(box parse_operators(*radical)?, box Expression::Atom(Atom::Floating(0.5)))),
        KnownLatex::Char(c) => Ok(Expression::Atom(Atom::PlainVariable(c))),
        KnownLatex::Standalone(sym) => Ok(Expression::Atom(Atom::Symbol(Symbol::Standalone(sym)))),
        KnownLatex::Natural(c) => Ok(Expression::Atom(Atom::Natural(c))),
        KnownLatex::Operator(o) => Err(ParseError::LoneOperator(o)),
        KnownLatex::Frac(top, bottom) => {
            let parsed_top = parse_operators(*top)?;
            let parsed_bottom = parse_operators(*bottom)?;
            Ok(Expression::Division(box parsed_top, box parsed_bottom))
        }
        KnownLatex::List(mut tokens) => {
            let mut operator_stack = vec![UniOp::Std(Operator::Begin)];
            let mut expression_stack = vec![];
            tokens.push(KnownLatex::Operator(UniOp::Std(Operator::End)));
            tokens.reverse();
            while let Some(token) = tokens.pop() {
                println!("New Round\n\tOps:   {:?}\n\tExprs: {:?}", operator_stack, expression_stack);
                match token {
                    KnownLatex::Operator(next_op) => {
                        while operator_stack.last().unwrap().right_precedence() > next_op.left_precedence() {
                            let combinator = operator_stack.pop().unwrap();
                            let second = expression_stack.pop().ok_or(ParseError::OperatorError)?;
                            let new_expr = if combinator.arity() == 1 {
                                combine1(second, combinator)
                            } else {
                                let first = expression_stack.pop().ok_or(ParseError::OperatorError)?;
                                combine2(first, combinator, second)
                            };
                            expression_stack.push(new_expr);
                        }
                        operator_stack.push(next_op);
                    },
                    KnownLatex::List(mut next_list) => {
                        next_list.reverse();
                        // FIXME(aozdemir): Here, we treat latex {} as grouping operators. This
                        // may not be correct, but it helps us handle ^{} and _{}.
                        tokens.push(KnownLatex::Operator(UniOp::Std(Operator::RGroup)));
                        tokens.extend(next_list);
                        tokens.push(KnownLatex::Operator(UniOp::Std(Operator::LGroup)));
                    }
                    expr => expression_stack.push(parse_operators(expr)?),
                };
                if let &[_.., UniOp::Std(Operator::LGroup), UniOp::Std(Operator::RGroup)] = operator_stack.as_slice() {
                    operator_stack.pop(); operator_stack.pop();
                }
            }
            debug_assert!(&operator_stack[..] == &[UniOp::Std(Operator::Begin), UniOp::Std(Operator::End)]);
            if expression_stack.len() == 1 {
                Ok(expression_stack.pop().unwrap())
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

fn combine1(expr: Expression, op: UniOp) -> Expression {
    use self::Operator::*;
    match op {
        UniOp::Std(Neg) => Expression::Negation(box expr),
        UniOp::Symbol(sym) => Expression::Application(box Expression::Atom(Atom::Symbol(Symbol::Operator(sym))), box expr),
        _ => panic!("Combine1 called with non-unary operator"),
    }
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

fn parse_equation(input: &str) -> Result<Equation, ParseError> {
    let mut sides = input.split("=").collect::<Vec<_>>();
    if sides.len() != 2 {
        return Err(ParseError::WrongNumberOfEqualSigns);
    }
    let right = parse_expr(sides.pop().unwrap())?;
    let left = parse_expr(sides.pop().unwrap())?;
    Ok(Equation{ left: left, right: right })
}

fn parse_expr(input: &str) -> Result<Expression, ParseError> {
    let latex_tokens = latex::parse_tokens(input).map_err(ParseError::LatexError)?;
    println!("{:#?}", latex_tokens);
    let expanded = to_known(latex_tokens)?;
    println!("{:#?}", expanded);
    parse_operators(expanded)
}

#[cfg(test)]
mod tests;
