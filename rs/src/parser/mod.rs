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
    List(Vec<KnownLatex>),
    Symbol(Symbol),
    Operator(Operator),
    Char(char),
    Natural(u64),
}

impl KnownLatex {
    /// Return whether there should be an operator after this type of LaTeX construct.
    fn expects_op_after(&self) -> bool {
        match self {
            &KnownLatex::List(ref list) => list.last().expect("No empty lists!").expects_op_after(),
            &KnownLatex::Frac(_, _) => true,
            &KnownLatex::Operator(Operator::RGroup) => true,
            &KnownLatex::Symbol(sym) => sym.expects_op_after(),
            &KnownLatex::Operator(_) => false,
            &KnownLatex::Char(_) => true,
            &KnownLatex::Natural(_) => true,
        }
    }
    /// Return whether there should be an operator before this type of LaTeX construct.
    fn expects_op_before(&self) -> bool {
        match self {
            &KnownLatex::Frac(_, _) => true,
            &KnownLatex::List(ref list) => list.first().expect("No empty lists!").expects_op_before(),
            &KnownLatex::Operator(Operator::LGroup) => true,
            &KnownLatex::Symbol(sym) => sym.expects_op_before(),
            &KnownLatex::Operator(_) => false,
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
    EmptyList,
    LoneOperator(Operator),
    OperatorError,
    LatexError(String),
    WrongNumberOfEqualSigns,
}

pub fn to_known(input: latex::Token) -> Result<KnownLatex, ParseError> {
    match input {
        Token::Special(Special::Star) => Ok(KnownLatex::Operator(Operator::Times)),
        Token::Special(Special::LParen) => Ok(KnownLatex::Operator(Operator::LGroup)),
        Token::Special(Special::RParen) => Ok(KnownLatex::Operator(Operator::RGroup)),
        Token::Special(Special::Plus) => Ok(KnownLatex::Operator(Operator::Plus)),
        Token::Special(Special::LSquareBracket) => Ok(KnownLatex::Operator(Operator::LGroup)),
        Token::Special(Special::RSquareBracket) => Ok(KnownLatex::Operator(Operator::RGroup)),
        Token::Special(Special::Dash) => Ok(KnownLatex::Operator(Operator::Minus)),
        Token::Special(Special::Divide) => Ok(KnownLatex::Operator(Operator::Div)),
        Token::Special(Special::Caret) => Ok(KnownLatex::Operator(Operator::Caret)),
        Token::Special(Special::Underscore) => Ok(KnownLatex::Operator(Operator::Underscore)),
        Token::Special(x) => Err(ParseError::UnknownSpecialChar(x)),
        Token::Char(c) => Ok(KnownLatex::Char(c)),
        Token::Natural(n) => Ok(KnownLatex::Natural(n)),
        Token::ControlSequence(cs) => {
            let known_cs = KnownCS::from_str(cs.as_str()).ok_or(ParseError::UnknownControlSequence(cs))?;
            match known_cs {
                KnownCS::div => Ok(KnownLatex::Operator(Operator::Div)),
                KnownCS::cdot | KnownCS::times => Ok(KnownLatex::Operator(Operator::Times)),

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
                    e@Err(_) => return e,
                    Ok(x) => x,
                };
                let op_expected = result_list.last()
                                             .map(KnownLatex::expects_op_after)
                                             .unwrap_or(false);
                let implicit_times = op_expected && next.expects_op_before();
                if implicit_times {
                    result_list.push(KnownLatex::Operator(Operator::Times));
                }
                if next == KnownLatex::Operator(Operator::Minus) && !op_expected {
                    next = KnownLatex::Operator(Operator::Neg);
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

fn two_expressions(input: &mut Vec<latex::Token>) -> Result<(KnownLatex, KnownLatex), ParseError> {
    let first = to_known(input.pop().ok_or(ParseError::FracNotFollowedByTwoExprs)?)?;
    let second = to_known(input.pop().ok_or(ParseError::FracNotFollowedByTwoExprs)?)?;
    Ok((first, second))
}

fn parse_operators(input: KnownLatex) -> Result<Expression, ParseError> {
    match input {
        KnownLatex::Symbol(_) => unimplemented!(),
        KnownLatex::Char(c) => Ok(Expression::Atom(Atom::PlainVariable(c))),
        KnownLatex::Natural(c) => Ok(Expression::Atom(Atom::Natural(c))),
        KnownLatex::Operator(o) => Err(ParseError::LoneOperator(o)),
        KnownLatex::Frac(top, bottom) => {
            let parsed_top = parse_operators(*top)?;
            let parsed_bottom = parse_operators(*bottom)?;
            Ok(Expression::Division(box parsed_top, box parsed_bottom))
        }
        KnownLatex::List(mut tokens) => {
            let mut operator_stack = vec![Operator::Begin];
            let mut expression_stack = vec![];
            tokens.push(KnownLatex::Operator(Operator::End));
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
                        tokens.push(KnownLatex::Operator(Operator::RGroup));
                        tokens.extend(next_list);
                        tokens.push(KnownLatex::Operator(Operator::LGroup));
                    }
                    expr => expression_stack.push(parse_operators(expr)?),
                };
                if let &[_.., Operator::LGroup, Operator::RGroup] = operator_stack.as_slice() {
                    operator_stack.pop(); operator_stack.pop();
                }
            }
            debug_assert!(&operator_stack[..] == &[Operator::Begin, Operator::End]);
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

fn combine1(expr: Expression, op: Operator) -> Expression {
    match op {
        Operator::Neg => Expression::Negation(box expr),
        _ => panic!("Combine1 called with non-unary operator"),
    }
}

fn combine2(left: Expression, op: Operator, right: Expression) -> Expression {
    match op {
        Operator::Times => combine_associative!(left, Expression::Product, right),
        Operator::Plus => combine_associative!(left, Expression::Sum, right),
        Operator::Minus => combine2(left, Operator::Plus, Expression::Negation(box right)),
        Operator::Div => Expression::Division(box left, box right),
        Operator::Caret => Expression::Power(box left, box right),
        Operator::Underscore => Expression::Subscript(box left, box right),
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
mod tests {
    use super::*;

    // This macro is an assertion with nicely formatted failure output
    macro_rules! assert_expected_eq_actual {
        ($a:expr, $b:expr) => ({
            let (a, b) = (&$a, &$b);
            assert!(*a == *b,
                    "\nExpected `{:?}` is not equal to Actual `{:?}`\
                     \nAssertion: `assert_expected_eq_actual!({}, {})`",
                    *a,
                    *b,
                    stringify!($a),
                    stringify!($b));
        })
    }

    #[test]
    fn just_a_variable() {
        assert_expected_eq_actual!(Ok(Expression::Atom(Atom::PlainVariable('x'))), parse_expr("x"));
    }

    #[test]
    fn single_digit_natural() {
        assert_expected_eq_actual!(Ok(Expression::Atom(Atom::Natural(1))), parse_expr("1"));
    }

    #[test]
    fn multi_digit_natural() {
        assert_expected_eq_actual!(Ok(Expression::Atom(Atom::Natural(123465))), parse_expr("123465"));
    }

    #[test]
    fn single_addition() {
        let expected = Expression::Sum(vec![Expression::Atom(Atom::PlainVariable('x')),
                                            Expression::Atom(Atom::Natural(2))]);
        assert_expected_eq_actual!(Ok(expected), parse_expr("x +  2"));
    }

    #[test]
    fn multi_addition() {
        let expected = Expression::Sum(vec![Expression::Atom(Atom::PlainVariable('x')),
                                            Expression::Atom(Atom::Natural(2)),
                                            Expression::Atom(Atom::Natural(9))]);
        assert_expected_eq_actual!(Ok(expected), parse_expr("x +  2+9"));
    }

    #[test]
    fn negation() {
        let expected = Expression::Negation(box Expression::Atom(Atom::PlainVariable('y')));
        assert_expected_eq_actual!(Ok(expected), parse_expr("-y"));
    }

    #[test]
    fn negation_and_addition() {
        let expected =
            Expression::Sum(vec![Expression::Negation(box Expression::Atom(Atom::PlainVariable('x'))),
                                 Expression::Atom(Atom::Natural(2)),
                                 Expression::Atom(Atom::Natural(9)),
                                 Expression::Negation(box Expression::Atom(Atom::Natural(7)))]);
        assert_expected_eq_actual!(Ok(expected), parse_expr("-x +  2+9-7"));
    }

    #[test]
    fn double_negation_and_addition() {
        let expected =
            Expression::Sum(vec![Expression::Negation(box Expression::Negation(
                                    box Expression::Atom(Atom::PlainVariable('x')))
                                 ),
                                 Expression::Atom(Atom::Natural(2)),
                                 Expression::Atom(Atom::Natural(9)),
                                 Expression::Negation(box Expression::Atom(Atom::Natural(7)))]);
        assert_expected_eq_actual!(Ok(expected), parse_expr("--x +  2+9-7"));
    }

    #[test]
    fn single_multiplication() {
        let expected = Expression::Product(vec![Expression::Atom(Atom::PlainVariable('x')),
                                            Expression::Atom(Atom::Natural(2))]);
        assert_expected_eq_actual!(Ok(expected), parse_expr("x \\times  2"));
    }

    #[test]
    fn multi_multiplication() {
        let expected = Expression::Product(vec![Expression::Atom(Atom::PlainVariable('x')),
                                            Expression::Atom(Atom::Natural(2)),
                                            Expression::Atom(Atom::Natural(9)),
                                            Expression::Atom(Atom::Natural(8))]);
        assert_expected_eq_actual!(Ok(expected), parse_expr("x \\times  2 \\cdot 9 * 8"));
    }

    #[test]
    fn implicit_multiplication() {
        let expected = Expression::Product(vec![Expression::Atom(Atom::Natural(2)),
                                            Expression::Atom(Atom::PlainVariable('x')),
                                            Expression::Atom(Atom::Natural(9)),
                                            Expression::Atom(Atom::Natural(8))]);
        assert_expected_eq_actual!(Ok(expected), parse_expr("2x9(8)"));
    }

    #[test]
    fn implicit_multiplication_adjacent_parens() {
        let expected = Expression::Product(vec![Expression::Atom(Atom::Natural(2)),
                                            Expression::Atom(Atom::PlainVariable('x')),
                                            Expression::Atom(Atom::Natural(9)),
                                            Expression::Atom(Atom::Natural(8))]);
        assert_expected_eq_actual!(Ok(expected), parse_expr("2x(9)(8)"));
    }

    #[test]
    fn addition_and_multiplication() {
        let expected =
            Expression::Sum(vec![Expression::Product(vec![Expression::Atom(Atom::Natural(4)),
                                                          Expression::Atom(Atom::PlainVariable('x'))]),
                                 Expression::Atom(Atom::Natural(2))]);
        assert_expected_eq_actual!(Ok(expected), parse_expr("4x + 2"));
    }

    #[test]
    fn addition_and_multiplication_grouping() {
        let expected =
            Expression::Product(vec![Expression::Sum(vec![Expression::Atom(Atom::Natural(4)),
                                                          Expression::Atom(Atom::PlainVariable('x'))]),
                                 Expression::Atom(Atom::Natural(2))]);
        assert_expected_eq_actual!(Ok(expected), parse_expr("(4 + x)2"));
    }

    #[test]
    fn single_div() {
        let expected = Expression::Division(box Expression::Atom(Atom::PlainVariable('x')),
                                            box Expression::Atom(Atom::Natural(2)));
        assert_expected_eq_actual!(Ok(expected), parse_expr("x /2"));
    }

    #[test]
    fn single_frac() {
        let expected = Expression::Division(box Expression::Atom(Atom::PlainVariable('x')),
                                            box Expression::Atom(Atom::Natural(2)));
        assert_expected_eq_actual!(Ok(expected), parse_expr("\\frac x 2"));
    }

    #[test]
    fn single_frac_with_braces() {
        let expected = Expression::Division(box Expression::Atom(Atom::PlainVariable('x')),
                                            box Expression::Atom(Atom::Natural(2)));
        assert_expected_eq_actual!(Ok(expected), parse_expr("\\dfrac{x}{2}"));
    }

    #[test]
    fn multi_frac() {
        let expected =
            Expression::Division(box Expression::Division(box Expression::Atom(Atom::Natural(4)),
                                                          box Expression::Atom(Atom::PlainVariable('x'))),
                                 box Expression::Atom(Atom::Natural(2)));
        assert_expected_eq_actual!(Ok(expected), parse_expr("\\frac{\\frac4x}{2}"));
    }

    #[test]
    fn power() {
        let expected = Expression::Power(box Expression::Atom(Atom::Natural(4)),
                                         box Expression::Atom(Atom::PlainVariable('x')));
        assert_expected_eq_actual!(Ok(expected), parse_expr("4^x"));
    }

    #[test]
    fn power_grouped_exponent() {
        let expected = Expression::Power(box Expression::Atom(Atom::Natural(4)),
                                         box Expression::Sum(vec![
                                            Expression::Atom(Atom::PlainVariable('x')),
                                            Expression::Atom(Atom::Natural(1))]));
        assert_expected_eq_actual!(Ok(expected), parse_expr("4^{x+1}"));
    }

    #[test]
    fn subscript() {
        let expected = Expression::Subscript(box Expression::Atom(Atom::Natural(4)),
                                             box Expression::Atom(Atom::PlainVariable('x')));
        assert_expected_eq_actual!(Ok(expected), parse_expr("4_x"));
    }

    #[test]
    fn subscript_grouped() {
        let expected = Expression::Subscript(box Expression::Atom(Atom::Natural(4)),
                                             box Expression::Sum(vec![
                                                Expression::Atom(Atom::PlainVariable('x')),
                                                Expression::Atom(Atom::Natural(1))]));
        assert_expected_eq_actual!(Ok(expected), parse_expr("4_{x+1}"));
    }

    #[test]
    fn equation() {
        let expected = Equation{ left: Expression::Atom(Atom::PlainVariable('x')),
                right: Expression::Atom(Atom::Natural(4)) };
        assert_expected_eq_actual!(Ok(expected), parse_equation("x=4"));
    }
}
