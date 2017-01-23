//! The `latex` module contains a parser which produces LaTeX tokens
//!
//! We then convert those into `Expression`s

pub mod latex;
use self::latex::{Token, Special};
use std::fmt;

pub struct Equation {
    left: Expression,
    right: Expression,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Expression {
    Negation(Box<Expression>),
    Sum(Vec<Expression>),
    Product(Vec<Expression>),
    Division(Box<Expression>, Box<Expression>),
    Atom(Atom),
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Atom {
    PlainVariable(char),
    Natural(u64),
}

impl Expression {
    fn fmt_as_math_ml(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        unimplemented!()
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum KnownCS {
    // TODO: Greek letters
    Frac,
    Dfrac,
    Cdot,
    Times,
    Div,
    // Int,
    // Partial,
}

impl KnownCS {
    fn from_str(s: &str) -> Option<KnownCS> {
        match s {
            "frac" => Some(KnownCS::Frac),
            "dfrac" => Some(KnownCS::Dfrac),
            "cdot" => Some(KnownCS::Cdot),
            "times" => Some(KnownCS::Times),
            "div" => Some(KnownCS::Div),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Operator {
    Times,
    Plus,
    Minus,
    Div,
    LGroup,
    RGroup,
}

#[derive(PartialEq, Eq, Debug)]
pub enum KnownLatex {
    Frac(Box<KnownLatex>, Box<KnownLatex>),
    List(Vec<KnownLatex>),
    Operator(Operator),
    Char(char),
    Number(u64),
}

impl KnownLatex {
    /// Return whether there should be an operator after this type of LaTeX construct.
    fn expects_op_after(&self) -> bool {
        match self {
            &KnownLatex::Frac(_, _) => true,
            &KnownLatex::List(ref list) => list.last().expect("No empty lists!").expects_op_after(),
            &KnownLatex::Operator(Operator::RGroup) => true,
            &KnownLatex::Operator(_) => false,
            &KnownLatex::Char(_) => true,
            &KnownLatex::Number(_) => true,
        }
    }
    /// Return whether there should be an operator before this type of LaTeX construct.
    fn expects_op_before(&self) -> bool {
        match self {
            &KnownLatex::Frac(_, _) => true,
            &KnownLatex::List(ref list) => list.first().expect("No empty lists!").expects_op_before(),
            &KnownLatex::Operator(Operator::LGroup) => true,
            &KnownLatex::Operator(_) => false,
            &KnownLatex::Char(_) => true,
            &KnownLatex::Number(_) => true,
        }
    }
}

pub enum ParseError {
    UnknownSpecialChar(Special),
    FracNotFollowedByTwoExprs,
    UnknownControlSequence(String),
    ControlSequenceCannotStandalone(KnownCS),
    EmptyList,
}

pub fn to_known(input: latex::Token) -> Result<KnownLatex, ParseError> {
    match input {
        Token::Special(Special::Star) => Ok(KnownLatex::Operator(Operator::Times)),
        Token::Special(Special::LParen) => Ok(KnownLatex::Operator(Operator::LGroup)),
        Token::Special(Special::RParen) => Ok(KnownLatex::Operator(Operator::RGroup)),
        Token::Special(Special::Plus) => Ok(KnownLatex::Operator(Operator::Plus)),
        Token::Special(Special::LSquareBracket) => Ok(KnownLatex::Operator(Operator::LGroup)),
        Token::Special(Special::RSquareBracket) => Ok(KnownLatex::Operator(Operator::RGroup)),
        Token::Special(x) => Err(ParseError::UnknownSpecialChar(x)),
        Token::Char(c) => Ok(KnownLatex::Char(c)),
        Token::Number(n) => Ok(KnownLatex::Number(n)),
        Token::ControlSequence(cs) => {
            let known_cs = KnownCS::from_str(cs.as_str()).ok_or(ParseError::UnknownControlSequence(cs))?;
            match known_cs {
                KnownCS::Div => Ok(KnownLatex::Operator(Operator::Div)),
                KnownCS::Cdot | KnownCS::Times => Ok(KnownLatex::Operator(Operator::Times)),
                x => Err(ParseError::ControlSequenceCannotStandalone(x)),
            }
        }
        Token::List(mut list) => {
            list.reverse();
            let mut result_list = vec![];
            while let Some(next) = list.pop() {
                let next = match to_known(next) {
                    Err(ParseError::ControlSequenceCannotStandalone(KnownCS::Dfrac)) | 
                    Err(ParseError::ControlSequenceCannotStandalone(KnownCS::Frac)) => {
                        let (first, second) = two_expressions(&mut list)?;
                        KnownLatex::Frac(box first, box second)
                    }
                    e@Err(_) => return e,
                    Ok(x) => x,
                };
                let implicit_times = result_list.last().map(KnownLatex::expects_op_after).unwrap_or(false) && next.expects_op_before();
                if implicit_times {
                    result_list.push(KnownLatex::Operator(Operator::Times));
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

fn parse(input: latex::Token) -> Expression {
    unimplemented!()
}
