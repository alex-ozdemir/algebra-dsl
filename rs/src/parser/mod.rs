//! The `latex` module contains a parser which produces LaTeX tokens
//!
//! We then convert those into `Expression`s

pub mod latex;
use std::fmt;

pub enum Expression {
    Negation(Box<Expression>),
    Sum(Vec<Expression>),
    Product(Vec<Expression>),
    Division(Box<Expression>, Box<Expression>),
    Variable(Variable),
    Natural(u64),
}

pub enum Variable {
    Plain(char),
    Subscripted(char, Box<Variable>)
}

impl Expression {
    fn fmt_as_math_ml(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        unimplemented!()
    }
}
