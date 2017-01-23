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
    Atom(Atom),
}

pub enum Atom {
    PlainVariable(char),
    SubscriptedVariable(char, Box<Variable>),
    Natural(u64),
}

impl Expression {
    fn fmt_as_math_ml(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        unimplemented!()
    }
}

enum KnownMacros {
    Frac,
    Dfrac,
    Cdot,
    Times,
    Div,
    // Int,
    // Partial,
}

pub enum KnownLatex {
    Frac(Box<KnownLatex>, Box<KnownLatex>),
    List(Vec<KnownLatex>),
    Operator(Operator),
    Atom(Atom),
}

fn parse(input: latex::Token) -> Expression {
    unimplemented!()
}
