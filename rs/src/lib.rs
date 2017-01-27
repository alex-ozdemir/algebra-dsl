#![feature(box_syntax, box_patterns, slice_patterns, advanced_slice_patterns)]
#[macro_use]
extern crate nom;
mod parser;

use parser::ParseError;

use std::{fmt, mem};

#[derive(PartialEq, Debug, Clone)]
pub struct Equation {
    left: Expression,
    right: Expression,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Side { Left, Right }

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct EquationIdx<'a> {
    side: Side,
    expr_idx: ExpressionIdx<'a>,
}

pub type ExpressionIdx<'a> = &'a [usize];

impl Equation {
    pub fn get(&self, index: EquationIdx) -> Result<&Expression, AlgebraDSLError> {
        (match index.side {
            Side::Left => &self.left,
            Side::Right => &self.right,
        }).get(index.expr_idx)
    }
    pub fn get_mut(&mut self, index: EquationIdx) -> Result<&mut Expression, AlgebraDSLError> {
        (match index.side {
            Side::Left => &mut self.left,
            Side::Right => &mut self.right,
        }).get_mut(index.expr_idx)
    }
    pub fn replace_with_expr(&mut self, index: EquationIdx, expr: Expression) -> Result<Expression, AlgebraDSLError> {
        let subtree = self.get_mut(index)?;
        let old = subtree.take();
        *subtree = expr;
        Ok(old)
    }
    pub fn replace_with_str(&mut self, index: EquationIdx, expr: &str) -> Result<Expression, AlgebraDSLError> {
        self.replace_with_expr(index, Expression::from_str(expr)?)
    }
    pub fn from_str(eq: &str) -> Result<Self, AlgebraDSLError> {
        parser::parse_equation(eq).map_err(AlgebraDSLError::Parse)
    }
}

impl Expression {
    pub fn from_str(expr: &str) -> Result<Self, AlgebraDSLError> {
        parser::parse_expr(expr).map_err(AlgebraDSLError::Parse)
    }
    pub fn take(&mut self) -> Self {
        mem::replace(self, Expression::Atom(Atom::Natural(0)))
    }
    pub fn get(&self, index: ExpressionIdx) -> Result<&Self, AlgebraDSLError> {
        if index.len() == 0 {
            Ok(self)
        } else {
            let first = index[index.len()-1];
            let rest = &index[0..index.len()-1];
            match self {
                &Expression::Negation(ref e) if first == 0 => e.get(rest),
                &Expression::Sum(ref e) if first < e.len() => e[first].get(rest),
                &Expression::Product(ref e) if first < e.len() => e[first].get(rest),
                &Expression::Division(ref top, _) if first == 0 => top.get(rest),
                &Expression::Division(_, ref bot) if first == 1 => bot.get(rest),
                &Expression::Power(ref base, _) if first == 0 => base.get(rest),
                &Expression::Power(_, ref power) if first == 1 => power.get(rest),
                &Expression::Subscript(ref base, _) if first == 0 => base.get(rest),
                &Expression::Subscript(_, ref script) if first == 1 => script.get(rest),
                &Expression::Application(ref func, _) if first == 0 => func.get(rest),
                &Expression::Application(_, ref arg) if first == 1 => arg.get(rest),
                _ => Err(AlgebraDSLError::InvalidIdx),
            }
        }
    }
    pub fn get_mut(&mut self, index: ExpressionIdx) -> Result<&mut Self, AlgebraDSLError> {
        if index.len() == 0 {
            Ok(self)
        } else {
            let first = index[index.len()-1];
            let rest = &index[0..index.len()-1];
            match self {
                &mut Expression::Negation(ref mut e) if first == 0 => e.get_mut(rest),
                &mut Expression::Sum(ref mut e) if first < e.len() => e[first].get_mut(rest),
                &mut Expression::Product(ref mut e) if first < e.len() => e[first].get_mut(rest),
                &mut Expression::Division(ref mut top, _) if first == 0 => top.get_mut(rest),
                &mut Expression::Division(_, ref mut bot) if first == 1 => bot.get_mut(rest),
                &mut Expression::Power(ref mut base, _) if first == 0 => base.get_mut(rest),
                &mut Expression::Power(_, ref mut power) if first == 1 => power.get_mut(rest),
                &mut Expression::Subscript(ref mut base, _) if first == 0 => base.get_mut(rest),
                &mut Expression::Subscript(_, ref mut script) if first == 1 => script.get_mut(rest),
                &mut Expression::Application(ref mut func, _) if first == 0 => func.get_mut(rest),
                &mut Expression::Application(_, ref mut arg) if first == 1 => arg.get_mut(rest),
                _ => Err(AlgebraDSLError::InvalidIdx),
            }
        }
    }
}

pub enum AlgebraDSLError {
    Parse(ParseError),
    InvalidIdx,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Negation(Box<Expression>),
    Sum(Vec<Expression>),
    Product(Vec<Expression>),
    /// Division(numerator, denominator)
    Division(Box<Expression>, Box<Expression>),
    Power(Box<Expression>, Box<Expression>),
    Subscript(Box<Expression>, Box<Expression>),
    Application(Box<Expression>, Box<Expression>),
    /// An indivisible unit, like a variable or numeric literal
    Atom(Atom),
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Atom {
    PlainVariable(char),
    Natural(u64),
    Floating(f64),
    Symbol(Symbol),
}

impl Expression {
    pub fn fmt_as_math_ml(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        // Tim can write code here
        unimplemented!()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Symbol {
    /// Symbols which are effectively variable names
    Standalone(StandaloneSymbol),
    /// Symbols which act as operators -- they expect something after them
    Operator(OperatorSymbol),
}

impl Symbol {
    pub fn expects_op_after(&self) -> bool {
        match self {
            &Symbol::Standalone(_) => true,
            &Symbol::Operator(_) => false,
        }
    }
    pub fn expects_op_before(&self) -> bool {
        true
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StandaloneSymbol {
    // Lowercase Greek Letters
    alpha,
    beta,
    gamma,
    delta,
    epsilon,
    zeta,
    eta,
    theta,
    iota,
    kappa,
    lambda,
    mu,
    nu,
    omicron,
    pi,
    rho,
    sigma,
    tau,
    upsilon,
    phi,
    chi,
    psi,
    omega,
    // Uppercase Greek Letters
    Gamma,
    Delta,
    Theta,
    Lambda,
    Pi,
    Sigma,
    Upsilon,
    Phi,
    Psi,
    Omega,
    // Other Lone Symbols
    partial,
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperatorSymbol {
    // Operators
    int,
    oint,
    sum,
    prod,
    // Functions
    arccos,
    cos,
    csc,
    exp,
    limsup,
    min,
    sinh,
    arcsin,
    cosh,
    gcd,
    lg,
    ln,
    sup,
    arctan,
    cot,
    det,
    lim,
    log,
    sec,
    tan,
    coth,
    inf,
    liminf,
    max,
    sin,
    tanh,
}

impl Symbol {
    fn from_str(s: &str) -> Option<Self> {
        match s {
            "alpha" => Some(Symbol::Standalone(StandaloneSymbol::alpha)),
            "beta" => Some(Symbol::Standalone(StandaloneSymbol::beta)),
            "gamma" => Some(Symbol::Standalone(StandaloneSymbol::gamma)),
            "delta" => Some(Symbol::Standalone(StandaloneSymbol::delta)),
            "epsilon" => Some(Symbol::Standalone(StandaloneSymbol::epsilon)),
            "zeta" => Some(Symbol::Standalone(StandaloneSymbol::zeta)),
            "eta" => Some(Symbol::Standalone(StandaloneSymbol::eta)),
            "theta" => Some(Symbol::Standalone(StandaloneSymbol::theta)),
            "iota" => Some(Symbol::Standalone(StandaloneSymbol::iota)),
            "kappa" => Some(Symbol::Standalone(StandaloneSymbol::kappa)),
            "lambda" => Some(Symbol::Standalone(StandaloneSymbol::lambda)),
            "mu" => Some(Symbol::Standalone(StandaloneSymbol::mu)),
            "nu" => Some(Symbol::Standalone(StandaloneSymbol::nu)),
            "omicron" => Some(Symbol::Standalone(StandaloneSymbol::omicron)),
            "pi" => Some(Symbol::Standalone(StandaloneSymbol::pi)),
            "rho" => Some(Symbol::Standalone(StandaloneSymbol::rho)),
            "sigma" => Some(Symbol::Standalone(StandaloneSymbol::sigma)),
            "tau" => Some(Symbol::Standalone(StandaloneSymbol::tau)),
            "upsilon" => Some(Symbol::Standalone(StandaloneSymbol::upsilon)),
            "phi" => Some(Symbol::Standalone(StandaloneSymbol::phi)),
            "chi" => Some(Symbol::Standalone(StandaloneSymbol::chi)),
            "psi" => Some(Symbol::Standalone(StandaloneSymbol::psi)),
            "omega" => Some(Symbol::Standalone(StandaloneSymbol::omega)),
            "Gamma" => Some(Symbol::Standalone(StandaloneSymbol::Gamma)),
            "Delta" => Some(Symbol::Standalone(StandaloneSymbol::Delta)),
            "Theta" => Some(Symbol::Standalone(StandaloneSymbol::Theta)),
            "Lambda" => Some(Symbol::Standalone(StandaloneSymbol::Lambda)),
            "Pi" => Some(Symbol::Standalone(StandaloneSymbol::Pi)),
            "Sigma" => Some(Symbol::Standalone(StandaloneSymbol::Sigma)),
            "Upsilon" => Some(Symbol::Standalone(StandaloneSymbol::Upsilon)),
            "Phi" => Some(Symbol::Standalone(StandaloneSymbol::Phi)),
            "Psi" => Some(Symbol::Standalone(StandaloneSymbol::Psi)),
            "Omega" => Some(Symbol::Standalone(StandaloneSymbol::Omega)),
            "partial" => Some(Symbol::Standalone(StandaloneSymbol::partial)),
            "int" => Some(Symbol::Operator(OperatorSymbol::int)),
            "oint" => Some(Symbol::Operator(OperatorSymbol::oint)),
            "sum" => Some(Symbol::Operator(OperatorSymbol::sum)),
            "prod" => Some(Symbol::Operator(OperatorSymbol::prod)),
            "arccos" => Some(Symbol::Operator(OperatorSymbol::arccos)),
            "cos" => Some(Symbol::Operator(OperatorSymbol::cos)),
            "csc" => Some(Symbol::Operator(OperatorSymbol::csc)),
            "exp" => Some(Symbol::Operator(OperatorSymbol::exp)),
            "limsup" => Some(Symbol::Operator(OperatorSymbol::limsup)),
            "min" => Some(Symbol::Operator(OperatorSymbol::min)),
            "sinh" => Some(Symbol::Operator(OperatorSymbol::sinh)),
            "arcsin" => Some(Symbol::Operator(OperatorSymbol::arcsin)),
            "cosh" => Some(Symbol::Operator(OperatorSymbol::cosh)),
            "gcd" => Some(Symbol::Operator(OperatorSymbol::gcd)),
            "lg" => Some(Symbol::Operator(OperatorSymbol::lg)),
            "ln" => Some(Symbol::Operator(OperatorSymbol::ln)),
            "sup" => Some(Symbol::Operator(OperatorSymbol::sup)),
            "arctan" => Some(Symbol::Operator(OperatorSymbol::arctan)),
            "cot" => Some(Symbol::Operator(OperatorSymbol::cot)),
            "det" => Some(Symbol::Operator(OperatorSymbol::det)),
            "lim" => Some(Symbol::Operator(OperatorSymbol::lim)),
            "log" => Some(Symbol::Operator(OperatorSymbol::log)),
            "sec" => Some(Symbol::Operator(OperatorSymbol::sec)),
            "tan" => Some(Symbol::Operator(OperatorSymbol::tan)),
            "coth" => Some(Symbol::Operator(OperatorSymbol::coth)),
            "inf" => Some(Symbol::Operator(OperatorSymbol::inf)),
            "liminf" => Some(Symbol::Operator(OperatorSymbol::liminf)),
            "max" => Some(Symbol::Operator(OperatorSymbol::max)),
            "sin" => Some(Symbol::Operator(OperatorSymbol::sin)),
            "tanh" => Some(Symbol::Operator(OperatorSymbol::tanh)),
            _ => None,
        }
    }
}
