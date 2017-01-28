#![feature(box_syntax, box_patterns, slice_patterns, advanced_slice_patterns)]
#![allow(dead_code)]
#[macro_use]
extern crate nom;
mod parser;

use parser::ParseError;
use std::{fmt, mem};
use std::string::String;


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

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let start = "0";
        write!(f,"<math xmlns=\"http://www.w3.org/1998/Math/MathML\">");
        fmt_as_math_ml(&self, f, start);
        write!(f,"</math>")
}
}

fn fmt_as_math_ml(expr: &Expression, f: &mut fmt::Formatter, prev_index: &str) -> Result<(), fmt::Error> {
    match expr {

        &Expression::Atom(atom) => {let mut base_string = String::from(prev_index);
            write!(f,"<mrow mathTreeNode=\"{}\">{}</mrow>", base_string, atom)},

        &Expression::Power(ref b,ref p) => {
            let mut base_string = String::from(prev_index);
            write!(f,"<mrow mathTreeNode=\"{}\"><msup>",base_string);
            base_string.push_str(",0");
            fmt_as_math_ml(b, f, &base_string);
            let mut base_string = String::from(prev_index);
            base_string.push_str(",1");
            fmt_as_math_ml(p, f, &base_string);
            write!(f,"</msup></mrow>")},

        &Expression::Negation(ref n) => {
            let mut base_string = String::from(prev_index);
            write!(f,"<mrow mathTreeNode=\"{}\"><mo>-</mo>", base_string);
            base_string.push_str(",0");
            fmt_as_math_ml(n, f, &base_string);
            write!(f,"</mrow>")},

        &Expression::Division(ref n, ref d) => {
            let mut base_string = String::from(prev_index);
            write!(f, "<mrow mathTreeNode=\"{}\"><mfrac>", base_string);
            base_string.push_str(",0");
            fmt_as_math_ml(n, f, &base_string);
            let mut base_string = String::from(prev_index);
            base_string.push_str(",1");
            fmt_as_math_ml(d, f, &base_string);
            write!(f,"</mfrac></mrow>")},

        &Expression::Subscript(ref e, ref s) => {
            let mut base_string = String::from(prev_index);
            write!(f, "<mrow mathTreeNode=\"{}\"><msub>", base_string);
            base_string.push_str(",0");
            fmt_as_math_ml(e, f, &base_string);
            let mut base_string = String::from(prev_index);
            base_string.push_str(",1");
            fmt_as_math_ml(s, f, &base_string);
            write!(f,"</msub></mrow>")},

        &Expression::Sum(ref s) => {
            let mut base_string = String::from(prev_index);
            write!(f,"<mrow mathTreeNode=\"{}\">", base_string);
            let len = s.len();
            let iter = s.iter().enumerate();
            for (i,e) in iter {
                let mut base_string = String::from(prev_index);
                base_string.push_str(",");
                base_string.push_str(&i.to_string());
                if i == len - 1 {
                    fmt_as_math_ml(e, f, &base_string);
                }
                else {
                    fmt_as_math_ml(e, f, &base_string);
                    write!(f,"<mo>+</mo>");
                }
            }
            write!(f,"</mrow>")},

        &Expression::Product(ref s) => {
            let mut base_string = String::from(prev_index);
            write!(f,"<mrow mathTreeNode=\"{}\">", base_string);
            let len = s.len();
            let iter = s.iter().enumerate();
            for (i,e) in iter {
                let mut base_string = String::from(prev_index);
                base_string.push_str(",");
                base_string.push_str(&i.to_string());
                if i == len - 1 {
                    fmt_as_math_ml(e, f, &base_string);
                }
                else {
                    fmt_as_math_ml(e, f, &base_string);
                    write!(f,"<mo>&#8290;</mo>");
                }
            }
            write!(f,"</mrow>")},

        _ => write!(f,"unimplemented")
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Atom {
    PlainVariable(char),
    Natural(u64),
    Floating(f64),
    Symbol(Symbol),
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Atom::PlainVariable(c) => write!(f,"<mi>{}</mi>",c),
            &Atom::Natural(n) => write!(f,"<mn>{}</mn>",n),
            &Atom::Floating(r) => write!(f,"<mn>{}</mn>",r),
            &Atom::Symbol(s) => write!(f,"No idea how to write symbols.")
        }
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
    fn format_power() {
        let expr = Expression::Power(box Expression::Atom(Atom::PlainVariable('x')), box Expression::Atom(Atom::PlainVariable('y')));
        let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow mathTreeNode=\"0\"><msup><mrow mathTreeNode=\"0,0\"><mi>x</mi></mrow><mrow mathTreeNode=\"0,1\"><mi>y</mi></mrow></msup></mrow></math>";
        let test = format!("{}", expr);
        assert_expected_eq_actual!(expected, test);
    }

    #[test]
    fn format_frac() {
        let expr = Expression::Division(box Expression::Atom(Atom::PlainVariable('x')), box Expression::Atom(Atom::PlainVariable('y')));
        let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow mathTreeNode=\"0\"><mfrac><mrow mathTreeNode=\"0,0\"><mi>x</mi></mrow><mrow mathTreeNode=\"0,1\"><mi>y</mi></mrow></mfrac></mrow></math>";
        let test = format!("{}", expr);
        assert_expected_eq_actual!(expected, test);
    }

    #[test]
    fn format_negation() {
        let expr = Expression::Negation(box Expression::Atom(Atom::PlainVariable('x')));
        let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow mathTreeNode=\"0\"><mo>-</mo><mrow mathTreeNode=\"0,0\"><mi>x</mi></mrow></mrow></math>";
        let test = format!("{}", expr);
        assert_expected_eq_actual!(expected, test);
    }

    #[test]
    fn format_sub() {
        let expr = Expression::Subscript(box Expression::Atom(Atom::PlainVariable('x')), box Expression::Atom(Atom::PlainVariable('y')));
        let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow mathTreeNode=\"0\"><msub><mrow mathTreeNode=\"0,0\"><mi>x</mi></mrow><mrow mathTreeNode=\"0,1\"><mi>y</mi></mrow></msub></mrow></math>";
        let test = format!("{}", expr);
        assert_expected_eq_actual!(expected, test);
    }

    #[test]
    fn format_add() {
        let expr = Expression::Sum(vec![Expression::Atom(Atom::PlainVariable('x')), Expression::Atom(Atom::PlainVariable('y')), Expression::Atom(Atom::PlainVariable('z'))]);
        let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow mathTreeNode=\"0\"><mrow mathTreeNode=\"0,0\"><mi>x</mi></mrow><mo>+</mo><mrow mathTreeNode=\"0,1\"><mi>y</mi></mrow><mo>+</mo><mrow mathTreeNode=\"0,2\"><mi>z</mi></mrow></mrow></math>";
        let test = format!("{}", expr);
        assert_expected_eq_actual!(expected, test);
    }

    #[test]
    fn format_prod() {
        let expr = Expression::Product(vec![Expression::Atom(Atom::PlainVariable('x')), Expression::Atom(Atom::PlainVariable('y')), Expression::Atom(Atom::PlainVariable('z'))]);
        let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow mathTreeNode=\"0\"><mrow mathTreeNode=\"0,0\"><mi>x</mi></mrow><mo>&#8290;</mo><mrow mathTreeNode=\"0,1\"><mi>y</mi></mrow><mo>&#8290;</mo><mrow mathTreeNode=\"0,2\"><mi>z</mi></mrow></mrow></math>";
        let test = format!("{}", expr);
        assert_expected_eq_actual!(expected, test);
    }


}
