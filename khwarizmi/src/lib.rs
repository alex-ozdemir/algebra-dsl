#![feature(box_syntax, box_patterns, slice_patterns, advanced_slice_patterns)]
#![allow(dead_code)]
#[macro_use]
extern crate nom;
mod parser;

use parser::ParseError;
use std::{fmt, mem};
use std::str::FromStr;
use std::string::String;

const NULL_EXPRESSION: Expression = Expression::Atom(Atom::Natural(0));
const NULL_IDX: &'static [TreeInt] = &[];

#[derive(PartialEq, Debug, Clone)]
pub struct Equation {
    left: Expression,
    right: Expression,
}

impl TreeIdx {
    pub fn from_str(s: &str) -> Result<Self, AlgebraDSLError> {
        if !(s.starts_with("#(") && s.ends_with(")")) {
            Err(AlgebraDSLError::IllFormattedIndex)
        } else {
            let s = &s[2..s.len() - 1];
            let mut idxs = s.split(',')
                .map(|d| usize::from_str(d).map_err(|_| AlgebraDSLError::IllFormattedIndex));
            // Pop the first index, because every expression/equation is in the trivial 0 idx
            debug_assert!(Some(Ok(0)) == idxs.next());
            let mut v = vec![];
            for idx in idxs {
                v.push(idx?);
            }
            Ok(TreeIdx(v))
        }
    }
    fn as_ref<'a>(&'a self) -> TreeIdxRef<'a> {
        TreeIdxRef(&self.0[..])
    }
}


type TreeInt = usize;


#[derive(PartialEq, Eq, Debug, Clone)]
pub struct TreeIdx(Vec<TreeInt>);

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct TreeIdxRef<'a>(&'a [TreeInt]);

impl<'a> TreeIdxRef<'a> {
    pub fn first(&self) -> Option<TreeInt> {
        if self.len() > 0 {
            Some(self.0[0])
        } else {
            None
        }
    }
    pub fn rest(&self) -> TreeIdxRef<'a> {
        TreeIdxRef(if self.len() > 0 {
            &self.0[1..]
        } else {
            NULL_IDX
        })
    }
    pub fn parent(&self) -> Option<TreeIdxRef<'a>> {
        if self.len() > 0 {
            Some(TreeIdxRef(&self.0[..self.len() - 1]))
        } else {
            None
        }
    }
    pub fn last(&self) -> Option<usize> {
        if self.len() > 0 {
            Some(self.0[self.len() - 1])
        } else {
            None
        }
    }
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl Equation {
    pub fn from_str(eq: &str) -> Result<Self, AlgebraDSLError> {
        parser::parse_equation(eq).map_err(AlgebraDSLError::Parse)
    }
    pub fn plus_to_both(&mut self, expr: Expression) {
        self.left = mem::replace(&mut self.left, NULL_EXPRESSION).inflate_addition(expr.clone());
        self.right = mem::replace(&mut self.right, NULL_EXPRESSION).inflate_addition(expr);
    }
    pub fn times_to_both(&mut self, expr: Expression) {
        self.left = mem::replace(&mut self.left, NULL_EXPRESSION)
            .inflate_multiplication(expr.clone());
        self.right = mem::replace(&mut self.right, NULL_EXPRESSION).inflate_multiplication(expr);
    }
}

impl Indexable for Equation {
    fn get(&self, index: TreeIdxRef) -> Result<&Expression, AlgebraDSLError> {
        println!("index {:?} in {:#?}", index, self);
        match index.first() {
            Some(0) => self.left.get(index.rest()),
            Some(1) => self.right.get(index.rest()),
            _ => Err(AlgebraDSLError::InvalidIdx),
        }
    }
    fn get_mut(&mut self, index: TreeIdxRef) -> Result<&mut Expression, AlgebraDSLError> {
        match index.first() {
            Some(0) => self.left.get_mut(index.rest()),
            Some(1) => self.right.get_mut(index.rest()),
            _ => Err(AlgebraDSLError::InvalidIdx),
        }
    }
    fn as_equation(&mut self) -> Option<&mut Equation> {
        Some(self)
    }
}

pub struct SiblingIndices<'a> {
    parent_idx: TreeIdxRef<'a>,
    // This list *must* be sorted
    children: Vec<TreeInt>,
}

impl<'a> SiblingIndices<'a> {
    pub fn from_indices(indices: &'a [TreeIdx]) -> Result<SiblingIndices<'a>, AlgebraDSLError> {
        if indices.len() < 2 || indices.iter().map(|i| i.as_ref().len()).min().unwrap() == 0 {
            return Err(AlgebraDSLError::InvalidSiblingIndices);
        }
        let first_parent = indices[0].as_ref().parent();
        if indices.iter().all(|idx| idx.as_ref().parent() == first_parent) {
            let mut vec: Vec<_> = indices.iter().map(|i| i.as_ref().last().unwrap()).collect();
            vec.sort();
            let first_parent = first_parent.expect("unreachable");
            Ok(SiblingIndices {
                parent_idx: first_parent,
                children: vec,
            })
        } else {
            Err(AlgebraDSLError::InvalidSiblingIndices)
        }
    }

    /// Get the smallest child index
    fn lowest_child_index(&self) -> usize {
        self.children[0]
    }
}

pub trait Indexable: fmt::Display + fmt::Debug {
    fn get(&self, index: TreeIdxRef) -> Result<&Expression, AlgebraDSLError>;
    fn get_mut(&mut self, index: TreeIdxRef) -> Result<&mut Expression, AlgebraDSLError>;
    fn replace(&mut self,
               index: &TreeIdx,
               expr: Expression)
               -> Result<Expression, AlgebraDSLError> {
        let old = {
            let subtree = self.get_mut(index.as_ref())?;
            let old = subtree.take();
            *subtree = expr;
            old
        };
        index.as_ref().parent().map(|idx| self.maybe_assoc_merge_with_parent(idx));
        Ok(old)
    }
    fn delete(&mut self, index: &TreeIdx) -> Result<Expression, AlgebraDSLError> {
        if let Some(parent_index) = index.as_ref().parent() {
            let child_index = index.as_ref().last().unwrap();
            match self.get_mut(parent_index)? {
                &mut Expression::Sum(ref mut args) |
                &mut Expression::Product(ref mut args) => {
                    if child_index < args.len() {
                        Ok(args.remove(child_index))
                    } else {
                        Err(AlgebraDSLError::InvalidDelete)
                    }
                }
                _ => Err(AlgebraDSLError::InvalidDelete),
            }
        } else {
            Err(AlgebraDSLError::InvalidDelete)
        }
    }
    fn replace_siblings(&mut self,
                        indices: SiblingIndices,
                        expr: Expression)
                        -> Result<(), AlgebraDSLError> {
        match self.get_mut(indices.parent_idx)? {
            &mut Expression::Sum(ref mut args) |
            &mut Expression::Product(ref mut args) => {
                for idx in indices.children.iter().rev() {
                    args.remove(*idx);
                }
                args.insert(indices.lowest_child_index(), expr);
                Ok(())
            }
            _ => Err(AlgebraDSLError::InvalidSiblingIndices),
        }
    }
    fn replace_with_str(&mut self,
                        index: &TreeIdx,
                        expr: &str)
                        -> Result<Expression, AlgebraDSLError> {
        self.replace(index, Expression::from_str(expr)?)
    }
    fn as_equation(&mut self) -> Option<&mut Equation> {
        None
    }

    /// Looks at the expression indicated by `index` and merges it with any associative children of
    /// the same type.
    ///
    /// Reports whether a merge occured, or if the index was invalid
    fn maybe_assoc_merge_with_parent(&mut self, idx: TreeIdxRef) -> Result<(), AlgebraDSLError> {
        let expr = self.get_mut(idx)?;
        let replacement = match expr.take() {
            Expression::Sum(summands) => {
                Expression::Sum(summands.into_iter()
                    .flat_map(|e| {
                        match e {
                            Expression::Sum(more_summands) => more_summands,
                            e => vec![e],
                        }
                    })
                    .collect())
            }
            Expression::Product(prods) => {
                Expression::Product(prods.into_iter()
                    .flat_map(|e| {
                        match e {
                            Expression::Product(more_prods) => more_prods,
                            e => vec![e],
                        }
                    })
                    .collect())
            }
            e => e,
        };
        *expr = replacement;
        Ok(())
    }
}

impl Expression {
    pub fn from_str(expr: &str) -> Result<Self, AlgebraDSLError> {
        parser::parse_expr(expr).map_err(AlgebraDSLError::Parse)
    }
    pub fn take(&mut self) -> Self {
        mem::replace(self, Expression::Atom(Atom::Natural(0)))
    }
    pub fn inflate_addition(self, expr: Expression) -> Self {
        match self {
            Expression::Sum(mut summands) => {
                summands.push(expr);
                Expression::Sum(summands)
            }
            not_sum => Expression::Sum(vec![not_sum, expr]),
        }
    }
    pub fn inflate_multiplication(self, expr: Expression) -> Self {
        match self {
            Expression::Product(mut args) => {
                args.push(expr);
                Expression::Product(args)
            }
            not_prod => Expression::Product(vec![not_prod, expr]),
        }
    }
}

impl Indexable for Expression {
    fn get(&self, index: TreeIdxRef) -> Result<&Expression, AlgebraDSLError> {
        println!("index {:?} in {:#?}", index, self);
        match index.first() {
            None => Ok(self),
            Some(first) => {
                let rest = index.rest();
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
                    &Expression::LimitOp(_, ref sub, _, _) if first == 0 => {
                        sub.as_ref()
                            .ok_or(AlgebraDSLError::InvalidIdx)
                            .and_then(|s| s.get(rest))
                    }
                    &Expression::LimitOp(_, _, ref sup, _) if first == 1 => {
                        sup.as_ref()
                            .ok_or(AlgebraDSLError::InvalidIdx)
                            .and_then(|s| s.get(rest))
                    }
                    &Expression::LimitOp(_, _, _, ref exp) if first == 2 => exp.get(rest),
                    _ => Err(AlgebraDSLError::InvalidIdx),
                }
            }
        }
    }
    fn get_mut(&mut self, index: TreeIdxRef) -> Result<&mut Expression, AlgebraDSLError> {
        match index.first() {
            None => Ok(self),
            Some(first) => {
                let rest = index.rest();
                match self {
                    &mut Expression::Negation(ref mut e) if first == 0 => e.get_mut(rest),
                    &mut Expression::Sum(ref mut e) if first < e.len() => e[first].get_mut(rest),
                    &mut Expression::Product(ref mut e) if first < e.len() => {
                        e[first].get_mut(rest)
                    }
                    &mut Expression::Division(ref mut top, _) if first == 0 => top.get_mut(rest),
                    &mut Expression::Division(_, ref mut bot) if first == 1 => bot.get_mut(rest),
                    &mut Expression::Power(ref mut base, _) if first == 0 => base.get_mut(rest),
                    &mut Expression::Power(_, ref mut power) if first == 1 => power.get_mut(rest),
                    &mut Expression::Subscript(ref mut base, _) if first == 0 => base.get_mut(rest),
                    &mut Expression::Subscript(_, ref mut pow) if first == 1 => pow.get_mut(rest),
                    &mut Expression::Application(ref mut func, _) if first == 0 => {
                        func.get_mut(rest)
                    }
                    &mut Expression::Application(_, ref mut arg) if first == 1 => arg.get_mut(rest),
                    &mut Expression::LimitOp(_, ref mut sub, _, _) if first == 0 => {
                        sub.as_mut()
                            .ok_or(AlgebraDSLError::InvalidIdx)
                            .and_then(|s| s.get_mut(rest))
                    }
                    &mut Expression::LimitOp(_, _, ref mut sup, _) if first == 1 => {
                        sup.as_mut()
                            .ok_or(AlgebraDSLError::InvalidIdx)
                            .and_then(|s| s.get_mut(rest))
                    }
                    &mut Expression::LimitOp(_, _, _, ref mut exp) if first == 2 => {
                        exp.get_mut(rest)
                    }
                    _ => Err(AlgebraDSLError::InvalidIdx),
                }
            }
        }
    }
}

#[derive(Debug,PartialEq, Eq)]
pub enum AlgebraDSLError {
    Parse(ParseError),
    InvalidIdx,
    IllFormattedIndex,
    IllFormattedCommand,
    InvalidDelete,
    InvalidSiblingIndices,
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
    /// Operator, Sub, Super, Operand
    LimitOp(OperatorSymbol, Option<Box<Expression>>, Option<Box<Expression>>, Box<Expression>),
    Application(Box<Expression>, Box<Expression>),
    /// An indivisible unit, like a variable or numeric literal
    Atom(Atom),
}

impl fmt::Display for Equation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">")?;
        write!(f, "<mrow mathTreeNode=\"0\">")?;
        fmt_as_math_ml(&self.left, f, "0,0")?;
        write!(f, "<mo>=</mo>")?;
        fmt_as_math_ml(&self.right, f, "0,1")?;
        write!(f, "</mrow>")?;
        write!(f, "</math>")
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">")?;
        fmt_as_math_ml(&self, f, "0")?;
        write!(f, "</math>")
    }
}

fn fmt_as_math_ml(expr: &Expression,
                  f: &mut fmt::Formatter,
                  prev_index: &str)
                  -> Result<(), fmt::Error> {
    match expr {
        &Expression::Atom(atom) => {
            write!(f, "<mrow mathTreeNode=\"{}\">{}</mrow>", prev_index, atom)
        }
        &Expression::Power(ref b, ref p) => {
            let mut base_string = String::from(prev_index);
            write!(f, "<mrow mathTreeNode=\"{}\"><msup>", base_string)?;
            base_string.push_str(",0");
            fmt_as_math_ml(b, f, &base_string)?;
            let mut base_string = String::from(prev_index);
            base_string.push_str(",1");
            fmt_as_math_ml(p, f, &base_string)?;
            write!(f, "</msup></mrow>")
        }
        &Expression::Negation(ref n) => {
            let mut base_string = String::from(prev_index);
            write!(f, "<mrow mathTreeNode=\"{}\"><mo>-</mo>", base_string)?;
            base_string.push_str(",0");
            fmt_as_math_ml(n, f, &base_string)?;
            write!(f, "</mrow>")
        }
        &Expression::Division(ref n, ref d) => {
            let mut base_string = String::from(prev_index);
            write!(f, "<mrow mathTreeNode=\"{}\"><mfrac>", base_string)?;
            base_string.push_str(",0");
            fmt_as_math_ml(n, f, &base_string)?;
            let mut base_string = String::from(prev_index);
            base_string.push_str(",1");
            fmt_as_math_ml(d, f, &base_string)?;
            write!(f, "</mfrac></mrow>")
        }
        &Expression::Subscript(ref e, ref s) => {
            let mut base_string = String::from(prev_index);
            write!(f, "<mrow mathTreeNode=\"{}\"><msub>", base_string)?;
            base_string.push_str(",0");
            fmt_as_math_ml(e, f, &base_string)?;
            let mut base_string = String::from(prev_index);
            base_string.push_str(",1");
            fmt_as_math_ml(s, f, &base_string)?;
            write!(f, "</msub></mrow>")
        }
        &Expression::Sum(ref s) => {
            write!(f, "<mrow mathTreeNode=\"{}\">", prev_index)?;
            let len = s.len();
            let iter = s.iter().enumerate();
            for (i, e) in iter {
                let mut base_string = String::from(prev_index);
                base_string.push_str(",");
                base_string.push_str(&i.to_string());
                if i == len - 1 {
                    fmt_as_math_ml(e, f, &base_string)?;
                } else {
                    fmt_as_math_ml(e, f, &base_string)?;
                    write!(f, "<mo>+</mo>")?;
                }
            }
            write!(f, "</mrow>")
        }
        &Expression::Product(ref s) => {
            write!(f, "<mrow mathTreeNode=\"{}\">", prev_index)?;
            let len = s.len();
            let iter = s.iter().enumerate();
            for (i, e) in iter {
                let mut base_string = String::from(prev_index);
                base_string.push_str(",");
                base_string.push_str(&i.to_string());
                if i == len - 1 {
                    fmt_as_math_ml(e, f, &base_string)?;
                } else {
                    fmt_as_math_ml(e, f, &base_string)?;
                    write!(f, "<mo>&#8290;</mo>")?;
                }
            }
            write!(f, "</mrow>")
        }
        &Expression::Application(ref func, ref arg) => {
            let mut base_string = String::from(prev_index);
            write!(f, "<mrow mathTreeNode=\"{}\">", base_string)?;
            base_string.push_str(",0");
            fmt_as_math_ml(func, f, &base_string)?;
            let mut base_string = String::from(prev_index);
            base_string.push_str(",1");
            write!(f, "<mo>(</mo>")?;
            fmt_as_math_ml(arg, f, &base_string)?;
            write!(f, "<mo>)</mo></mrow>")
        }
        &Expression::LimitOp(ref op, ref sub, ref sup, ref expr) => {
            let mut base_string = String::from(prev_index);
            let orig_len = base_string.len();
            write!(f, "<mrow mathTreeNode=\"{}\"><munderover>", base_string)?;
            write!(f, "<mo>{}</mo>", op.as_math_ml())?;

            base_string.push_str(",0");
            if let &Some(ref s) = sub {
                fmt_as_math_ml(&*s, f, &base_string)?;
            }
            base_string.truncate(orig_len);

            base_string.push_str(",1");
            if let &Some(ref s) = sup {
                fmt_as_math_ml(&*s, f, &base_string)?;
            }
            base_string.truncate(orig_len);

            write!(f, "</munderover>")?;
            base_string.push_str(",2");
            fmt_as_math_ml(expr, f, &base_string)?;
            write!(f, "</mrow>")
        }
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
            &Atom::PlainVariable(c) => write!(f, "<mi>{}</mi>", c),
            &Atom::Natural(n) => write!(f, "<mn>{}</mn>", n),
            &Atom::Floating(r) => write!(f, "<mn>{}</mn>", r),
            &Atom::Symbol(sym) => write!(f, "<mo>{}</mo>", sym.as_math_ml()),
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
    pm,
}

impl OperatorSymbol {
    fn as_math_ml(&self) -> &'static str {
        match self {
            &OperatorSymbol::int => "&int;",
            &OperatorSymbol::oint => "&oint;",
            &OperatorSymbol::sum => "&sum;",
            &OperatorSymbol::prod => "&prod;",
            &OperatorSymbol::arccos => "&arccos;",
            &OperatorSymbol::cos => "&cos;",
            &OperatorSymbol::csc => "&csc;",
            &OperatorSymbol::exp => "&exp;",
            &OperatorSymbol::limsup => "&limsup;",
            &OperatorSymbol::min => "&min;",
            &OperatorSymbol::sinh => "&sinh;",
            &OperatorSymbol::arcsin => "&arcsin;",
            &OperatorSymbol::cosh => "&cosh;",
            &OperatorSymbol::gcd => "&gcd;",
            &OperatorSymbol::lg => "&lg;",
            &OperatorSymbol::ln => "&ln;",
            &OperatorSymbol::sup => "&sup;",
            &OperatorSymbol::arctan => "&arctan;",
            &OperatorSymbol::cot => "&cot;",
            &OperatorSymbol::det => "&det;",
            &OperatorSymbol::lim => "&lim;",
            &OperatorSymbol::log => "&log;",
            &OperatorSymbol::sec => "&sec;",
            &OperatorSymbol::tan => "&tan;",
            &OperatorSymbol::coth => "&coth;",
            &OperatorSymbol::inf => "&inf;",
            &OperatorSymbol::liminf => "&liminf;",
            &OperatorSymbol::max => "&max;",
            &OperatorSymbol::sin => "&sin;",
            &OperatorSymbol::tanh => "&tanh;",
            &OperatorSymbol::pm => "&pm;",
        }
    }
}

impl StandaloneSymbol {
    fn as_math_ml(&self) -> &'static str {
        match self {
            &StandaloneSymbol::alpha => "&alpha;",
            &StandaloneSymbol::beta => "&beta;",
            &StandaloneSymbol::gamma => "&gamma;",
            &StandaloneSymbol::delta => "&delta;",
            &StandaloneSymbol::epsilon => "&epsilon;",
            &StandaloneSymbol::zeta => "&zeta;",
            &StandaloneSymbol::eta => "&eta;",
            &StandaloneSymbol::theta => "&theta;",
            &StandaloneSymbol::iota => "&iota;",
            &StandaloneSymbol::kappa => "&kappa;",
            &StandaloneSymbol::lambda => "&lambda;",
            &StandaloneSymbol::mu => "&mu;",
            &StandaloneSymbol::nu => "&nu;",
            &StandaloneSymbol::omicron => "&omicron;",
            &StandaloneSymbol::pi => "&pi;",
            &StandaloneSymbol::rho => "&rho;",
            &StandaloneSymbol::sigma => "&sigma;",
            &StandaloneSymbol::tau => "&tau;",
            &StandaloneSymbol::upsilon => "&upsilon;",
            &StandaloneSymbol::phi => "&phi;",
            &StandaloneSymbol::chi => "&chi;",
            &StandaloneSymbol::psi => "&psi;",
            &StandaloneSymbol::omega => "&omega;",
            &StandaloneSymbol::Gamma => "&Gamma;",
            &StandaloneSymbol::Delta => "&Delta;",
            &StandaloneSymbol::Theta => "&Theta;",
            &StandaloneSymbol::Lambda => "&Lambda;",
            &StandaloneSymbol::Pi => "&Pi;",
            &StandaloneSymbol::Sigma => "&Sigma;",
            &StandaloneSymbol::Upsilon => "&Upsilon;",
            &StandaloneSymbol::Phi => "&Phi;",
            &StandaloneSymbol::Psi => "&Psi;",
            &StandaloneSymbol::Omega => "&Omega;",
            &StandaloneSymbol::partial => "&partial;",
        }
    }
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
            "pm" => Some(Symbol::Operator(OperatorSymbol::pm)),
            _ => None,
        }
    }
    fn as_math_ml(&self) -> &'static str {
        match self {
            &Symbol::Standalone(ref sym) => sym.as_math_ml(),
            &Symbol::Operator(ref sym) => sym.as_math_ml(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Expression as Ex;

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
        let expr = Ex::Power(box Ex::Atom(Atom::PlainVariable('x')),
                             box Ex::Atom(Atom::PlainVariable('y')));
        let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow \
                        mathTreeNode=\"0\"><msup><mrow \
                        mathTreeNode=\"0,0\"><mi>x</mi></mrow><mrow \
                        mathTreeNode=\"0,1\"><mi>y</mi></mrow></msup></mrow></math>";
        let test = format!("{}", expr);
        assert_expected_eq_actual!(expected, test);
    }

    #[test]
    fn index_power() {
        let expr = Ex::Power(box Ex::Atom(Atom::PlainVariable('x')),
                             box Ex::Atom(Atom::PlainVariable('y')));
        let r = expr.get(TreeIdx(vec![1]).as_ref());
        let rr = &Ex::Atom(Atom::PlainVariable('y'));
        let e = Ok(rr);
        assert_expected_eq_actual!(e, r);
    }

    #[test]
    fn format_frac() {
        let expr = Ex::Division(box Ex::Atom(Atom::PlainVariable('x')),
                                box Ex::Atom(Atom::PlainVariable('y')));
        let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow \
                        mathTreeNode=\"0\"><mfrac><mrow \
                        mathTreeNode=\"0,0\"><mi>x</mi></mrow><mrow \
                        mathTreeNode=\"0,1\"><mi>y</mi></mrow></mfrac></mrow></math>";
        let test = format!("{}", expr);
        assert_expected_eq_actual!(expected, test);
    }

    #[test]
    fn format_negation() {
        let expr = Ex::Negation(box Ex::Atom(Atom::PlainVariable('x')));
        let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow \
                        mathTreeNode=\"0\"><mo>-</mo><mrow \
                        mathTreeNode=\"0,0\"><mi>x</mi></mrow></mrow></math>";
        let test = format!("{}", expr);
        assert_expected_eq_actual!(expected, test);
    }

    #[test]
    fn format_sub() {
        let expr = Ex::Subscript(box Ex::Atom(Atom::PlainVariable('x')),
                                 box Ex::Atom(Atom::PlainVariable('y')));
        let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow \
                        mathTreeNode=\"0\"><msub><mrow \
                        mathTreeNode=\"0,0\"><mi>x</mi></mrow><mrow \
                        mathTreeNode=\"0,1\"><mi>y</mi></mrow></msub></mrow></math>";
        let test = format!("{}", expr);
        assert_expected_eq_actual!(expected, test);
    }

    #[test]
    fn format_add() {
        let expr = Ex::Sum(vec![Ex::Atom(Atom::PlainVariable('x')),
                                        Ex::Atom(Atom::PlainVariable('y')),
                                        Ex::Atom(Atom::PlainVariable('z'))]);
        let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow \
                        mathTreeNode=\"0\"><mrow \
                        mathTreeNode=\"0,0\"><mi>x</mi></mrow><mo>+</mo><mrow \
                        mathTreeNode=\"0,1\"><mi>y</mi></mrow><mo>+</mo><mrow \
                        mathTreeNode=\"0,2\"><mi>z</mi></mrow></mrow></math>";
        let test = format!("{}", expr);
        assert_expected_eq_actual!(expected, test);
    }

    #[test]
    fn format_prod() {
        let expr = Ex::Product(vec![Ex::Atom(Atom::PlainVariable('x')),
                                            Ex::Atom(Atom::PlainVariable('y')),
                                            Ex::Atom(Atom::PlainVariable('z'))]);
        let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow \
                        mathTreeNode=\"0\"><mrow \
                        mathTreeNode=\"0,0\"><mi>x</mi></mrow><mo>&#8290;</mo><mrow \
                        mathTreeNode=\"0,1\"><mi>y</mi></mrow><mo>&#8290;</mo><mrow \
                        mathTreeNode=\"0,2\"><mi>z</mi></mrow></mrow></math>";
        let test = format!("{}", expr);
        assert_expected_eq_actual!(expected, test);
    }

    #[test]
    fn replace_siblings() {
        let mut before = Ex::Product(vec![Ex::Atom(Atom::Natural(3)),
                                          Ex::Atom(Atom::Natural(4)),
                                          Ex::Atom(Atom::PlainVariable('x')),
                                          Ex::Atom(Atom::Natural(7))]);
        let index_strings = vec!["#(0,0)", "#(0,1)", "#(0,3)"];
        let after =
            Ex::Product(vec![Ex::Atom(Atom::Natural(84)), Ex::Atom(Atom::PlainVariable('x'))]);
        let indices: Vec<_> =
            index_strings.into_iter().map(TreeIdx::from_str).map(Result::unwrap).collect();
        println!("{:#?}", indices);
        let siblings = SiblingIndices::from_indices(indices.as_slice()).unwrap();
        let replacement = Ex::Atom(Atom::Natural(84));
        before.replace_siblings(siblings, replacement).unwrap();
        assert_expected_eq_actual!(after, before);
    }

    #[test]
    fn delete_expr() {
        let mut before = Ex::Product(vec![Ex::Atom(Atom::Natural(3)),
                                          Ex::Atom(Atom::Natural(4)),
                                          Ex::Atom(Atom::PlainVariable('x')),
                                          Ex::Atom(Atom::Natural(7))]);
        let idx = TreeIdx::from_str("#(0,1)").unwrap();
        let after = Ex::Product(vec![Ex::Atom(Atom::Natural(3)),
                                     Ex::Atom(Atom::PlainVariable('x')),
                                     Ex::Atom(Atom::Natural(7))]);
        before.delete(&idx).unwrap();
        assert_expected_eq_actual!(after, before);
    }

}
