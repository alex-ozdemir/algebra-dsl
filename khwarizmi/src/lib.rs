#![feature(box_syntax, box_patterns, slice_patterns, advanced_slice_patterns, try_from)]
#![allow(dead_code)]
#[macro_use]
extern crate nom;
mod parser;

use parser::ParseError;
use std::convert::TryFrom;
use std::str::FromStr;
use std::string::String;
use std::{fmt, mem};

const NULL_EXPRESSION: Expression = Expression::Atom(Atom::Natural(0));
const NULL_IDX: &'static [TreeInt] = &[];
const UNREACH: &'static str = "An option/result that was expected to be Some/Ok was not.\n\
                               This is a bug!";

#[derive(PartialEq, Debug, Clone)]
pub struct Equation {
    left: Expression,
    right: Expression,
}

impl TreeIdx {
    pub fn from_str(s: &str) -> Result<Self, AlgebraDSLError> {
        if !(s.starts_with("#(mtn:") && s.ends_with(")")) {
            Err(AlgebraDSLError::IllFormattedIndex)
        } else {
            let s = &s[6..s.len() - 1];
            let mut idxs = s.split(',')
                .map(|d| usize::from_str(d).map_err(|_| AlgebraDSLError::IllFormattedIndex));
            // Pop the first index, because every expression/equation is in the trivial 0 idx
            assert!(Some(Ok(0)) == idxs.next());
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
    pub fn minus_to_both(&mut self, expr: Expression) {
        self.plus_to_both(Expression::Negation(box expr))
    }
    pub fn times_to_both(&mut self, expr: Expression) {
        self.left = mem::replace(&mut self.left, NULL_EXPRESSION)
            .inflate_multiplication(expr.clone());
        self.right = mem::replace(&mut self.right, NULL_EXPRESSION).inflate_multiplication(expr);
    }
    pub fn div_to_both(&mut self, expr: Expression) {
        self.left = mem::replace(&mut self.left, NULL_EXPRESSION).inflate_division(expr.clone());
        self.right = mem::replace(&mut self.right, NULL_EXPRESSION).inflate_division(expr);
    }
    pub fn simplify_constants(self) -> Self {
        Equation {
            left: self.left.simplify_constants(),
            right: self.right.simplify_constants(),
        }
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
}

pub struct SiblingIndices<'a> {
    parent_idx: TreeIdxRef<'a>,
    // This list *must* be sorted
    children: Vec<TreeInt>,
}

impl<'a> SiblingIndices<'a> {
    /// If all the input indices share a parent, constructs a sibling index representing all of
    /// them
    pub fn from_indices(indices: &'a [TreeIdx]) -> Result<SiblingIndices<'a>, AlgebraDSLError> {
        if indices.len() < 1 ||
           indices.iter().map(|i| i.as_ref().len()).min().expect(UNREACH) == 0 {
            return Err(AlgebraDSLError::InvalidSiblingIndices);
        }
        let first_parent = indices[0].as_ref().parent().expect(UNREACH);
        if indices.iter().all(|idx| idx.as_ref().parent() == Some(first_parent)) {
            let mut vec: Vec<_> =
                indices.iter().map(|i| i.as_ref().last().expect(UNREACH)).collect();
            vec.sort();
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
    fn delete(&mut self, indices: SiblingIndices) -> Result<(), AlgebraDSLError> {
        match self.get_mut(indices.parent_idx)? {
            &mut Expression::Sum(ref mut args) |
            &mut Expression::Product(ref mut args) => {
                if args.len() == indices.children.len() {
                    return Err(AlgebraDSLError::InvalidDelete);
                }

                for idx in indices.children.iter().rev() {
                    args.remove(*idx);
                }
                Ok(())
            }
            _ => Err(AlgebraDSLError::InvalidSiblingIndices),
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
    // TODO: Defaults
    pub fn inflate_division(self, expr: Expression) -> Self {
        Expression::Division(box self, box expr)
    }
    pub fn simplify_constants(self) -> Self {
        use Expression as Ex;
        match self {
            Ex::Negation(box expr) => {
                match expr.simplify_constants() {
                    Ex::Atom(Atom::Natural(n)) => Ex::Atom(Atom::Natural(-n)),
                    Ex::Atom(Atom::Floating(n)) => Ex::Atom(Atom::Floating(-n)),
                    e => Ex::Negation(box e),
                }
            }
            Ex::Sum(exprs) => {
                let mut f_acc = 0.0;
                let mut n_acc = 0;
                let mut new_exprs = vec![];
                for ex in exprs.into_iter().map(Ex::simplify_constants) {
                    match ex {
                        Ex::Atom(Atom::Natural(n)) => n_acc += n,
                        Ex::Atom(Atom::Floating(f)) => f_acc += f,
                        e => new_exprs.push(e),
                    }
                }
                if f_acc != 0. {
                    new_exprs.push(Ex::Atom(Atom::Floating(f_acc + n_acc as f64)));
                } else {
                    if n_acc != 0 {
                        new_exprs.push(Ex::Atom(Atom::Natural(n_acc)));
                    }
                }
                if new_exprs.len() == 1 {
                    new_exprs.pop().expect(UNREACH)
                } else {
                    Ex::Sum(new_exprs)
                }
            }
            Ex::Product(exprs) => {
                let mut f_acc = 1.0;
                let mut n_acc = 1;
                let mut new_exprs = vec![];
                for ex in exprs.into_iter().map(Ex::simplify_constants) {
                    match ex {
                        Ex::Atom(Atom::Natural(n)) => n_acc *= n,
                        Ex::Atom(Atom::Floating(f)) => f_acc *= f,
                        e => new_exprs.push(e),
                    }
                }
                if f_acc != 1. {
                    new_exprs.insert(0, Ex::Atom(Atom::Floating(f_acc * n_acc as f64)));
                } else if n_acc != 1 {
                    new_exprs.insert(0, Ex::Atom(Atom::Natural(n_acc)));
                }
                if new_exprs.len() == 1 {
                    new_exprs.pop().expect(UNREACH)
                } else {
                    Ex::Product(new_exprs)
                }
            }
            Ex::Division(box top, box bottom) => {
                let t = top.simplify_constants();
                let b = bottom.simplify_constants();
                match (t, b) {
                    (Ex::Atom(Atom::Floating(f)), Ex::Atom(Atom::Floating(f2))) => {
                        Ex::Atom(Atom::Floating(f / f2))
                    }
                    (Ex::Atom(Atom::Natural(n)), Ex::Atom(Atom::Floating(f))) => {
                        Ex::Atom(Atom::Floating(n as f64 / f))
                    }
                    (Ex::Atom(Atom::Floating(f)), Ex::Atom(Atom::Natural(n))) => {
                        Ex::Atom(Atom::Floating(f / n as f64))
                    }
                    (Ex::Atom(Atom::Natural(n)), Ex::Atom(Atom::Natural(n2))) => {
                        if n % n2 == 0 {
                            Ex::Atom(Atom::Natural(n / n2))
                        } else {
                            Ex::Atom(Atom::Floating(n as f64 + n2 as f64))
                        }
                    }
                    (e1, e2) => Ex::Division(box e1, box e2),

                }
            }
            Ex::Power(box base, box exp) => {
                let b = base.simplify_constants();
                let e = exp.simplify_constants();
                match (b, e) {
                    (Ex::Atom(Atom::Floating(f)), Ex::Atom(Atom::Floating(f2))) => {
                        Ex::Atom(Atom::Floating(f.powf(f2)))
                    }
                    (Ex::Atom(Atom::Natural(n)), Ex::Atom(Atom::Floating(f))) => {
                        Ex::Atom(Atom::Floating((n as f64).powf(f)))
                    }
                    (Ex::Atom(Atom::Floating(f)), Ex::Atom(Atom::Natural(n))) => {
                        Ex::Atom(Atom::Floating(f.powf(n as f64)))
                    }
                    (Ex::Atom(Atom::Natural(n)), Ex::Atom(Atom::Natural(n2))) => {
                        let p: Result<u32, _> = TryFrom::try_from(n2);
                        Ex::Atom(p.map(|u| n.pow(u))
                            .map(Atom::Natural)
                            .unwrap_or(Atom::Floating((n as f64).powf(n2 as f64))))
                    }
                    (e1, e2) => Ex::Power(box e1, box e2),

                }
            }
            Ex::LimitOp(sym, sub, sup, op) => {
                Ex::LimitOp(sym, sub.map(|box x| box x.simplify_constants()),
                                 sup.map(|box x| box x.simplify_constants()),
                                  box op.simplify_constants())
            }
            Ex::Application(func, arg) => {
                Ex::Application(func, box arg.simplify_constants())
            }
            e => e
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
    MapExpression,
    NeedsExpression,
    UnrecognizedCmd,
    InternalError,
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
        fmt_as_math_ml(&self.left, f, "0,0", (&self.left,false,true))?;
        write!(f, "<mo>=</mo>")?;
        fmt_as_math_ml(&self.right, f, "0,1", (&self.right,false,true))?;
        write!(f, "</mrow>")?;
        write!(f, "</math>")
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">")?;
        fmt_as_math_ml(&self, f, "0", (&self,false,true))?;
        write!(f, "</math>")
    }
}

pub struct LatexWriter(String);

impl LatexWriter {
    pub fn new() -> LatexWriter {
        LatexWriter("\\begin{align*}\n".to_string())
    }

    pub fn add_math(&mut self, e: &EqOrExpr) -> fmt::Result {
        use std::fmt::Write;

        struct DisplaysAsLatex<'a>(&'a EqOrExpr);
        impl<'a> fmt::Display for DisplaysAsLatex<'a> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "  ")?;
                match self.0 {
                    &EqOrExpr::Eq(ref eq) => {
                        fmt_as_latex(&eq.left, f, (&eq.left,false,true))?;
                        write!(f, " &= ")?;
                        fmt_as_latex(&eq.right, f, (&eq.right,false,true))?;
                    }
                    &EqOrExpr::Ex(ref ex) => {
                        fmt_as_latex(&ex, f, (&ex,false,true))?;
                    }
                }
                write!(f, " \\\\\n")
            }
        }
        write!(self.0, "{}", DisplaysAsLatex(e))
    }

    pub fn finish_str(mut self) -> Result<String, fmt::Error> {
        use std::fmt::Write;

        write!(self.0, "\\end{{align*}}")?;
        Ok(self.0)
    }
}

//alt is usually for right expressions, when they need different rules
//Outputs whether expr1 captures expr2 with parenthesis
fn capture(expr1: &Expression, expr2: &Expression, alt: bool, top: bool) -> bool {
    if top { //Did we just come from the top level call?
        return false;
    }
    if !alt { //Left capture
        match expr1 {
            &Expression::Sum(_) => {
                match expr2 {
                    _ => false
                }
            }
            &Expression::Product(_) => {
                match expr2 {
                    &Expression::Sum(_) => true,
                    &Expression::Negation(_) => true,
                    _ => false
                }
            }
            &Expression::Negation(_) => {
                match expr2 {
                    &Expression::Sum(_) => true,
                    _ => false
                }
            }
            &Expression::Application(_,_) => {
                match expr2 {
                    _ => false
                }
            }
            &Expression::LimitOp(_,_,_,_) => {
                match expr2 {
                    _ => false
                }
            }
            &Expression::Division(_,_) => {
                match expr2 {
                    _ => false
                }
            }
            &Expression::Power(_,_) => {
                match expr2 {
                    &Expression::Sum(_) => true,
                    &Expression::Product(_) => true,
                    &Expression::Power(_,_) => true,
                    &Expression::Negation(_) => true,
                    &Expression::Division(_,_) => true,
                    _ => false
                }
            }
            &Expression::Subscript(_,_) => {
                match expr2 {
                    &Expression::Atom(_) => false,
                    _ => true
                }
            }
            &Expression::Atom(_) => {
                false
            }
        }
    } else { //right capture
        match expr1 {
            &Expression::Sum(_) => {
                match expr2 {
                    _ => false
                }
            }
            &Expression::Product(_) => {
                match expr2 {
                    _ => false
                }
            }
            &Expression::Negation(_) => {
                match expr2 {
                    _ => false
                }
            }
            &Expression::Application(_,_) => {
                match expr2 {
                    _ => true
                }
            }
            &Expression::LimitOp(_,_,_,_) => {
                match expr2 {
                    _ => false
                }
            }
            &Expression::Division(_,_) => {
                match expr2 {
                    _ => false
                }
            }
            &Expression::Power(_,_) => {
                match expr2 {
                    _ => false
                }
            }
            &Expression::Subscript(_,_) => {
                match expr2 {
                    _ => false
                }
            }
            &Expression::Atom(_) => {
                false
            }
        }
    }
}

fn fmt_as_math_ml(expr: &Expression,
                  f: &mut fmt::Formatter,
                  prev_index: &str,
                  prev_precedence: (&Expression, bool,bool))
                  -> Result<(), fmt::Error> {
    write!(f, "<mrow mathTreeNode=\"{}\">", prev_index)?;
    if capture(prev_precedence.0, expr, prev_precedence.1, prev_precedence.2) {
        write!(f,"<mo form=\"prefix\">(</mo>")?;
    }
    match expr {
        &Expression::Atom(atom) => {
            match prev_precedence.0 {
                &Expression::Sum(_) => {
                    match atom {
                        Atom::Natural(n) => {
                            if n < 0 {
                                write!(f, "<mo>-</mo><mn>{}</mn>", -n)?
                            } else {
                                write!(f, "{}", atom)?
                            }
                        }
                        Atom::Floating(n) => {
                            if n < 0.0 {
                                write!(f, "<mo>-</mo><mn>{}</mn>", -n)?
                            } else {
                                write!(f, "{}", atom)?
                            }
                        }
                        _ => write!(f, "{}", atom)?
                    }
                }
                _ => write!(f, "{}", atom)?
            };
        }
        &Expression::Power(ref b, ref p) => {
            let mut base_string = String::from(prev_index);
            write!(f, "<msup>")?;
            base_string.push_str(",0");
            fmt_as_math_ml(b, f, &base_string, (expr,false,false))?;
            let mut base_string = String::from(prev_index);
            base_string.push_str(",1");
            fmt_as_math_ml(p, f, &base_string, (expr,true,false))?;
            write!(f, "</msup>")?;
        }
        &Expression::Negation(ref n) => {
            let mut base_string = String::from(prev_index);
            write!(f, "<mo>-</mo>")?;
            base_string.push_str(",0");
            fmt_as_math_ml(n, f, &base_string, (expr,false,false))?;
        }
        &Expression::Division(ref n, ref d) => {
            let mut base_string = String::from(prev_index);
            write!(f, "<mfrac>")?;
            base_string.push_str(",0");
            fmt_as_math_ml(n, f, &base_string, (expr,false,false))?;
            let mut base_string = String::from(prev_index);
            base_string.push_str(",1");
            fmt_as_math_ml(d, f, &base_string, (expr,false,false))?;
            write!(f, "</mfrac>")?;
        }
        &Expression::Subscript(ref e, ref s) => {
            let mut base_string = String::from(prev_index);
            write!(f, "<msub>")?;
            base_string.push_str(",0");
            fmt_as_math_ml(e, f, &base_string, (expr,false,false))?;
            let mut base_string = String::from(prev_index);
            base_string.push_str(",1");
            fmt_as_math_ml(s, f, &base_string, (expr,true,false))?;
            write!(f, "</msub>")?;
        }
        &Expression::Sum(ref s) => {
            let len = s.len();
            let iter = s.iter().enumerate();
            for (i, e) in iter {
                let mut base_string = String::from(prev_index);
                base_string.push_str(",");
                base_string.push_str(&i.to_string());
                if i == len - 1 {
                    fmt_as_math_ml(e, f, &base_string, (expr,false,false))?;
                } else {
                    fmt_as_math_ml(e, f, &base_string, (expr,false,false))?;
                    let e_next = &s[i+1];
                    match e_next {
                        &Expression::Negation(_) => {},
                        &Expression::Atom(Atom::Floating(n)) => {
                            if n >= 0.0 {
                                write!(f, "<mo>+</mo>")?;
                            }
                        }
                        &Expression::Atom(Atom::Natural(n)) => {
                            if n >= 0 {
                                write!(f, "<mo>+</mo>")?;
                            }
                        }
                        _ => write!(f, "<mo>+</mo>")?
                    };
                }
            }
        }
        &Expression::Product(ref s) => {
            let len = s.len();
            let iter = s.iter().enumerate();
            for (i, e) in iter {
                let mut base_string = String::from(prev_index);
                base_string.push_str(",");
                base_string.push_str(&i.to_string());
                if i == len - 1 {
                    fmt_as_math_ml(e, f, &base_string, (expr,false,false))?;
                } else {
                    fmt_as_math_ml(e, f, &base_string, (expr,false,false))?;
                    let e_next = &s[i+1];
                    match e_next {
                        &Expression::Atom(Atom::Floating(_)) => write!(f, "<mo>&#x022C5;</mo>")?,
                        &Expression::Atom(Atom::Natural(_)) => write!(f, "<mo>&#x022C5;</mo>")?,
                        _ => write!(f, "<mo>&#8290;</mo>")?
                    };
                }
            }
        }
        &Expression::Application(ref func, ref arg) => {
            let mut base_string = String::from(prev_index);
            base_string.push_str(",0");
            fmt_as_math_ml(func, f, &base_string, (expr,false,false))?;
            let mut base_string = String::from(prev_index);
            base_string.push_str(",1");
            fmt_as_math_ml(arg, f, &base_string, (expr,true,false))?;
        }
        &Expression::LimitOp(ref op, None, None, ref expr) => {
            let mut base_string = String::from(prev_index);
            write!(f, "{}", op.as_math_ml())?;

            base_string.push_str(",2");
            fmt_as_math_ml(expr, f, &base_string, (expr,false,false))?;
        }
        &Expression::LimitOp(ref op, Some(ref sub), None, ref expr) => {
            let mut base_string = String::from(prev_index);
            let orig_len = base_string.len();
            write!(f, "<munder>")?;
            write!(f, "{}", op.as_math_ml())?;

            base_string.push_str(",0");
            fmt_as_math_ml(sub, f, &base_string, (expr,false,false))?;
            base_string.truncate(orig_len);

            write!(f, "</munder>")?;
            base_string.push_str(",2");
            fmt_as_math_ml(expr, f, &base_string, (expr,false,false))?;
        }
        &Expression::LimitOp(ref op, None, Some(ref sup), ref expr) => {
            let mut base_string = String::from(prev_index);
            let orig_len = base_string.len();
            write!(f, "<mover>")?;
            write!(f, "{}", op.as_math_ml())?;

            base_string.push_str(",1");
            fmt_as_math_ml(sup, f, &base_string, (expr,false,false))?;
            base_string.truncate(orig_len);

            write!(f, "</mover>")?;
            base_string.push_str(",2");
            fmt_as_math_ml(expr, f, &base_string, (expr,false,false))?;
        }
        &Expression::LimitOp(ref op, Some(ref sub), Some(ref sup), ref expr) => {
            let mut base_string = String::from(prev_index);
            let orig_len = base_string.len();
            write!(f, "<munderover>")?;
            write!(f, "{}", op.as_math_ml())?;

            base_string.push_str(",0");

            fmt_as_math_ml(sub, f, &base_string, (expr,false,false))?;

            base_string.truncate(orig_len);

            base_string.push_str(",1");

            fmt_as_math_ml(sup, f, &base_string, (expr,false,false))?;

            base_string.truncate(orig_len);

            write!(f, "</munderover>")?;
            base_string.push_str(",2");
            fmt_as_math_ml(expr, f, &base_string, (expr,false,false))?;
        }
    }
    if capture(prev_precedence.0, expr, prev_precedence.1, prev_precedence.2) {
        write!(f,"<mo form=\"postfix\">)</mo>")?;
    }
    write!(f, "</mrow>")
}

fn fmt_as_latex(expr: &Expression,
                  f: &mut fmt::Formatter,
                  prev_precedence: (&Expression,bool,bool))
                  -> Result<(), fmt::Error> {
    if capture(prev_precedence.0, expr, prev_precedence.1, prev_precedence.2) {
        write!(f,"\\left(")?;
    }
    match expr {
        &Expression::Atom(atom) => {
            match &atom {
                &Atom::PlainVariable(c) => write!(f, "{}", c)?,
                &Atom::Natural(n) => write!(f, "{}", n)?,
                &Atom::Floating(r) => write!(f, "{}", r)?,
                &Atom::Symbol(sym) => write!(f, "{} ", sym.as_latex())?,
            }
        }
        &Expression::Power(ref b, ref p) => {
            write!(f,"{{")?;
            fmt_as_latex(b, f, (expr,false,false))?;
            write!(f,"}}^{{")?;
            fmt_as_latex(p, f, (expr,true,false))?;
            write!(f,"}}")?;
        }
        &Expression::Negation(ref n) => {
            write!(f,"-")?;
            fmt_as_latex(n, f, (expr,false,false))?;
        }
        &Expression::Division(ref n, ref d) => {
            write!(f,"\\frac{{")?;
            fmt_as_latex(n, f, (expr,false,false))?;
            write!(f,"}}{{")?;
            fmt_as_latex(d, f, (expr,false,false))?;
            write!(f,"}}")?;
        }
        &Expression::Subscript(ref e, ref s) => {
            write!(f,"{{")?;
            fmt_as_latex(e, f, (expr,false,false))?;
            write!(f,"}}_{{")?;
            fmt_as_latex(s, f, (expr,true,false))?;
            write!(f,"}}")?;
        }
        &Expression::Sum(ref s) => {
            let len = s.len();
            let iter = s.iter().enumerate();
            for (i, e) in iter {
                if i == len - 1 {
                    fmt_as_latex(e, f, (expr,false,false))?;
                } else {
                    fmt_as_latex(e, f, (expr,false,false))?;
                    let e_next = &s[i+1];
                    match e_next {
                        &Expression::Negation(_) => {},
                        &Expression::Atom(Atom::Floating(n)) => {
                            if n >= 0.0 {
                                write!(f, "<mo>+</mo>")?;
                            }
                        }
                        &Expression::Atom(Atom::Natural(n)) => {
                            if n >= 0 {
                                write!(f, "<mo>+</mo>")?;
                            }
                        }
                        _ => write!(f, "<mo>+</mo>")?
                    };
                }
            }
        }
        &Expression::Product(ref s) => {
            let len = s.len();
            let iter = s.iter().enumerate();
            for (i, e) in iter {
                if i == len - 1 {
                    fmt_as_latex(e, f, (expr,false,false))?;
                } else {
                    fmt_as_latex(e, f, (expr,false,false))?;
                    let e_next = &s[i+1];
                    match e_next {
                        &Expression::Atom(Atom::Floating(_)) => write!(f,"\\cdot")?,
                        &Expression::Atom(Atom::Natural(_)) =>  write!(f,"\\cdot")?,
                        _ => {}
                    };
                }
            }
        }
        &Expression::Application(ref func, ref arg) => {
            fmt_as_latex(func, f, (expr,false,false))?;
            write!(f," ")?;
            fmt_as_latex(arg, f, (expr,true,false))?;
        }
        &Expression::LimitOp(ref op, None, None, ref expr) => {
            write!(f,"{}", op.as_latex())?;
            fmt_as_latex(expr, f, (expr,false,false))?;
        }
        &Expression::LimitOp(ref op, Some(ref sub), None, ref expr) => {
            write!(f,"{}", op.as_latex())?;
            write!(f,"_{{")?;
            fmt_as_latex(sub, f, (expr,false,false))?;
            write!(f,"}} ")?;
            fmt_as_latex(expr, f, (expr,false,false))?;
        }
        &Expression::LimitOp(ref op, None, Some(ref sup), ref expr) => {
            write!(f,"{}", op.as_latex())?;
            write!(f,"^{{")?;
            fmt_as_latex(sup, f, (expr,false,false))?;
            write!(f,"}} ")?;
            fmt_as_latex(expr, f, (expr,false,false))?;
        }
        &Expression::LimitOp(ref op, Some(ref sub), Some(ref sup), ref expr) => {
            write!(f,"{}", op.as_latex())?;
            write!(f,"_{{")?;
            fmt_as_latex(sub, f, (expr,false,false))?;
            write!(f,"}}^{{")?;
            fmt_as_latex(sup, f, (expr,false,false))?;
            write!(f,"}} ")?;
            fmt_as_latex(expr, f, (expr,false,false))?;
        }
    }
    if capture(prev_precedence.0, expr, prev_precedence.1, prev_precedence.2) {
        write!(f,"\\right)")?;
    }
    Ok(())
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Atom {
    PlainVariable(char),
    Natural(i64),
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
            &OperatorSymbol::int => "<mo>&int;</mo>",
            &OperatorSymbol::oint => "<mo>&oint;</mo>",
            &OperatorSymbol::sum => "<mo>&sum;</mo>",
            &OperatorSymbol::prod => "<mo>&prod;</mo>",
            &OperatorSymbol::arccos => "<mi>arccos</mi>",
            &OperatorSymbol::cos => "<mi>cos</mi>",
            &OperatorSymbol::csc => "<mi>csc</mi>",
            &OperatorSymbol::exp => "<mi>exp</mi>",
            &OperatorSymbol::limsup => "<mi>limsup</mi>",
            &OperatorSymbol::min => "<mi>min</mi>",
            &OperatorSymbol::sinh => "<mi>sinh</mi>",
            &OperatorSymbol::arcsin => "<mi>arcsin</mi>",
            &OperatorSymbol::cosh => "<mi>cosh</mi>",
            &OperatorSymbol::gcd => "<mi>gcd</mi>",
            &OperatorSymbol::lg => "<mi>lg</mi>",
            &OperatorSymbol::ln => "<mi>ln</mi>",
            &OperatorSymbol::sup => "<mi>sup</mi>",
            &OperatorSymbol::arctan => "<mi>arctan</mi>",
            &OperatorSymbol::cot => "<mi>cot</mi>",
            &OperatorSymbol::det => "<mi>det</mi>",
            &OperatorSymbol::lim => "<mi>lim</mi>",
            &OperatorSymbol::log => "<mi>log</mi>",
            &OperatorSymbol::sec => "<mi>sec</mi>",
            &OperatorSymbol::tan => "<mi>tan</mi>",
            &OperatorSymbol::coth => "<mi>coth</mi>",
            &OperatorSymbol::inf => "<mi>inf</mi>",
            &OperatorSymbol::liminf => "<mi>liminf</mi>",
            &OperatorSymbol::max => "<mi>max</mi>",
            &OperatorSymbol::sin => "<mi>sin</mi>",
            &OperatorSymbol::tanh => "<mi>tanh</mi>",
            &OperatorSymbol::pm => "<mi>pm</mi>",
        }
    }
    fn as_latex(&self) -> &'static str {
        match self {
            &OperatorSymbol::int => "\\int",
            &OperatorSymbol::oint => "\\oint",
            &OperatorSymbol::sum => "\\sum",
            &OperatorSymbol::prod => "\\prod",
            &OperatorSymbol::arccos => "\\arccos",
            &OperatorSymbol::cos => "\\cos",
            &OperatorSymbol::csc => "\\csc",
            &OperatorSymbol::exp => "\\exp",
            &OperatorSymbol::limsup => "\\limsup",
            &OperatorSymbol::min => "\\min",
            &OperatorSymbol::sinh => "\\sinh",
            &OperatorSymbol::arcsin => "\\arcsin",
            &OperatorSymbol::cosh => "\\cosh",
            &OperatorSymbol::gcd => "\\gcd",
            &OperatorSymbol::lg => "\\lg",
            &OperatorSymbol::ln => "\\ln",
            &OperatorSymbol::sup => "\\sup",
            &OperatorSymbol::arctan => "\\arctan",
            &OperatorSymbol::cot => "\\cot",
            &OperatorSymbol::det => "\\det",
            &OperatorSymbol::lim => "\\lim",
            &OperatorSymbol::log => "\\log",
            &OperatorSymbol::sec => "\\sec",
            &OperatorSymbol::tan => "\\tan",
            &OperatorSymbol::coth => "\\coth",
            &OperatorSymbol::inf => "\\inf",
            &OperatorSymbol::liminf => "\\liminf",
            &OperatorSymbol::max => "\\max",
            &OperatorSymbol::sin => "\\sin",
            &OperatorSymbol::tanh => "\\tanh",
            &OperatorSymbol::pm => "\\pm",
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
    fn as_latex(&self) -> &'static str {
        match self {
            &StandaloneSymbol::alpha => "\\alpha",
            &StandaloneSymbol::beta => "\\beta",
            &StandaloneSymbol::gamma => "\\gamma",
            &StandaloneSymbol::delta => "\\delta",
            &StandaloneSymbol::epsilon => "\\epsilon",
            &StandaloneSymbol::zeta => "\\zeta",
            &StandaloneSymbol::eta => "\\eta",
            &StandaloneSymbol::theta => "\\theta",
            &StandaloneSymbol::iota => "\\iota",
            &StandaloneSymbol::kappa => "\\kappa",
            &StandaloneSymbol::lambda => "\\lambda",
            &StandaloneSymbol::mu => "\\mu",
            &StandaloneSymbol::nu => "\\nu",
            &StandaloneSymbol::omicron => "\\omicron",
            &StandaloneSymbol::pi => "\\pi",
            &StandaloneSymbol::rho => "\\rho",
            &StandaloneSymbol::sigma => "\\sigma",
            &StandaloneSymbol::tau => "\\tau",
            &StandaloneSymbol::upsilon => "\\upsilon",
            &StandaloneSymbol::phi => "\\phi",
            &StandaloneSymbol::chi => "\\chi",
            &StandaloneSymbol::psi => "\\psi",
            &StandaloneSymbol::omega => "\\omega",
            &StandaloneSymbol::Gamma => "\\Gamma",
            &StandaloneSymbol::Delta => "\\Delta",
            &StandaloneSymbol::Theta => "\\Theta",
            &StandaloneSymbol::Lambda => "\\Lambda",
            &StandaloneSymbol::Pi => "\\Pi",
            &StandaloneSymbol::Sigma => "\\Sigma",
            &StandaloneSymbol::Upsilon => "\\Upsilon",
            &StandaloneSymbol::Phi => "\\Phi",
            &StandaloneSymbol::Psi => "\\Psi",
            &StandaloneSymbol::Omega => "\\Omega",
            &StandaloneSymbol::partial => "\\partial",
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
    fn as_latex(&self) -> &'static str {
        match self {
            &Symbol::Standalone(ref sym) => sym.as_latex(),
            &Symbol::Operator(ref sym) => sym.as_latex(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum EqOrExpr {
    Eq(Equation),
    Ex(Expression),
}

impl fmt::Display for EqOrExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &EqOrExpr::Eq(ref eq) => write!(f, "{}", eq),
            &EqOrExpr::Ex(ref ex) => write!(f, "{}", ex),
        }
    }
}

impl Indexable for EqOrExpr {
    fn get(&self, index: TreeIdxRef) -> Result<&Expression, AlgebraDSLError> {
        match self {
            &EqOrExpr::Eq(ref eq) => eq.get(index),
            &EqOrExpr::Ex(ref ex) => ex.get(index),
        }
    }
    fn get_mut(&mut self, index: TreeIdxRef) -> Result<&mut Expression, AlgebraDSLError> {
        match self {
            &mut EqOrExpr::Eq(ref mut eq) => eq.get_mut(index),
            &mut EqOrExpr::Ex(ref mut ex) => ex.get_mut(index),
        }
    }
}

impl EqOrExpr {
    pub fn simplify_constants(self) -> Self {
        use self::EqOrExpr::*;
        match self {
            Eq(eq) => Eq(eq.simplify_constants()),
            Ex(ex) => Ex(ex.simplify_constants()),
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
                        mathTreeNode=\"0\"><mo form=\"prefix\">(</mo><mfrac><mrow \
                        mathTreeNode=\"0,0\"><mi>x</mi></mrow><mrow \
                        mathTreeNode=\"0,1\"><mi>y</mi></mrow></mfrac>\
                        <mo form=\"postfix\">)</mo></mrow></math>";
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
                        mathTreeNode=\"0\"><mo form=\"prefix\">(</mo><msub><mrow \
                        mathTreeNode=\"0,0\"><mi>x</mi></mrow><mrow \
                        mathTreeNode=\"0,1\"><mi>y</mi></mrow></msub><mo \
                        form=\"postfix\">)</mo></mrow></math>";
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
    fn format_prod_add() {
        let expr = Ex::Product(vec![
            Ex::Sum(vec![Ex::Atom(Atom::PlainVariable('x')), Ex::Atom(Atom::PlainVariable('y'))]),
            Ex::Sum(vec![Ex::Atom(Atom::PlainVariable('z')), Ex::Atom(Atom::PlainVariable('a'))])]);
        let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow \
                        mathTreeNode=\"0\"><mrow \
                        mathTreeNode=\"0,0\"><mo form=\"prefix\">(</mo><mrow \
                        mathTreeNode=\"0,0,0\"><mi>x</mi></mrow><mo>+</mo><mrow \
                        mathTreeNode=\"0,0,1\"><mi>y</mi></mrow><mo form=\"postfix\">)</mo></mrow><mo>&#8290;</mo><mrow \
                        mathTreeNode=\"0,1\"><mo form=\"prefix\">(</mo><mrow \
                        mathTreeNode=\"0,1,0\"><mi>z</mi></mrow><mo>+</mo><mrow \
                        mathTreeNode=\"0,1,1\"><mi>a</mi></mrow><mo form=\"postfix\">)</mo></mrow></mrow></math>";
        let test = format!("{}", expr);
        assert_expected_eq_actual!(expected, test);
    }

    #[test]
    fn replace_siblings() {
        let mut before = Ex::Product(vec![Ex::Atom(Atom::Natural(3)),
                                          Ex::Atom(Atom::Natural(4)),
                                          Ex::Atom(Atom::PlainVariable('x')),
                                          Ex::Atom(Atom::Natural(7))]);
        let index_strings = vec!["#(mtn:0,0)", "#(mtn:0,1)", "#(mtn:0,3)"];
        let after = Ex::Product(vec![Ex::Atom(Atom::Natural(84)),
                                     Ex::Atom(Atom::PlainVariable('x'))]);
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
        let v = vec![TreeIdx::from_str("#(mtn:0,1)").unwrap()];
        let idx = SiblingIndices::from_indices(&v).unwrap();
        let after = Ex::Product(vec![Ex::Atom(Atom::Natural(3)),
                                     Ex::Atom(Atom::PlainVariable('x')),
                                     Ex::Atom(Atom::Natural(7))]);
        before.delete(idx).unwrap();
        assert_expected_eq_actual!(after, before);
    }

    #[test]
    fn simplify_product() {
        let before = Ex::Product(vec![Ex::Atom(Atom::Natural(3)),
                                      Ex::Atom(Atom::Natural(4)),
                                      Ex::Atom(Atom::Natural(7))]);
        let after = Ex::Atom(Atom::Natural(84));
        assert_expected_eq_actual!(after, before.simplify_constants());

        let before = Ex::Product(vec![Ex::Atom(Atom::Natural(3)),
                                      Ex::Atom(Atom::Floating(4.0)),
                                      Ex::Atom(Atom::Natural(7))]);
        let after = Ex::Atom(Atom::Floating(84.0));
        assert_expected_eq_actual!(after, before.simplify_constants());
    }

    #[test]
    fn simplify_sum() {
        let before = Ex::Sum(vec![Ex::Atom(Atom::Natural(3)),
                                  Ex::Atom(Atom::Natural(4)),
                                  Ex::Atom(Atom::Natural(7))]);
        let after = Ex::Atom(Atom::Natural(14));
        assert_expected_eq_actual!(after, before.simplify_constants());

        let before = Ex::Sum(vec![Ex::Atom(Atom::Natural(3)),
                                  Ex::Atom(Atom::Floating(4.0)),
                                  Ex::Atom(Atom::Natural(7))]);
        let after = Ex::Atom(Atom::Floating(14.0));
        assert_expected_eq_actual!(after, before.simplify_constants());
    }

    #[test]
    fn simplify_division() {
        let before = Ex::Division(box Ex::Atom(Atom::Natural(8)),
                                  box Ex::Atom(Atom::Natural(4)));
        let after = Ex::Atom(Atom::Natural(2));
        assert_expected_eq_actual!(after, before.simplify_constants());

        let before = Ex::Division(box Ex::Atom(Atom::Floating(8.)),
                                  box Ex::Atom(Atom::Floating(4.)));
        let after = Ex::Atom(Atom::Floating(2.));
        assert_expected_eq_actual!(after, before.simplify_constants());

    }
}
