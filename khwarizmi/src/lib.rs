#![feature(box_syntax, box_patterns, slice_patterns, advanced_slice_patterns, try_from)]
#![allow(dead_code)]
#[macro_use]
extern crate nom;
mod parser;
mod output;
mod symbols;
mod iter;

pub use output::LatexWriter;

pub use symbols::{Symbol, StandaloneSymbol, OperatorSymbol, FunctionSymbol};

pub use iter::{ChildIter, ExpressionIter};

use std::convert::TryFrom;
use std::str::FromStr;
use std::{fmt, mem};

use parser::ParseError;

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
            if Some(Ok(0)) != idxs.next() {
                return Err(AlgebraDSLError::IllFormattedIndex);
            }
            let mut v = vec![];
            for idx in idxs {
                v.push(idx?);
            }
            Ok(TreeIdx(v))
        }
    }
    fn from_vec(v: Vec<TreeInt>) -> Self {
        TreeIdx(v)
    }
    fn as_ref<'a>(&'a self) -> TreeIdxRef<'a> {
        TreeIdxRef(&self.0[..])
    }
    fn push(&mut self, i: TreeInt) {
        self.0.push(i)
    }
    fn pop(&mut self) -> Option<TreeInt> {
        self.0.pop()
    }
    fn get(&self, i: usize) -> Option<TreeInt> {
        self.0.get(i).cloned()
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
    pub fn grandparent(&self) -> Option<TreeIdxRef<'a>> {
        if self.len() > 1 {
            Some(TreeIdxRef(&self.0[..self.len() - 2]))
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
    pub fn prefix(&self) -> Option<TreeIdxRef<'a>> {
        if self.len() > 0 {
            Some(TreeIdxRef(&self.0[0..(self.len() - 1)]))
        } else {
            None
        }
    }
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn tail_from(&self, n: usize) -> Option<TreeIdxRef<'a>> {
        if n < self.len() {
            Some(TreeIdxRef(&self.0[n..]))
        } else {
            None
        }
    }
    pub fn to_owned(&self) -> TreeIdx {
        TreeIdx(self.0.iter().cloned().collect())
    }
}

impl Equation {
    pub fn expr_iter(&self) -> ExpressionIter {
        ExpressionIter::new(EqOrExprRef::Eq(self))
    }
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

#[derive(PartialEq, Debug, Clone)]
pub struct SiblingIndices {
    parent_idx: TreeIdx,
    // This list *must* be sorted. This is enforced by the constructor
    children: Vec<TreeInt>,
}

impl SiblingIndices {
    fn parent(&self) -> TreeIdxRef {
        self.parent_idx.as_ref()
    }
}

pub trait Indexable: fmt::Display + fmt::Debug + Clone {
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
        index.as_ref().parent().map(|idx| self.flatten(idx));
        Ok(old)
    }
    fn delete(&mut self, indices: SiblingIndices) -> Result<(), AlgebraDSLError> {
        let cp = self.clone();
        let res = self.replace_with_inner(&indices, None, AlgebraDSLError::InvalidDelete);
        if let Err(_) = res {
            *self = cp;
        }
        res
    }

    /// May eat the structure on error
    ///
    /// If `expr` is present, puts it in the first location indicated by `indices`. If not, it just
    /// removes the locations specified by indices.
    fn replace_with_inner(&mut self,
                          indices: &SiblingIndices,
                          expr: Option<Expression>,
                          gen_err: AlgebraDSLError)
                          -> Result<(), AlgebraDSLError> {
        fn insert_many<T, I: IntoIterator<Item = T>>(v: &mut Vec<T>, idx: usize, i: I) {
            let back = v.split_off(idx);
            v.extend(i.into_iter());
            v.extend(back.into_iter());
        }
        use Expression as Ex;
        let &SiblingIndices { ref parent_idx, ref children } = indices;
        let parent_ref = self.get_mut(parent_idx.as_ref())?;
        let insert_idx = *children.get(0).ok_or(AlgebraDSLError::InvalidSiblingIndices)?;
        let result = match parent_ref.take() {
            Ex::Sum(args) => {
                if args.len() == children.len() {
                    return Err(gen_err);
                }
                let mut new_exprs = delete_prod(args, children.as_slice());
                expr.map(|e| match e {
                    Ex::Sum(args) => {
                        insert_many(&mut new_exprs, insert_idx, args);
                    }
                    e => new_exprs.insert(insert_idx, e),
                });
                Ex::sum_many(new_exprs).ok_or(gen_err)
            }
            Ex::Division(top, bottom) => {
                let original_top_len = top.len();
                let bottom_indices = children.iter()
                    .cloned()
                    .filter_map(|i| {
                        if i >= top.len() && i < top.len() + bottom.len() {
                            Some(i - top.len())
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>();
                let top_indices = children.iter()
                    .cloned()
                    .filter_map(|i| { if i < top.len() { Some(i) } else { None } })
                    .collect::<Vec<_>>();
                let mut new_top = delete_prod(top, top_indices.as_slice());
                let mut new_bottom = delete_prod(bottom, bottom_indices.as_slice());

                expr.map(|e| {
                    // TODO
                    fn insert_division(prod: &mut Vec<Ex>, idx: usize, e: Ex) {
                        match e {
                            Ex::Division(t, b) => {
                                if b.len() == 0 {
                                    insert_many(prod, idx, t);
                                } else {
                                    prod.insert(idx, Ex::Division(t, b));
                                }
                            }
                            e => prod.insert(idx, e),
                        }
                    }
                    if insert_idx < original_top_len {
                        insert_division(&mut new_top, insert_idx, e);
                    } else {
                        insert_division(&mut new_bottom, insert_idx - original_top_len, e);
                    }
                });
                Ok(Ex::divide_products(new_top, new_bottom))
            }
            _ => Err(gen_err),
        };
        result.map(|e| {
            *parent_ref = e;
        })
    }

    fn replace_siblings(&mut self,
                        indices: SiblingIndices,
                        expr: Expression)
                        -> Result<(), AlgebraDSLError> {

        let cp = self.clone();
        let res = self.replace_with_inner(&indices, Some(expr), AlgebraDSLError::InvalidMake);
        if let Err(_) = res {
            *self = cp;
        }
        self.flatten(indices.parent())?;
        res
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
    fn flatten(&mut self, idx: TreeIdxRef) -> Result<(), AlgebraDSLError> {
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
            Expression::Division(top, bottom) => {
                let mut new_top = vec![];
                let mut new_bottom = vec![];
                top.into_iter()
                    .map(|e| match e {
                        Expression::Division(t, b) => {
                            new_top.extend(t.into_iter());
                            new_bottom.extend(b.into_iter());
                        }
                        e => new_top.push(e),
                    })
                    .count();
                bottom.into_iter()
                    .map(|e| match e {
                        Expression::Division(t, b) => {
                            new_bottom.extend(t.into_iter());
                            new_top.extend(b.into_iter());
                        }
                        e => new_bottom.push(e),
                    })
                    .count();
                Expression::Division(new_top, new_bottom)
            }
            e => e,
        };
        *expr = replacement;
        Ok(())
    }

    /// If all the input indices share a parent, constructs a sibling index representing all of
    /// them
    fn make_siblings(&self, indices: &[TreeIdx]) -> Result<SiblingIndices, AlgebraDSLError> {
        fn stem(indices: &[TreeIdx]) -> Option<(TreeIdx, Vec<TreeIdx>)> {
            let mut v = vec![];
            let mut i = 0;
            if indices.len() == 0 {
                return None;
            }
            loop {
                let first = match indices[0].get(i) {
                    None => break,
                    Some(first) => first,
                };
                if indices.iter().any(|idx| idx.get(i) != Some(first)) {
                    break;
                }
                v.push(first);
                i += 1;
            }
            let tails = indices.iter()
                .filter_map(|idx: &TreeIdx| idx.as_ref().tail_from(i).map(|idx| idx.to_owned()))
                .collect();
            Some((TreeIdx(v), tails))
        }

        if indices.len() < 1 ||
           indices.iter().map(|i| i.as_ref().len()).min().expect(UNREACH) == 0 {
            return Err(AlgebraDSLError::InvalidSiblingIndices);
        }


        let (mut trunk, mut branches) =
            stem(indices).ok_or(AlgebraDSLError::InvalidSiblingIndices)?;

        // Make sure there is at least one branch, pushing back the trunk if needed
        if branches.len() == 0 {
            // If there is only one index, push the trunk up one.
            match trunk.pop() {
                Some(last) => branches.push(TreeIdx(vec![last])),
                None => return Err(AlgebraDSLError::InvalidSiblingIndices),
            }
        }

        // Collect the leading index for each branch.
        let mut single_indices: Vec<_> =
            branches.iter().map(|idx| idx.as_ref().first().expect(UNREACH)).collect();
        single_indices.sort();
        single_indices.dedup();

        // Error out if there are extra indices
        // However, if the parent is a sum and we're refering to the interior of a negation, then
        // it's okay.
        match self.get(trunk.as_ref())? {
            &Expression::Sum(ref args) => {
                if branches.iter().any(|idx| {
                    if idx.as_ref().len() != 1 {
                        if let Some(&Expression::Negation(_)) = idx.as_ref()
                            .first()
                            .and_then(|i| args.get(i)) {
                            idx.as_ref().len() > 2
                        } else {
                            true
                        }
                    } else {
                        false
                    }
                }) {
                    return Err(AlgebraDSLError::InvalidSiblingIndices);
                }
            }
            _ => {
                if branches.iter().any(|idx| idx.as_ref().len() > 1) {
                    return Err(AlgebraDSLError::InvalidSiblingIndices);
                }
            }
        }

        Ok(SiblingIndices {
            parent_idx: trunk,
            children: single_indices,
        })
    }
}

fn delete_prod(mut p: Vec<Expression>, is: &[TreeInt]) -> Vec<Expression> {
    remove_args(&mut p, is);
    p
}

fn remove_args(args: &mut Vec<Expression>, idxs: &[TreeInt]) {
    for idx in idxs.iter().rev() {
        args.remove(*idx);
    }
}

impl Expression {
    pub fn from_str(expr: &str) -> Result<Self, AlgebraDSLError> {
        parser::parse_expr(expr).map_err(AlgebraDSLError::Parse)
    }
    pub fn take(&mut self) -> Self {
        mem::replace(self, Expression::Atom(Atom::Natural(0)))
    }
    pub fn expr_iter(&self) -> ExpressionIter {
        ExpressionIter::new(EqOrExprRef::Ex(self))
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
            Expression::Division(mut top, bottom) => {
                top.push(expr);
                Expression::Division(top, bottom)
            }
            not_prod => Expression::Division(vec![not_prod, expr], vec![]),
        }
    }
    pub fn inflate_division(self, expr: Expression) -> Self {
        match self {
            Expression::Division(top, mut bottom) => {
                bottom.push(expr);
                Expression::Division(top, bottom)
            }
            not_prod => Expression::Division(vec![not_prod], vec![expr]),
        }
    }

    fn simplify_product(exprs: Vec<Expression>) -> (f64, i64, Vec<Expression>) {
        use Expression as Ex;
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
        (f_acc, n_acc, new_exprs)
    }

    pub fn simplify_constants(self) -> Self {
        fn gcd(mut a: i64, mut b: i64) -> i64 {
            a = a.abs();
            b = b.abs();
            while b != 0 {
                if b > a {
                    mem::swap(&mut a, &mut b);
                } else {
                    let r = a % b;
                    a = b;
                    b = r;
                }
            }
            return a;
        }
        fn reduce_division(mut top: i64, mut bottom: i64) -> (i64, i64) {
            let c = gcd(top, bottom);
            top /= c;
            bottom /= c;
            if top < 0 {
                (-top, -bottom)
            } else {
                (top, bottom)
            }
        }
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
                    if n_acc != 0 || new_exprs.len() == 0 {
                        new_exprs.push(Ex::Atom(Atom::Natural(n_acc)));
                    }
                }
                if new_exprs.len() == 1 {
                    new_exprs.pop().expect(UNREACH)
                } else {
                    Ex::Sum(new_exprs)
                }
            }
            Ex::Division(top, bottom) => {
                let (top_float, top_nat, mut top_exprs) = Ex::simplify_product(top);
                let (bottom_float, bottom_nat, mut bottom_exprs) = Ex::simplify_product(bottom);
                let float = top_float / bottom_float;
                let (r_top_nat, r_bottom_nat) = reduce_division(top_nat, bottom_nat);
                if float != 1.0 {
                    let net_float = float * (r_top_nat as f64) / (r_bottom_nat as f64);
                    top_exprs.insert(0, Ex::Atom(Atom::Floating(net_float)));
                } else {
                    if r_top_nat != 1 {
                        top_exprs.insert(0, Ex::Atom(Atom::Natural(r_top_nat)));
                    }
                    if r_bottom_nat != 1 {
                        bottom_exprs.insert(0, Ex::Atom(Atom::Natural(r_bottom_nat)));
                    }
                }
                Ex::divide_products(top_exprs, bottom_exprs)
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
                Ex::LimitOp(sym,
                            sub.map(|box x| box x.simplify_constants()),
                            sup.map(|box x| box x.simplify_constants()),
                            box op.simplify_constants())
            }
            Ex::Application(func, arg) => Ex::Application(func, box arg.simplify_constants()),
            e => e,
        }
    }

    /// If this thing is a product (a `Division` without a denominator), produces that list of
    /// expressions. Otherwise returns the original
    fn as_product(self) -> Result<Vec<Self>, Self> {
        match self {
            Expression::Division(t, b) => {
                if b.len() == 0 {
                    Ok(t)
                } else {
                    Err(Expression::Division(t, b))
                }
            }
            e => Err(e),
        }
    }

    /// Returns whether this is a product (has an empty denominator)
    fn is_product(&self) -> bool {
        match self {
            &Expression::Division(_, ref b) if b.len() == 0 => true,
            _ => false,
        }
    }

    /// Creates an `Expression` with `top` divided by `bottom`, doing 1-level flattening
    fn divide(top: Expression, bottom: Expression) -> Expression {
        let mut top_vec = vec![];
        let mut bottom_vec = vec![];
        top.as_product()
            .map(|prod| top_vec.extend(prod))
            .map_err(|other| top_vec.push(other))
            .unwrap_or(());
        bottom.as_product()
            .map(|prod| bottom_vec.extend(prod))
            .map_err(|other| bottom_vec.push(other))
            .unwrap_or(());
        Expression::Division(top_vec, bottom_vec)
    }

    /// Creates an `Expression` with `top`s divided by `bottom`s
    fn divide_products(mut top: Vec<Expression>, bottom: Vec<Expression>) -> Expression {
        let one = Expression::Atom(Atom::Natural(1));
        match (top.len(), bottom.len()) {
            (0, 0) => one,
            (1, 0) => top.pop().expect(UNREACH),
            (0, _) => Expression::Division(vec![one], bottom),
            (_, _) => Expression::Division(top, bottom),
        }
    }

    /// Creates an `Expression` with `top`s divided by `bottom`s
    fn sum_many(mut summands: Vec<Expression>) -> Option<Expression> {
        match summands.len() {
            0 => None,
            1 => summands.pop(),
            _ => Some(Expression::Sum(summands)),
        }
    }

    /// Creates an `Expression` with `left` times `right`, doing 1-level flattening
    fn multiply(left: Expression, right: Expression) -> Expression {
        let mut vec = Vec::new();
        left.as_product()
            .map(|prod| vec.extend(prod))
            .map_err(|other| vec.push(other))
            .unwrap_or(());
        right.as_product()
            .map(|prod| vec.extend(prod))
            .map_err(|other| vec.push(other))
            .unwrap_or(());
        Expression::Division(vec, Vec::new())
    }
}

impl Indexable for Expression {
    fn get(&self, index: TreeIdxRef) -> Result<&Expression, AlgebraDSLError> {
        match index.first() {
            None => Ok(self),
            Some(first) => {
                let rest = index.rest();
                match self {
                    &Expression::Negation(ref e) if first == 0 => e.get(rest),
                    &Expression::Sum(ref e) if first < e.len() => e[first].get(rest),
                    &Expression::Division(ref t, ref b) if first < t.len() + b.len() => {
                        if first < t.len() {
                            t[first].get(rest)
                        } else {
                            b[first - t.len()].get(rest)
                        }
                    }
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
                    &mut Expression::Division(ref mut t, ref mut b) if first <
                                                                       t.len() + b.len() => {
                        if first < t.len() {
                            t[first].get_mut(rest)
                        } else {
                            b[first - t.len()].get_mut(rest)
                        }
                    }
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
    InvalidMake,
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
    /// Division(numerator_prod, denominator_prod)
    Division(Vec<Expression>, Vec<Expression>),
    Power(Box<Expression>, Box<Expression>),
    Subscript(Box<Expression>, Box<Expression>),
    /// Operator, Sub, Super, Operand
    LimitOp(OperatorSymbol, Option<Box<Expression>>, Option<Box<Expression>>, Box<Expression>),
    Application(Box<Expression>, Box<Expression>),
    /// An indivisible unit, like a variable or numeric literal
    Atom(Atom),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Atom {
    PlainVariable(char),
    Escaped(String),
    Natural(i64),
    Floating(f64),
    Symbol(Symbol),
}

#[derive(Debug, PartialEq, Clone)]
pub enum EqOrExpr {
    Eq(Equation),
    Ex(Expression),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum EqOrExprRef<'a> {
    Eq(&'a Equation),
    Ex(&'a Expression),
}

impl<'a> EqOrExprRef<'a> {
    pub fn expr_iter(self) -> ExpressionIter<'a> {
        ExpressionIter::new(self)
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
    pub fn from_str(s: &str) -> Result<Self, AlgebraDSLError> {
        if let Ok(eq) = Equation::from_str(s) {
            Ok(EqOrExpr::Eq(eq))
        } else {
            let ex = Expression::from_str(s)?;
            Ok(EqOrExpr::Ex(ex))
        }
    }
    pub fn as_ref(&self) -> EqOrExprRef {
        match self {
            &EqOrExpr::Ex(ref e) => EqOrExprRef::Ex(e),
            &EqOrExpr::Eq(ref e) => EqOrExprRef::Eq(e),
        }
    }
    pub fn expr_iter(&self) -> ExpressionIter {
        self.as_ref().expr_iter()
    }
}

#[cfg(test)]
mod tests;
