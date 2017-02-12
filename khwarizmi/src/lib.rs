#![feature(box_syntax, box_patterns, slice_patterns, advanced_slice_patterns, try_from)]
#![allow(dead_code)]
#[macro_use]
extern crate nom;
mod parser;
mod output;

pub use output::LatexWriter;

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
pub enum SiblingIndices {
    // Siblings in an associative soup (a sum or product).
    AssocSoup {
        parent_idx: TreeIdx,
        // This list *must* be sorted. This is enforced by the constructor
        children: Vec<TreeInt>,
    },
    Division {
        division_idx: TreeIdx,
        // These lists *must* be sorted. This is enforced by the constructor
        //
        // If they are empty it is understood to mean delete *no* elements
        // If they are None that is understood to mean delete *all*
        top_children: Option<Vec<TreeInt>>,
        bottom_children: Option<Vec<TreeInt>>,
    },
}

impl SiblingIndices {
    fn parent(&self) -> TreeIdxRef {
        match self {
            &SiblingIndices::AssocSoup { ref parent_idx, .. } => parent_idx.as_ref(),
            &SiblingIndices::Division { ref division_idx, .. } => division_idx.as_ref(),
        }
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
        index.as_ref().parent().map(|idx| self.maybe_assoc_merge(idx));
        Ok(old)
    }
    fn delete(&mut self, indices: SiblingIndices) -> Result<(), AlgebraDSLError> {
        let cp = self.clone();
        let res = self.delete_inner(indices);
        if let Err(_) = res {
            *self = cp;
        }
        res
    }

    /// May eat the structure on error
    fn delete_inner(&mut self, indices: SiblingIndices) -> Result<(), AlgebraDSLError> {
        use Expression as Ex;
        match indices {
            SiblingIndices::AssocSoup { parent_idx, children } => {
                match self.get_mut(parent_idx.as_ref())? {
                    &mut Ex::Sum(ref mut args) |
                    &mut Ex::Product(ref mut args) => {
                        if args.len() == children.len() {
                            return Err(AlgebraDSLError::InvalidDelete);
                        }

                        for idx in children.iter().rev() {
                            args.remove(*idx);
                        }
                        Ok(())
                    }
                    _ => Err(AlgebraDSLError::InvalidSiblingIndices),
                }
            }
            SiblingIndices::Division { division_idx, top_children, bottom_children } => {
                let expr_ref = self.get_mut(division_idx.as_ref())?;
                match expr_ref {
                    &mut Ex::Division(..) => {}
                    _ => return Err(AlgebraDSLError::InvalidDelete),
                };
                let (top, bottom) = match expr_ref.take() {
                    Ex::Division(box top, box bottom) => {
                        let new_top = match top_children.as_ref().map(Vec::as_slice) {
                            Some(slice) if slice.len() == 0 => vec![top],
                            Some(slice) => {
                                match top {
                                    Ex::Product(args) => delete_prod(args, slice),
                                    _ => return Err(AlgebraDSLError::InvalidDelete),
                                }
                            }
                            None => vec![],
                        };
                        let new_bottom = match bottom_children.as_ref().map(Vec::as_slice) {
                            Some(slice) if slice.len() == 0 => vec![bottom],
                            Some(slice) => {
                                match bottom {
                                    Ex::Product(args) => delete_prod(args, slice),
                                    _ => return Err(AlgebraDSLError::InvalidDelete),
                                }
                            }
                            None => vec![],
                        };
                        (new_top, new_bottom)
                    }
                    _ => unreachable!(),
                };
                *expr_ref = make_division(top, bottom);
                Ok(())
            }
        }
    }

    /// May eat the structure on error
    fn replace_with_inner(&mut self,
                          indices: &SiblingIndices,
                          expr: Expression)
                          -> Result<(), AlgebraDSLError> {
        use Expression as Ex;
        match indices {
            &SiblingIndices::AssocSoup { ref parent_idx, ref children } => {
                let r = self.get_mut(parent_idx.as_ref())?;
                let new: Expression = match r.take() {
                    Ex::Sum(mut args) => {
                        remove_args(&mut args, children.as_slice());
                        args.insert(children.iter().min().cloned().unwrap_or(0), expr);
                        if args.len() == 1 {
                            args.remove(0)
                        } else {
                            Ex::Sum(args)
                        }
                    }
                    Ex::Product(mut args) => {
                        remove_args(&mut args, children.as_slice());
                        args.insert(children.iter().min().cloned().unwrap_or(0), expr);
                        if args.len() == 1 {
                            args.remove(0)
                        } else {
                            Ex::Product(args)
                        }
                    }
                    _ => return Err(AlgebraDSLError::InvalidSiblingIndices),
                };
                *r = new;
                Ok(())
            }
            &SiblingIndices::Division { ref division_idx,
                                        ref top_children,
                                        ref bottom_children } => {
                let expr_ref = self.get_mut(division_idx.as_ref())?;
                match expr_ref {
                    &mut Ex::Division(..) => {}
                    _ => return Err(AlgebraDSLError::InvalidMake),
                };
                let (mut top, mut bottom) = match expr_ref.take() {
                    Ex::Division(box top, box bottom) => {
                        let new_top = match top_children.as_ref().map(Vec::as_slice) {
                            Some(slice) if slice.len() == 0 => vec![top],
                            Some(slice) => {
                                match top {
                                    Ex::Product(args) => delete_prod(args, slice),
                                    _ => return Err(AlgebraDSLError::InvalidDelete),
                                }
                            }
                            None => vec![],
                        };
                        let new_bottom = match bottom_children.as_ref().map(Vec::as_slice) {
                            Some(slice) if slice.len() == 0 => vec![bottom],
                            Some(slice) => {
                                match bottom {
                                    Ex::Product(args) => delete_prod(args, slice),
                                    _ => return Err(AlgebraDSLError::InvalidDelete),
                                }
                            }
                            None => vec![],
                        };
                        (new_top, new_bottom)
                    }
                    _ => unreachable!(),
                };

                // We figure out if it would be reasonable for the user to insert in various
                // location, regardless of the expression they're inserting.
                let top_idx = top_children.as_ref()
                    .map(|c| c.iter().cloned().min().unwrap_or(0))
                    .unwrap_or(0);
                let bottom_idx = bottom_children.as_ref()
                    .map(|c| c.iter().cloned().min().unwrap_or(0))
                    .unwrap_or(0);
                match expr {
                    Ex::Division(box insert_top, box insert_bottom) => {
                        top.insert(top_idx, insert_top);
                        bottom.insert(bottom_idx, insert_bottom);
                    }
                    e => {
                        match (top_children, bottom_children) {
                            (&Some(ref t), &Some(_)) if t.len() == 0 => {
                                bottom.insert(bottom_idx, e)
                            }
                            _ => top.insert(top_idx, e),
                        }
                    }
                };
                *expr_ref = make_division(top, bottom);
                Ok(())
            }
        }
    }
    fn replace_siblings(&mut self,
                        indices: SiblingIndices,
                        expr: Expression)
                        -> Result<(), AlgebraDSLError> {

        let cp = self.clone();
        let res = self.replace_with_inner(&indices, expr);
        if let Err(_) = res {
            *self = cp;
        }
        self.maybe_assoc_merge(indices.parent())?;
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
    fn maybe_assoc_merge(&mut self, idx: TreeIdxRef) -> Result<(), AlgebraDSLError> {
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

        if branches.len() == 0 {
            // If there is only one index, push the trunk up one.
            match trunk.pop() {
                Some(last) => branches.push(TreeIdx(vec![last])),
                None => return Err(AlgebraDSLError::InvalidSiblingIndices),
            }
        }

        match self.get(trunk.as_ref())? {
            &Expression::Sum(ref args) |
            &Expression::Product(ref args) => {
                if branches.iter().any(|idx| {
                    if idx.as_ref().len() != 1 {
                        if let Some(&Expression::Negation(_)) =
                            idx.as_ref().first().and_then(|i| args.get(i)) {
                            false
                        } else {
                            true
                        }
                    } else {
                        false
                    }
                }) {
                    return Err(AlgebraDSLError::InvalidSiblingIndices);
                }
                let mut single_indices: Vec<_> =
                    branches.iter().map(|idx| idx.as_ref().first().expect(UNREACH)).collect();
                single_indices.sort();
                single_indices.dedup();


                if let Some(&Expression::Division(..)) =
                    trunk.as_ref().prefix().and_then(|idx| self.get(idx).ok()) {
                    let last = trunk.pop().expect(UNREACH);
                    if last == 0 {
                        Ok(SiblingIndices::Division {
                            division_idx: trunk,
                            top_children: Some(single_indices),
                            bottom_children: Some(vec![]),
                        })
                    } else {
                        Ok(SiblingIndices::Division {
                            division_idx: trunk,
                            top_children: Some(vec![]),
                            bottom_children: Some(single_indices),
                        })
                    }
                } else {
                    Ok(SiblingIndices::AssocSoup {
                        parent_idx: trunk,
                        children: single_indices,
                    })
                }
            }
            &Expression::Division(..) => {
                if branches.iter().any(|idx| idx.as_ref().len() > 2 || idx.as_ref().len() == 0) {
                    return Err(AlgebraDSLError::InvalidSiblingIndices);
                }
                let delete_top = branches.iter()
                    .any(|idx| idx.as_ref().len() == 1 && idx.as_ref().first() == Some(0));
                let delete_bottom = branches.iter()
                    .any(|idx| idx.as_ref().len() == 1 && idx.as_ref().first() == Some(1));
                if delete_top &&
                   branches.iter()
                    .any(|idx| idx.as_ref().first() == Some(0) && idx.as_ref().len() > 1) {
                    return Err(AlgebraDSLError::InvalidSiblingIndices);
                }
                if delete_bottom &&
                   branches.iter()
                    .any(|idx| idx.as_ref().first() == Some(1) && idx.as_ref().len() > 1) {
                    return Err(AlgebraDSLError::InvalidSiblingIndices);
                }
                let top_vec = if delete_top {
                    None
                } else {
                    Some(branches.iter()
                        .filter(|idx| idx.0[0] == 0)
                        .filter_map(|idx| idx.0.get(0).cloned())
                        .collect())
                };
                let bottom_vec = if delete_bottom {
                    None
                } else {
                    Some(branches.iter()
                        .filter(|idx| idx.0[0] == 1)
                        .filter_map(|idx| idx.0.get(1).cloned())
                        .collect())
                };
                Ok(SiblingIndices::Division {
                    division_idx: trunk,
                    top_children: top_vec,
                    bottom_children: bottom_vec,
                })
            }
            _ => Err(AlgebraDSLError::InvalidSiblingIndices),
        }
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

fn make_division(mut top: Vec<Expression>, mut bottom: Vec<Expression>) -> Expression {
    use Expression as Ex;
    match (top.len(), bottom.len()) {
        (0, 0) => Ex::Atom(Atom::Natural(1)),
        (0, 1) => Ex::Division(box Ex::Atom(Atom::Natural(1)), box bottom.remove(0)),
        (1, 0) => top.remove(0),
        (1, 1) => Ex::Division(box top.remove(0), box bottom.remove(0)),
        (0, _) => Ex::Division(box Ex::Atom(Atom::Natural(1)), box Ex::Product(bottom)),
        (_, 0) => Ex::Product(top),
        (1, _) => Ex::Division(box top.remove(0), box Ex::Product(bottom)),
        (_, 1) => Ex::Division(box Ex::Product(top), box bottom.remove(0)),
        (_, _) => Ex::Division(box Ex::Product(top), box Ex::Product(bottom)),
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
                Ex::LimitOp(sym,
                            sub.map(|box x| box x.simplify_constants()),
                            sup.map(|box x| box x.simplify_constants()),
                            box op.simplify_constants())
            }
            Ex::Application(func, arg) => Ex::Application(func, box arg.simplify_constants()),
            e => e,
        }
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

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Atom {
    PlainVariable(char),
    Natural(i64),
    Floating(f64),
    Symbol(Symbol),
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
    limsup,
    min,
    gcd,
    sup,
    det,
    lim,
    inf,
    liminf,
    max,
    pm,
    Function(FunctionSymbol),
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionSymbol {
    // Superscriptable
    sin,
    cos,
    tan,
    csc,
    sec,
    cot,
    arcsin,
    arccos,
    arctan,
    sinh,
    cosh,
    tanh,
    coth,

    // Subscriptable & superscriptable
    lg,
    ln,
    log,

    // Neither
    exp,
}

impl FunctionSymbol {
    fn as_latex(&self) -> &'static str {
        match self {
            &FunctionSymbol::arccos => "\\arccos",
            &FunctionSymbol::cos => "\\cos",
            &FunctionSymbol::csc => "\\csc",
            &FunctionSymbol::exp => "\\exp",
            &FunctionSymbol::sinh => "\\sinh",
            &FunctionSymbol::arcsin => "\\arcsin",
            &FunctionSymbol::cosh => "\\cosh",
            &FunctionSymbol::lg => "\\lg",
            &FunctionSymbol::ln => "\\ln",
            &FunctionSymbol::arctan => "\\arctan",
            &FunctionSymbol::cot => "\\cot",
            &FunctionSymbol::log => "\\log",
            &FunctionSymbol::sec => "\\sec",
            &FunctionSymbol::tan => "\\tan",
            &FunctionSymbol::coth => "\\coth",
            &FunctionSymbol::sin => "\\sin",
            &FunctionSymbol::tanh => "\\tanh",
        }
    }
    fn as_math_ml(&self) -> &'static str {
        match self {
            &FunctionSymbol::arccos => "<mi>arccos</mi>",
            &FunctionSymbol::cos => "<mi>cos</mi>",
            &FunctionSymbol::csc => "<mi>csc</mi>",
            &FunctionSymbol::exp => "<mi>exp</mi>",
            &FunctionSymbol::sinh => "<mi>sinh</mi>",
            &FunctionSymbol::arcsin => "<mi>arcsin</mi>",
            &FunctionSymbol::cosh => "<mi>cosh</mi>",
            &FunctionSymbol::lg => "<mi>lg</mi>",
            &FunctionSymbol::ln => "<mi>ln</mi>",
            &FunctionSymbol::arctan => "<mi>arctan</mi>",
            &FunctionSymbol::cot => "<mi>cot</mi>",
            &FunctionSymbol::log => "<mi>log</mi>",
            &FunctionSymbol::sec => "<mi>sec</mi>",
            &FunctionSymbol::tan => "<mi>tan</mi>",
            &FunctionSymbol::coth => "<mi>coth</mi>",
            &FunctionSymbol::sin => "<mi>sin</mi>",
            &FunctionSymbol::tanh => "<mi>tanh</mi>",
        }
    }
    fn as_expr(self) -> Expression {
        Expression::Atom(Atom::Symbol(Symbol::Operator(OperatorSymbol::Function(self))))
    }
    fn accepts_subscripts(self) -> bool {
        use FunctionSymbol::*;
        match self {
            lg | ln | log => true,
            exp => false,
            sin | cos | tan | csc | sec | cot | arcsin | arccos | arctan | sinh | cosh | tanh |
            coth => false,
        }
    }
    fn accepts_superscripts(self) -> bool {
        use FunctionSymbol::*;
        match self {
            lg | ln | log => true,
            exp => false,
            sin | cos | tan | csc | sec | cot | arcsin | arccos | arctan | sinh | cosh | tanh |
            coth => true,
        }
    }
}

impl OperatorSymbol {
    fn as_math_ml(&self) -> &'static str {
        match self {
            &OperatorSymbol::int => "<mo>&int;</mo>",
            &OperatorSymbol::oint => "<mo>&oint;</mo>",
            &OperatorSymbol::sum => "<mo>&sum;</mo>",
            &OperatorSymbol::prod => "<mo>&prod;</mo>",
            &OperatorSymbol::limsup => "<mi>limsup</mi>",
            &OperatorSymbol::min => "<mi>min</mi>",
            &OperatorSymbol::gcd => "<mi>gcd</mi>",
            &OperatorSymbol::sup => "<mi>sup</mi>",
            &OperatorSymbol::det => "<mi>det</mi>",
            &OperatorSymbol::lim => "<mi>lim</mi>",
            &OperatorSymbol::inf => "<mi>inf</mi>",
            &OperatorSymbol::liminf => "<mi>liminf</mi>",
            &OperatorSymbol::max => "<mi>max</mi>",
            &OperatorSymbol::pm => "<mi>pm</mi>",
            &OperatorSymbol::Function(ref f) => f.as_math_ml(),
        }
    }
    fn as_latex(&self) -> &'static str {
        match self {
            &OperatorSymbol::int => "\\int",
            &OperatorSymbol::oint => "\\oint",
            &OperatorSymbol::sum => "\\sum",
            &OperatorSymbol::prod => "\\prod",
            &OperatorSymbol::limsup => "\\limsup",
            &OperatorSymbol::min => "\\min",
            &OperatorSymbol::gcd => "\\gcd",
            &OperatorSymbol::sup => "\\sup",
            &OperatorSymbol::det => "\\det",
            &OperatorSymbol::lim => "\\lim",
            &OperatorSymbol::inf => "\\inf",
            &OperatorSymbol::liminf => "\\liminf",
            &OperatorSymbol::max => "\\max",
            &OperatorSymbol::pm => "\\pm",
            &OperatorSymbol::Function(ref f) => f.as_latex(),
        }
    }
    fn as_expr(self) -> Expression {
        Expression::Atom(Atom::Symbol(Symbol::Operator(self)))
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
            "limsup" => Some(Symbol::Operator(OperatorSymbol::limsup)),
            "min" => Some(Symbol::Operator(OperatorSymbol::min)),
            "gcd" => Some(Symbol::Operator(OperatorSymbol::gcd)),
            "sup" => Some(Symbol::Operator(OperatorSymbol::sup)),
            "det" => Some(Symbol::Operator(OperatorSymbol::det)),
            "lim" => Some(Symbol::Operator(OperatorSymbol::lim)),
            "inf" => Some(Symbol::Operator(OperatorSymbol::inf)),
            "liminf" => Some(Symbol::Operator(OperatorSymbol::liminf)),
            "max" => Some(Symbol::Operator(OperatorSymbol::max)),
            "pm" => Some(Symbol::Operator(OperatorSymbol::pm)),
            "arccos" => Some(Symbol::Operator(OperatorSymbol::Function(FunctionSymbol::arccos))),
            "cos" => Some(Symbol::Operator(OperatorSymbol::Function(FunctionSymbol::cos))),
            "csc" => Some(Symbol::Operator(OperatorSymbol::Function(FunctionSymbol::csc))),
            "exp" => Some(Symbol::Operator(OperatorSymbol::Function(FunctionSymbol::exp))),
            "sinh" => Some(Symbol::Operator(OperatorSymbol::Function(FunctionSymbol::sinh))),
            "arcsin" => Some(Symbol::Operator(OperatorSymbol::Function(FunctionSymbol::arcsin))),
            "cosh" => Some(Symbol::Operator(OperatorSymbol::Function(FunctionSymbol::cosh))),
            "lg" => Some(Symbol::Operator(OperatorSymbol::Function(FunctionSymbol::lg))),
            "ln" => Some(Symbol::Operator(OperatorSymbol::Function(FunctionSymbol::ln))),
            "arctan" => Some(Symbol::Operator(OperatorSymbol::Function(FunctionSymbol::arctan))),
            "cot" => Some(Symbol::Operator(OperatorSymbol::Function(FunctionSymbol::cot))),
            "log" => Some(Symbol::Operator(OperatorSymbol::Function(FunctionSymbol::log))),
            "sec" => Some(Symbol::Operator(OperatorSymbol::Function(FunctionSymbol::sec))),
            "tan" => Some(Symbol::Operator(OperatorSymbol::Function(FunctionSymbol::tan))),
            "coth" => Some(Symbol::Operator(OperatorSymbol::Function(FunctionSymbol::coth))),
            "sin" => Some(Symbol::Operator(OperatorSymbol::Function(FunctionSymbol::sin))),
            "tanh" => Some(Symbol::Operator(OperatorSymbol::Function(FunctionSymbol::tanh))),
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
mod tests;
