#![feature(box_syntax, box_patterns, slice_patterns, advanced_slice_patterns, try_from)]
#[macro_use]
extern crate nom;
mod parser;
mod output;
mod symbols;
mod iter;

pub use output::{LatexWriter,KhwarizmiOutput};

pub use symbols::{Symbol, StandaloneSymbol, OperatorSymbol, FunctionSymbol};

pub use iter::{ChildIter, ExpressionIter};

use std::ops::Deref;
use std::convert::TryFrom;
use std::str::FromStr;
use std::{cmp, fmt, mem};


use parser::ParseError;

const NULL_EXPRESSION: Expression = Expression::Atom(Atom::Natural(0));
const NULL_SLICE: &'static [TreeInt] = &[];
const UNREACH: &'static str = "An option/result that was expected to be Some/Ok was not.\n\
                               This is a bug!";
const PLACEHOLDER: &'static str = "@@Placeholder@@";

#[derive(Debug,PartialEq, Eq)]
pub struct AlgebraDSLError {
    err: ErrorVariant,
    msg: String,
}

#[derive(Debug,PartialEq, Eq)]
pub enum ErrorVariant {
    Parse(ParseError),
    InvalidIdx,
    IllFormattedIndex,
    IllFormattedCommand,
    InvalidDelete,
    InvalidMake,
    InvalidCancel,
    NotEnoughNestedExponents,
    InvalidSiblingIndices,
    MapExpression,
    NeedsExpression,
    NeedsEquation,
    UnrecognizedCmd,
    InternalError,
    Unimplemented,
}

impl AlgebraDSLError {
    pub fn from_variant(var: ErrorVariant) -> Self {
        AlgebraDSLError::new(var, String::new())
    }
    pub fn new(var: ErrorVariant, msg: String) -> Self {
        AlgebraDSLError {
            err: var,
            msg: msg,
        }
    }
}

impl fmt::Display for AlgebraDSLError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Error: {:?}\n", self.err)?;
        for l in self.msg.split('\n') {
            write!(f, "\t{}\n", l)?;
        }
        Ok(())
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Atom {
    PlainVariable(char),
    Escaped(String),
    Natural(i64),
    Floating(f64),
    Symbol(Symbol),
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

#[derive(Debug, PartialEq, Clone)]
pub enum Math {
    Eq(Equation),
    Ex(Expression),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum MathRef<'a> {
    Eq(&'a Equation),
    Ex(&'a Expression),
}

#[derive(PartialEq, Debug, Clone)]
pub struct Equation {
    left: Expression,
    right: Expression,
}

type TreeInt = usize;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct TreeIdx(Vec<TreeInt>);

#[derive(PartialEq, Debug, Clone)]
pub struct SiblingIndices {
    parent_idx: TreeIdx,
    // This list *must* be sorted. This is enforced by the constructor
    children: Vec<TreeInt>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct TreeIdxSlice {
    inner: [TreeInt],
}

fn idx_fmt_err(s: String) -> AlgebraDSLError {
    AlgebraDSLError {
        err: ErrorVariant::IllFormattedIndex,
        msg: s,
    }
}

impl<'a> fmt::Display for &'a TreeIdxSlice {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#(mtn:0")?;
        for i in &self.inner {
            write!(f, ",{}", i)?;
        }
        write!(f, ")")
    }
}

impl fmt::Display for TreeIdx {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

impl TreeIdx {
    pub fn from_str(s: &str) -> Result<Self, AlgebraDSLError> {
        if !(s.starts_with("#(mtn:") && s.ends_with(")")) {
            Err(idx_fmt_err(format!("Index\n\t{}\nmust start with `#(mtn:` and end with `)`", s)))
        } else {
            let inner = &s[6..s.len() - 1];
            let mut idxs = inner.split(',').map(|d| {
                                                    usize::from_str(d).map_err(|_| {
                        idx_fmt_err(format!("Index\n\t{}\nmust contain comma-delimited numbers", s))
                    })
                                                });
            // Pop the first index, because every expression/equation is in the trivial 0 idx
            if Some(Ok(0)) != idxs.next() {
                return Err(idx_fmt_err(format!("Index\n\t{}\nmust start with a 0", s)));
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

impl Deref for TreeIdx {
    type Target = TreeIdxSlice;
    fn deref(&self) -> &Self::Target {
        TreeIdxSlice::from_slice(self.0.as_slice())
    }
}

impl AsRef<TreeIdxSlice> for TreeIdx {
    fn as_ref(&self) -> &TreeIdxSlice {
        self.deref()
    }
}

impl AsRef<TreeIdxSlice> for TreeIdxSlice {
    fn as_ref(&self) -> &TreeIdxSlice {
        self
    }
}

impl TreeIdxSlice {
    pub fn from_slice(slice: &[TreeInt]) -> &Self {
        // Safe because the layouts are the same.
        unsafe { mem::transmute(slice) }
    }
    pub fn first(&self) -> Option<TreeInt> {
        if self.len() > 0 {
            Some(self.inner[0])
        } else {
            None
        }
    }
    pub fn rest(&self) -> &TreeIdxSlice {
        if self.len() > 0 {
            TreeIdxSlice::from_slice(&self.inner[1..])
        } else {
            TreeIdxSlice::from_slice(NULL_SLICE)
        }
    }
    pub fn parent(&self) -> Option<&TreeIdxSlice> {
        if self.len() > 0 {
            Some(TreeIdxSlice::from_slice(&self.inner[..self.len() - 1]))
        } else {
            None
        }
    }
    pub fn grandparent(&self) -> Option<&TreeIdxSlice> {
        if self.len() > 1 {
            Some(TreeIdxSlice::from_slice(&self.inner[..self.len() - 2]))
        } else {
            None
        }
    }
    pub fn last(&self) -> Option<usize> {
        if self.len() > 0 {
            Some(self.inner[self.len() - 1])
        } else {
            None
        }
    }
    pub fn prefix(&self) -> Option<&TreeIdxSlice> {
        if self.len() > 0 {
            Some(TreeIdxSlice::from_slice(&self.inner[0..(self.len() - 1)]))
        } else {
            None
        }
    }
    #[inline]
    pub fn len(&self) -> usize {
        self.inner.len()
    }
    pub fn tail_from(&self, n: usize) -> Option<&TreeIdxSlice> {
        if n <= self.len() {
            Some(TreeIdxSlice::from_slice(&self.inner[n..]))
        } else {
            None
        }
    }
    pub fn to_owned(&self) -> TreeIdx {
        TreeIdx(self.inner
                    .iter()
                    .cloned()
                    .collect())
    }
    /// Returns whether `self` refers to a location in `other` *or* vis-versa
    pub fn nested(&self, other: &TreeIdxSlice) -> bool {
        let min_len = cmp::min(self.len(), other.len());
        self.inner
            .iter()
            .zip(other.inner.iter())
            .filter(|&(a, b)| a == b)
            .count() >= min_len
    }

    /// Returns whether `self` contains (or equals) `other`
    pub fn contains(&self, other: &TreeIdxSlice) -> bool {
        return self.nested(other) && self.len() <= other.len();
    }

    /// Returns whether `self` is the child of `other`
    pub fn is_child_of(&self, other: &TreeIdxSlice) -> bool {
        other.contains(self) && self.len() == other.len() + 1
    }
}

impl Equation {
    pub fn expr_iter(&self) -> ExpressionIter {
        ExpressionIter::new(MathRef::Eq(self))
    }
    pub fn from_str(eq: &str) -> Result<Self, AlgebraDSLError> {
        parser::parse_equation(eq)
            .map_err(ErrorVariant::Parse)
            .map_err(AlgebraDSLError::from_variant)
    }
    pub fn simplify_constants(self) -> Self {
        Equation {
            left: self.left.simplify_constants(),
            right: self.right.simplify_constants(),
        }
    }
    pub fn flip(self) -> Self {
        Equation {
            left: self.right,
            right: self.left,
        }
    }
    pub fn map(self, template: Expression) -> Result<Self, AlgebraDSLError> {
        let Equation { left, right } = self;
        Ok(Equation {
            left: left.map(template.clone())?,
            right: right.map(template)?,
        })
    }
}

fn bad_idx_err(s: String) -> AlgebraDSLError {
    AlgebraDSLError {
        err: ErrorVariant::InvalidIdx,
        msg: s,
    }
}

impl Indexable for Equation {
    fn get_opt<Idx: AsRef<TreeIdxSlice> + ?Sized>(&self, index: &Idx) -> Option<&Expression> {
        match index.as_ref().first() {
            Some(0) => self.left.get_opt(index.as_ref().rest()),
            Some(1) => self.right.get_opt(index.as_ref().rest()),
            _ => None,
        }
    }
    fn get_mut_opt<Idx: AsRef<TreeIdxSlice> + ?Sized>(&mut self,
                                                      index: &Idx)
                                                      -> Option<&mut Expression> {
        match index.as_ref().first() {
            Some(0) => self.left.get_mut_opt(index.as_ref().rest()),
            Some(1) => self.right.get_mut_opt(index.as_ref().rest()),
            _ => None,
        }
    }
}

impl SiblingIndices {
    fn parent(&self) -> &TreeIdxSlice {
        self.parent_idx.as_ref()
    }
}

pub trait Indexable: fmt::Display + fmt::Debug + Clone + KhwarizmiOutput {
    fn get<Idx: AsRef<TreeIdxSlice> + ?Sized>(&self,
                                              index: &Idx)
                                              -> Result<&Expression, AlgebraDSLError> {
        let err = bad_idx_err(format!("Cannot index `{}` by `{}`", self.as_khwarizmi_latex(), index.as_ref()));
        self.get_opt(index).ok_or(err)
    }
    fn get_mut<Idx: AsRef<TreeIdxSlice> + ?Sized>(&mut self,
                                                  index: &Idx)
                                                  -> Result<&mut Expression, AlgebraDSLError> {
        let err = bad_idx_err(format!("Cannot index `{}` by `{}`", self.as_khwarizmi_latex(), index.as_ref()));
        self.get_mut_opt(index).ok_or(err)
    }
    fn get_opt<Idx: AsRef<TreeIdxSlice> + ?Sized>(&self, index: &Idx) -> Option<&Expression>;
    fn get_mut_opt<Idx: AsRef<TreeIdxSlice> + ?Sized>(&mut self,
                                                      index: &Idx)
                                                      -> Option<&mut Expression>;
    /// Find location `index` in `self` and replace it with `expr`
    fn make(&mut self, index: &TreeIdx, expr: Expression) -> Result<(), AlgebraDSLError> {
        self.replace(index, expr)?;
        index.as_ref().parent().map(|idx| self.flatten(idx));
        Ok(())
    }

    /// Replace each location indicated by one of`indices` in `self` with `expr`
    fn replace_all<'a, Idx, Iter>(&mut self,
                                  indices: Iter,
                                  expr: Expression)
                                  -> Result<(), AlgebraDSLError>
        where Idx: AsRef<TreeIdxSlice> + ?Sized + 'a,
              Iter: Iterator<Item = &'a Idx>
    {
        let idxs: Vec<_> = indices.collect();
        for idx in idxs.iter() {
            self.replace(idx, expr.clone())?;
        }
        self.flatten_many(idxs.iter()).ok(); // Ignore Error
        Ok(())
    }

    fn replace<Idx: AsRef<TreeIdxSlice> + ?Sized>(&mut self,
                                                  index: &Idx,
                                                  expr: Expression)
                                                  -> Result<Expression, AlgebraDSLError> {
        let old = {
            let subtree = self.get_mut(index.as_ref())?;
            let old = subtree.take();
            *subtree = expr;
            old
        };
        Ok(old)
    }

    /// Delete the specified indices
    fn delete(&mut self, indices: SiblingIndices) -> Result<(), AlgebraDSLError> {
        let cp = self.clone();
        let res = self.replace_with_inner(&indices, None, ErrorVariant::InvalidDelete);
        if let Err(_) = res {
            *self = cp;
        }
        res
    }

    /// May eat the structure on error
    ///
    /// If `expr` is present, puts it in the first location indicated by `indices`. If not, it just
    /// removes the locations specified by indices.
    ///
    /// Does not do any flattening.
    fn replace_with_inner(&mut self,
                          indices: &SiblingIndices,
                          expr: Option<Expression>,
                          gen_err: ErrorVariant)
                          -> Result<(), AlgebraDSLError> {
        use Expression as Ex;
        let &SiblingIndices { ref parent_idx, ref children } = indices;
        let parent_ref = self.get_mut(parent_idx.as_ref())?;
        let insert_idx =
            *children.get(0)
                 .ok_or(AlgebraDSLError::from_variant(ErrorVariant::InvalidSiblingIndices))?;
        let result = match parent_ref.take() {
            Ex::Sum(args) => {
                if args.len() < children.len() {
                    return Err(AlgebraDSLError::new(gen_err,
                        format!("Can't get rid of more things than we have.")));
                }
                let mut new_exprs = delete_prod(args, children.as_slice());
                expr.map(|e| new_exprs.insert(insert_idx, e));
                Ex::sum_many(new_exprs).ok_or(AlgebraDSLError::new(gen_err,
                                                                   format!("Empty sum is illegal")))
            }
            Ex::Division(top, bottom) => {
                let original_top_len = top.len();
                let bottom_indices = children.iter()
                    .cloned()
                    .filter_map(|i| if i >= top.len() && i < top.len() + bottom.len() {
                                    Some(i - top.len())
                                } else {
                                    None
                                })
                    .collect::<Vec<_>>();
                let top_indices = children.iter()
                    .cloned()
                    .filter_map(|i| if i < top.len() { Some(i) } else { None })
                    .collect::<Vec<_>>();
                let mut new_top = delete_prod(top, top_indices.as_slice());
                let mut new_bottom = delete_prod(bottom, bottom_indices.as_slice());

                expr.map(|e| if insert_idx < original_top_len {
                             new_top.insert(insert_idx, e);
                         } else {
                             new_bottom.insert(insert_idx - original_top_len, e);
                         });
                Ok(Ex::divide_products(new_top, new_bottom))
            }
            e => {
                let s = format!("Can't refer to sibling children of the expression `{}`",
                                e.as_khwarizmi_latex());
                Err(AlgebraDSLError::new(gen_err, s))
            }
        };
        result.map(|e| { *parent_ref = e; })
    }

    fn make_siblings(&mut self,
                     indices: SiblingIndices,
                     expr: Expression)
                     -> Result<(), AlgebraDSLError> {
        let cp = self.clone();
        let res = self.replace_with_inner(&indices, Some(expr), ErrorVariant::InvalidMake);
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
    fn flatten(&mut self, idx: &TreeIdxSlice) -> Result<(), AlgebraDSLError> {
        let expr = self.get_mut(idx)?;
        let replacement = match expr.take() {
            Expression::Sum(summands) => {
                Expression::Sum(summands.into_iter()
                                    .flat_map(|e| match e {
                                                  Expression::Sum(more_summands) => more_summands,
                                                  e => vec![e],
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
                Expression::divide_products_flatten(new_top, new_bottom)
            }
            e => e,
        };
        *expr = replacement;
        Ok(())
    }

    /// Flattens many indices, being sure to not invalidate any in the intermediate period
    fn flatten_many<'a, Idx, Iter>(&mut self, indices: Iter) -> Result<(), AlgebraDSLError>
        where Idx: AsRef<TreeIdxSlice> + ?Sized + 'a,
              Iter: Iterator<Item = &'a Idx>
    {
        let mut indices = indices.filter_map(|idx| idx.as_ref().parent()).collect::<Vec<_>>();
        indices.sort();
        indices.dedup();
        indices.sort_by_key(|idx| idx.len());
        for idx in indices.iter().rev() {
            self.flatten(idx)?;
        }
        Ok(())
    }

    /// Collapses a tower of exponents
    /// (x^y)^z => x^(yz)
    /// Does this process `howmany` times.
    fn collapse(&mut self, index: &TreeIdx, howmany: usize) -> Result<(), AlgebraDSLError> {
        let mut tree = self.get(index.as_ref())?.clone();
        for _ in 0..howmany {
            tree = match tree {
                Expression::Power(box big_base, box outer_exp) => {
                    match big_base {
                        Expression::Power(inner_base, box inner_exp) => {
                            let newexp = inner_exp.inflate_multiplication_symmetric(outer_exp);
                            Expression::Power(inner_base, box newexp)
                        }
                        _ => return Err(AlgebraDSLError::from_variant(
                                ErrorVariant::NotEnoughNestedExponents)),
                    }
                }
                _ => return Err(AlgebraDSLError::from_variant(
                        ErrorVariant::NotEnoughNestedExponents)),
            }
        }
        self.replace(index, tree).map(|_| ())
    }

    /// If all the input indices share a parent, constructs a sibling index representing all of
    /// them
    fn sibling_indices(&self, indices: &[TreeIdx]) -> Result<SiblingIndices, AlgebraDSLError> {
        fn err_sib_idx(s: String) -> AlgebraDSLError {
            AlgebraDSLError::new(ErrorVariant::InvalidSiblingIndices, s)
        }
        // Takes in a bunch of indices and combines common stems.
        // If there are no indices, returns `None`.
        // If there is only one, the list of siblings will be empty
        fn stem(indices: &[TreeIdx]) -> Result<(TreeIdx, Vec<TreeIdx>), AlgebraDSLError> {
            let mut v = vec![];
            let mut i = 0;
            if indices.len() == 0 {
                return Err(err_sib_idx(format!("Can't make sibling indices from an empty list \
                                                of indices")));
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
            let tails: Vec<_> = indices.iter()
                .filter_map(|idx: &TreeIdx| idx.as_ref().tail_from(i).map(|idx| idx.to_owned()))
                .filter(|idx| idx.len() > 0)
                .collect();
            // If any of the above "tail_from" calls were out of bounds, then the tails list will
            // be shorter
            Ok((TreeIdx(v), tails))
        }

        let (mut trunk, mut branches) = stem(indices)?;

        // Make sure there is at least one branch, pushing back the trunk if needed
        if branches.len() == 0 {
            // If there is only one index, push the trunk up one.
            match trunk.pop() {
                Some(last) => branches.push(TreeIdx(vec![last])),
                None => {
                    return Err(AlgebraDSLError::from_variant(ErrorVariant::InvalidSiblingIndices))
                }
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
                if branches.iter().any(|idx| if idx.as_ref().len() != 1 {
                                           if let Some(&Expression::Negation(_)) =
                        idx.as_ref().first().and_then(|i| args.get(i)) {
                                               idx.as_ref().len() > 2
                                           } else {
                                               true
                                           }
                                       } else {
                                           false
                                       }) {
                    return Err(err_sib_idx(format!("Sibling indices must differ only in last \
                                                    expression. Actual differences: {:?}",
                                                   branches)));
                }
            }
            _ => {
                if branches.iter().any(|idx| idx.as_ref().len() > 1) {
                    return Err(err_sib_idx(format!("Sibling indices must differ only in last \
                                                    expression. Actual differences: {:?}",
                                                   branches)));
                }
            }
        }

        Ok(SiblingIndices {
               parent_idx: trunk,
               children: single_indices,
           })
    }
    fn swap(&mut self, a: &TreeIdx, b: &TreeIdx) -> Result<(), AlgebraDSLError> {
        // Detect errors early and fail. Necessary because we don't have a concept of disjoint
        // borrows, so we can't mutably borrow a and b simultaneously
        let nested = a.as_ref().nested(b.as_ref());
        if nested {
            Err(bad_idx_err(format!("Indices `{}` and `{}` are nested", a, b)))
        } else if let Err(e) = self.get_mut(a.as_ref()) {
            Err(e)
        } else if let Err(e) = self.get_mut(b.as_ref()) {
            Err(e)
        } else {
            let tmp_a = self.get_mut(a.as_ref()).expect(UNREACH).take();
            let tmp_b = self.get_mut(b.as_ref()).expect(UNREACH).take();
            *self.get_mut(a.as_ref()).expect(UNREACH) = tmp_b;
            *self.get_mut(b.as_ref()).expect(UNREACH) = tmp_a;
            Ok(())
        }
    }
    fn cancel_inverse(&mut self, indices: SiblingIndices) -> Result<(), AlgebraDSLError> {
        //check whether the sibling indices refer to a valid pair of inverses
        let should_delete;
        {
            let &SiblingIndices { ref parent_idx, ref children } = &indices;
            if children.len() != 2 {
                //Cancel 2 at a time
                return Err(AlgebraDSLError::from_variant(ErrorVariant::InvalidCancel));
            }
            let mut child0_idx = parent_idx.clone();
            child0_idx.push(*children.get(0)
                        .ok_or(AlgebraDSLError::from_variant(ErrorVariant::InternalError))?);
            let mut child1_idx = parent_idx.clone();
            child1_idx.push(*children.get(1)
                        .ok_or(AlgebraDSLError::from_variant(ErrorVariant::InternalError))?);
            should_delete = self.are_inverses(&parent_idx, &child0_idx, &child1_idx)?;
        }
        //then delete if it is a valid pair
        if should_delete {
            self.delete(indices)
        } else {
            Err(AlgebraDSLError::from_variant(ErrorVariant::InvalidCancel))
        }
    }
    fn are_inverses(&self,
                    parent: &TreeIdx,
                    child0: &TreeIdx,
                    child1: &TreeIdx)
                    -> Result<bool, AlgebraDSLError> {
        let parent_ref = self.get(parent.as_ref())?;
        let child0_ref = self.get(child0.as_ref())?;
        let child1_ref = self.get(child1.as_ref())?;
        match parent_ref {
            &Expression::Sum(_) => {
                if let &Expression::Negation(ref child0_inner) = child0_ref {
                    if **child0_inner == *child1_ref {
                        return Ok(true);
                    }
                }
                if let &Expression::Negation(ref child1_inner) = child1_ref {
                    if **child1_inner == *child0_ref {
                        return Ok(true);
                    }
                }
            }
            &Expression::Division(ref top, _) => {
                if child0.last() < Some(top.len()) && child1.last() >= Some(top.len()) &&
                   *child0_ref == *child1_ref {
                    return Ok(true);
                }
            }
            _ => return Ok(false),
        }
        Ok(false)
    }
    fn cancel_in_sum(&mut self,
                     mut args: Vec<Expression>,
                     idx: &TreeIdx)
                     -> Result<(), AlgebraDSLError> {
        //if we're down to only one thing, it doesn't run anyway.
        for i in 1..args.len() {
            for j in 0..i {
                //if expr i and expr j are the same, but with negative sign, delete them
                if let Expression::Negation(ref inner_i) = args[i].clone() {
                    if **inner_i == args[j] {
                        args.remove(i);
                        args.remove(j); //i must come after j
                        let mut index_i = idx.clone();
                        index_i.push(i);
                        let mut index_j = idx.clone();
                        index_j.push(j);
                        let sibs = self.sibling_indices(vec![index_i, index_j].as_slice())?;
                        self.delete(sibs)?;
                        return self.cancel_in_sum(args, idx);
                    }
                }
                if let Expression::Negation(ref inner_j) = args[j].clone() {
                    if **inner_j == args[i] {
                        args.remove(i);
                        args.remove(j);
                        let mut index_i = idx.clone();
                        index_i.push(i);
                        let mut index_j = idx.clone();
                        index_j.push(j);
                        let sibs = self.sibling_indices(vec![index_i, index_j].as_slice())?;
                        self.delete(sibs)?;
                        return self.cancel_in_sum(args, idx);
                    }
                }
            }
        }
        Ok(())
    }
    fn cancel_in_div(&mut self,
                     mut top: Vec<Expression>,
                     mut bot: Vec<Expression>,
                     idx: &TreeIdx)
                     -> Result<(), AlgebraDSLError> {
        for t in 0..top.len() {
            for b in 0..bot.len() {
                if top[t] == bot[b] {
                    let mut index_t = idx.clone();
                    index_t.push(t);
                    let mut index_b = idx.clone();
                    index_b.push(b + top.len());
                    let sibs = self.sibling_indices(vec![index_t, index_b].as_slice())?;
                    top.remove(t);
                    bot.remove(b);
                    self.delete(sibs)?;
                    return self.cancel_in_div(top, bot, idx);
                }
            }
        }
        Ok(())
    }
    fn cancel_assoc(&mut self, idx: &TreeIdx) -> Result<(), AlgebraDSLError> {
        //Cancels everything in an entire associative structure
        let expr = self.get(idx.as_ref())?.clone();
        match expr {
            Expression::Sum(args) => self.cancel_in_sum(args, idx),
            Expression::Division(top, bot) => self.cancel_in_div(top, bot, idx),
            _ => Err(AlgebraDSLError::from_variant(ErrorVariant::InvalidIdx)),
        }
    }
    fn simplify(&mut self, idx: &TreeIdx) -> Result<(), AlgebraDSLError> {
        //Attempts to simplify a given location in the tree, deterministically
        match self.get(idx.as_ref())? {//expr {
            &Expression::Sum(_) => self.cancel_assoc(idx),
            &Expression::Division(_, _) => self.cancel_assoc(idx),
            &Expression::Power(box Expression::Power(_, _), _) => self.collapse(idx, 1),
            _ => Ok(()),
        }
    }

    fn distribute_power(mut self, whole: &TreeIdx) -> Result<Self, AlgebraDSLError> {
        {
            let location = self.get_mut(whole)?;
            let contents = location.take();
            *location = match contents {
                Expression::Power(box base, box power) => {
                    if let Expression::Division(top, bot) = base {
                        Expression::Division(
                            top.into_iter().map(|t| t.inflate_power(power.clone())).collect(),
                            bot.into_iter().map(|t| t.inflate_power(power.clone())).collect()
                        )
                    } else {
                        return Err(AlgebraDSLError::new(ErrorVariant::InvalidIdx,
                             format!("The index `{}` does not refer to a power \
                                      with a product in it.", whole)));
                    }
                }
                _ => {
                    return Err(AlgebraDSLError::new(ErrorVariant::InvalidIdx,
                         format!("The index `{}` does not refer to a power.", whole)));
                }
            };
        }
        Ok(self)
    }

    /// Takes two indices refering to siblings of a product `term` and `sum`.
    /// Assuming that `sum` refers to a summation, it distrubtes term across the summands
    ///
    /// Left multiplies or right multiplies according to the relative positions of the original
    /// referents.
    fn distribute(mut self, term: &TreeIdx, sum: &TreeIdx) -> Result<Self, AlgebraDSLError> {
        let parent = term.parent().ok_or(AlgebraDSLError::from_variant(ErrorVariant::InvalidIdx))?;
        if !sum.is_child_of(parent) {
            println!("{}, {}, {}", term, sum, parent);
            return Err(AlgebraDSLError::new(ErrorVariant::InvalidIdx,
            format!("Indices {} and {} are not siblings, so can't be distributed.", sum, term)));
        }
        {
            let location = self.get_mut(parent)?;
            let mut contents = location.take();
            let term_idx = term.last().ok_or(AlgebraDSLError::from_variant(ErrorVariant::InvalidIdx))?;
            let sum_idx = sum.last().ok_or(AlgebraDSLError::from_variant(ErrorVariant::InvalidIdx))?;
            let left_multiply = term_idx < sum_idx;
            match &mut contents {
                &mut Expression::Division(ref mut top, _) => {
                    if term_idx < top.len() && sum_idx < top.len() {
                        let term_expr = top[term_idx].clone();
                        match &mut top[sum_idx] {
                            &mut Expression::Sum(ref mut summands) => {
                                // Multiply each summand by the term
                                for summand in summands.iter_mut() {
                                    let contents = summand.take();
                                    *summand = if left_multiply {
                                        Expression::multiply(term_expr.clone(), contents)
                                    } else {
                                        Expression::multiply(contents, term_expr.clone())
                                    };
                                }
                            }
                            _ => {
                                return Err(AlgebraDSLError::new(ErrorVariant::InvalidIdx,
                                                                format!("Index {} does not refer to a sum, so cannot distribute",
                                                                        sum)));
                            }
                        }
                        top.remove(term_idx);
                    }
                    else {
                        return Err(AlgebraDSLError::new(ErrorVariant::InvalidIdx,
                                                        format!("Index {} does not refer to a product, so cannot distribute",
                                                                sum)));
                    }
                }
                _ => {
                    return Err(AlgebraDSLError::new(ErrorVariant::InvalidIdx,
                                                    format!("Index {} does not refer to a product, so cannot distribute",
                                                            sum)));
                }
            }
            *location = match contents {
                Expression::Division(mut top, bot) => {
                    if top.len() == 1 && bot.len() == 0 {
                        top.pop().unwrap()
                    } else {
                        Expression::Division(top, bot)
                    }
                }
                e => e,
            };
        }
        Ok(self)
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

impl FromStr for Expression {
    type Err = AlgebraDSLError;
    fn from_str(expr: &str) -> Result<Self, Self::Err> {
        parser::parse_expr(expr).map_err(ErrorVariant::Parse).map_err(AlgebraDSLError::from_variant)
    }
}

impl Expression {
    pub fn take(&mut self) -> Self {
        mem::replace(self, NULL_EXPRESSION)
    }
    pub fn expr_iter(&self) -> ExpressionIter {
        ExpressionIter::new(MathRef::Ex(self))
    }
    pub fn map(self, mut template: Expression) -> Result<Self, AlgebraDSLError> {
        let mut idxs = Vec::new();
        let placeholder = Expression::Atom(Atom::Escaped(PLACEHOLDER.to_string()));
        for (idx, e) in template.expr_iter() {
            if e == &placeholder {
                idxs.push(idx);
            }
        }
        template.replace_all(idxs.iter(), self)?;
        Ok(template)
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
    /// Multiplies self by expr. If either self or other are themselves Divisions, combine them
    pub fn inflate_multiplication_symmetric(self, other: Expression) -> Self {
        match self {
            Expression::Division(mut mytop, mut mybot) => {
                match other {
                    Expression::Division(mut otop, mut obot) => {
                        mytop.append(&mut otop);
                        mybot.append(&mut obot);
                    }
                    _ => mytop.push(other),
                }
                Expression::Division(mytop, mybot)
            }
            _ => {
                match other {
                    Expression::Division(mut otop, obot) => {
                        // Put self at the front
                        otop.insert(0, self);
                        Expression::Division(otop, obot)
                    }
                    _ => Expression::Division(vec![self, other], vec![]),
                }
            }
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
    pub fn inflate_power(self, expr: Expression) -> Self {
        Expression::Power(box self, box expr)
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
                Ex::divide_products_flatten(top_exprs, bottom_exprs)
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
    fn divide_products(top: Vec<Expression>, bottom: Vec<Expression>) -> Expression {
        let one = Expression::Atom(Atom::Natural(1));
        match (top.len(), bottom.len()) {
            (0, 0) => one,
            (0, _) => Expression::Division(vec![one], bottom),
            (_, _) => Expression::Division(top, bottom),
        }
    }

    /// Creates an `Expression` with `top`s divided by `bottom`s
    fn divide_products_flatten(mut top: Vec<Expression>, bottom: Vec<Expression>) -> Expression {
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
        let zero = Expression::Atom(Atom::Natural(0));
        match summands.len() {
            0 => Some(zero),
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
    fn get_opt<Idx: AsRef<TreeIdxSlice> + ?Sized>(&self, index: &Idx) -> Option<&Expression> {
        match index.as_ref().first() {
            None => Some(self),
            Some(first) => {
                let rest = index.as_ref().rest();
                match self {
                    &Expression::Negation(ref e) if first == 0 => e.get_opt(rest),
                    &Expression::Sum(ref e) if first < e.len() => e[first].get_opt(rest),
                    &Expression::Division(ref t, ref b) if first < t.len() + b.len() => {
                        if first < t.len() {
                            t[first].get_opt(rest)
                        } else {
                            b[first - t.len()].get_opt(rest)
                        }
                    }
                    &Expression::Power(ref base, _) if first == 0 => base.get_opt(rest),
                    &Expression::Power(_, ref power) if first == 1 => power.get_opt(rest),
                    &Expression::Subscript(ref base, _) if first == 0 => base.get_opt(rest),
                    &Expression::Subscript(_, ref script) if first == 1 => script.get_opt(rest),
                    &Expression::Application(ref func, _) if first == 0 => func.get_opt(rest),
                    &Expression::Application(_, ref arg) if first == 1 => arg.get_opt(rest),
                    &Expression::LimitOp(_, ref sub, _, _) if first == 0 => {
                        sub.as_ref().and_then(|s| s.get_opt(rest))
                    }
                    &Expression::LimitOp(_, _, ref sup, _) if first == 1 => {
                        sup.as_ref().and_then(|s| s.get_opt(rest))
                    }
                    &Expression::LimitOp(_, _, _, ref exp) if first == 2 => exp.get_opt(rest),
                    _ => None,
                }
            }
        }
    }
    fn get_mut_opt<Idx: AsRef<TreeIdxSlice> + ?Sized>(&mut self,
                                                      index: &Idx)
                                                      -> Option<&mut Expression> {
        match index.as_ref().first() {
            None => Some(self),
            Some(first) => {
                let rest = index.as_ref().rest();
                match self {
                    &mut Expression::Negation(ref mut e) if first == 0 => e.get_mut_opt(rest),
                    &mut Expression::Sum(ref mut e) if first < e.len() => {
                        e[first].get_mut_opt(rest)
                    }
                    &mut Expression::Division(ref mut t, ref mut b) if first <
                                                                       t.len() + b.len() => {
                        if first < t.len() {
                            t[first].get_mut_opt(rest)
                        } else {
                            b[first - t.len()].get_mut_opt(rest)
                        }
                    }
                    &mut Expression::Power(ref mut base, _) if first == 0 => base.get_mut_opt(rest),
                    &mut Expression::Power(_, ref mut power) if first == 1 => {
                        power.get_mut_opt(rest)
                    }
                    &mut Expression::Subscript(ref mut base, _) if first == 0 => {
                        base.get_mut_opt(rest)
                    }
                    &mut Expression::Subscript(_, ref mut pow) if first == 1 => {
                        pow.get_mut_opt(rest)
                    }
                    &mut Expression::Application(ref mut func, _) if first == 0 => {
                        func.get_mut_opt(rest)
                    }
                    &mut Expression::Application(_, ref mut arg) if first == 1 => {
                        arg.get_mut_opt(rest)
                    }
                    &mut Expression::LimitOp(_, ref mut sub, _, _) if first == 0 => {
                        sub.as_mut().and_then(|s| s.get_mut_opt(rest))
                    }
                    &mut Expression::LimitOp(_, _, ref mut sup, _) if first == 1 => {
                        sup.as_mut().and_then(|s| s.get_mut_opt(rest))
                    }
                    &mut Expression::LimitOp(_, _, _, ref mut exp) if first == 2 => {
                        exp.get_mut_opt(rest)
                    }
                    _ => None,
                }
            }
        }
    }
}

impl<'a> MathRef<'a> {
    pub fn expr_iter(self) -> ExpressionIter<'a> {
        ExpressionIter::new(self)
    }
}

impl Indexable for Math {
    fn get_opt<Idx: AsRef<TreeIdxSlice> + ?Sized>(&self, index: &Idx) -> Option<&Expression> {
        match self {
            &Math::Eq(ref eq) => eq.get_opt(index),
            &Math::Ex(ref ex) => ex.get_opt(index),
        }
    }
    fn get_mut_opt<Idx: AsRef<TreeIdxSlice> + ?Sized>(&mut self,
                                                      index: &Idx)
                                                      -> Option<&mut Expression> {
        match self {
            &mut Math::Eq(ref mut eq) => eq.get_mut_opt(index),
            &mut Math::Ex(ref mut ex) => ex.get_mut_opt(index),
        }
    }
}

impl Math {
    pub fn simplify_constants(self) -> Self {
        use self::Math::*;
        match self {
            Eq(eq) => Eq(eq.simplify_constants()),
            Ex(ex) => Ex(ex.simplify_constants()),
        }
    }
    pub fn from_str(s: &str) -> Result<Self, AlgebraDSLError> {
        if let Ok(eq) = Equation::from_str(s) {
            Ok(Math::Eq(eq))
        } else {
            Ok(Math::Ex(Expression::from_str(s)?))
        }
    }
    pub fn as_ref(&self) -> MathRef {
        match self {
            &Math::Ex(ref e) => MathRef::Ex(e),
            &Math::Eq(ref e) => MathRef::Eq(e),
        }
    }
    pub fn expr_iter(&self) -> ExpressionIter {
        self.as_ref().expr_iter()
    }
    pub fn map(self, template: Expression) -> Result<Self, AlgebraDSLError> {
        use self::Math::*;
        Ok(match self {
            Eq(eq) => Eq(eq.map(template)?),
            Ex(ex) => Ex(ex.map(template)?),
        })
    }
}

#[cfg(test)]
mod tests;
