use super::{Expression, MathRef, TreeIdx};

pub struct ChildIter<'a> {
    parent: MathRef<'a>,
    next_idx: usize,
}

impl<'a> ChildIter<'a> {
    pub fn new(parent: MathRef<'a>) -> Self {
        ChildIter {
            parent: parent,
            next_idx: 0,
        }
    }
}

impl<'a> Iterator for ChildIter<'a> {
    type Item = (usize, &'a Expression);
    fn next(&mut self) -> Option<Self::Item> {
        use Expression as Ex;
        // Generally we report the intial `next_idx` as the next one, but sometimes we don't
        // Namely, if a LimitOp is missing bounds.
        let mut override_idx = None;
        let idx = self.next_idx;
        let res = match self.parent {
            MathRef::Eq(eq) if idx == 0 => Some(&eq.left),
            MathRef::Eq(eq) if idx == 1 => Some(&eq.right),
            MathRef::Ex(&Ex::Negation(box ref e)) if idx == 0 => Some(e),
            MathRef::Ex(&Ex::Sum(ref es)) if idx < es.len() => Some(&es[idx]),
            MathRef::Ex(&Ex::Division(ref t, _)) if idx < t.len() => Some(&t[idx]),
            MathRef::Ex(&Ex::Division(ref t, ref b)) if idx - t.len() < b.len() => {
                Some(&b[idx - t.len()])
            }
            MathRef::Ex(&Ex::Power(box ref b, _)) if idx == 0 => Some(b),
            MathRef::Ex(&Ex::Power(_, box ref e)) if idx == 1 => Some(e),
            MathRef::Ex(&Ex::Subscript(box ref b, _)) if idx == 0 => Some(b),
            MathRef::Ex(&Ex::Subscript(_, box ref e)) if idx == 1 => Some(e),
            MathRef::Ex(&Ex::LimitOp(_, None, None, box ref arg)) if idx == 0 => {
                override_idx = Some(2);
                Some(arg)
            }
            MathRef::Ex(&Ex::LimitOp(_, None, Some(box ref e), _)) if idx == 0 => {
                override_idx = Some(1);
                Some(e)
            }
            MathRef::Ex(&Ex::LimitOp(_, Some(box ref e), _, _)) if idx == 0 => Some(e),
            MathRef::Ex(&Ex::LimitOp(_, _, None, box ref arg)) if idx == 1 => {
                override_idx = Some(2);
                Some(arg)
            }
            MathRef::Ex(&Ex::LimitOp(_, _, Some(box ref e), _)) if idx == 1 => Some(e),
            MathRef::Ex(&Ex::LimitOp(_, _, _, box ref arg)) if idx == 2 => Some(arg),
            _ => None,
        };
        let idx_and_res = res.map(|r| (override_idx.clone().unwrap_or(idx), r));
        override_idx.map(|i| {
            self.next_idx = i;
        });
        self.next_idx += 1;
        idx_and_res
    }
}

pub struct ExpressionIter<'a> {
    iter_stack: Vec<ChildIter<'a>>,
    idx_stack: TreeIdx,
    unemitted_root: Option<&'a Expression>,
}

impl<'a> ExpressionIter<'a> {
    pub fn new(e: MathRef<'a>) -> Self {
        ExpressionIter {
            iter_stack: vec![ChildIter::new(e)],
            idx_stack: TreeIdx::from_vec(vec![]),
            unemitted_root: match e {
                MathRef::Ex(ex) => Some(ex),
                MathRef::Eq(_) => None,
            },
        }
    }
}

impl<'a> Iterator for ExpressionIter<'a> {
    type Item = (TreeIdx, &'a Expression);
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(root) = self.unemitted_root.take() {
            return Some((TreeIdx::from_vec(vec![]), root));
        }
        loop {
            let new_child_iter = match self.iter_stack.last_mut() {
                None => return None,
                Some(last) => last
                    .next()
                    .map(|(i, e)| (i, ChildIter::new(MathRef::Ex(e)), e)),
            };
            match new_child_iter {
                None => {
                    self.iter_stack.pop();
                    self.idx_stack.pop();
                }
                Some((i, child_iter, e)) => {
                    self.iter_stack.push(child_iter);
                    self.idx_stack.push(i);
                    return Some((self.idx_stack.clone(), e));
                }
            }
        }
    }
}
