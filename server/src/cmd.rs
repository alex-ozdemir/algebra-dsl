use std::fs::OpenOptions;
use std::io::Write;
use std::str::FromStr;

use khwarizmi::{AlgebraDSLError as Error, ErrorVariant as Variant, Expression, Indexable,
                LatexWriter, Math, TreeIdx, KhwarizmiOutput};

#[derive(Debug, PartialEq, Clone)]
pub enum Return {
    Math(Math),
    LaTeXBlock(String),
    LaTeXLine(String),
    NoReturn,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Op {
    Plus,
    Times,
    Div,
    Minus,
    Power,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprOrIdx {
    Exp(Expression),
    Idx(TreeIdx),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Cmd {
    New(Math),
    Make(Vec<TreeIdx>, ExprOrIdx),
    Delete(Vec<TreeIdx>),
    Apply(Op, ExprOrIdx),
    FullApply(ExprOrIdx),
    Swap(TreeIdx, TreeIdx),
    Output(Vec<usize>),
    Recover(usize),
    GetCode(usize),
    Factor(TreeIdx, Expression),
    Feedback(String, String),
    Replace(Vec<TreeIdx>, ExprOrIdx),
    Collapse(TreeIdx, Option<usize>),
    Cancel(Vec<TreeIdx>),
    Distribute(Vec<TreeIdx>, TreeIdx),
    DistributePower(TreeIdx),
    Simplify(TreeIdx),
    Flatten(TreeIdx),
    Flip,
}

fn internal_err() -> Error {
    Error::new(Variant::InternalError,
               "There as been an internal error -- it's not your fault :).\nPlease let us know"
                   .to_string())
}

fn invalid_hist_idx_err(idx: usize) -> Error {
    Error::new(Variant::InvalidIdx,
               format!("No history entry with that index `{}`", idx))
}

impl ExprOrIdx {
    fn as_expr(self, last: Option<&Math>) -> Result<Expression, Error> {
        match (self, last) {
            (ExprOrIdx::Exp(e), _) => Ok(e),
            (ExprOrIdx::Idx(ref i), Some(ref l)) => l.get(&i).map(|x| x.clone()),
            _ => Err(Error::from_variant(Variant::NeedsExpression)),
        }
    }

    fn as_expr_ref<'a>(&'a self, last: Option<&'a Math>) -> Result<&'a Expression, Error> {
        match (self, last) {
            (&ExprOrIdx::Exp(ref e), _) => Ok(e),
            (&ExprOrIdx::Idx(ref i), Some(ref l)) => l.get(&i),
            _ => Err(Error::from_variant(Variant::NeedsExpression)),
        }
    }
}

impl FromStr for ExprOrIdx {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if !s.starts_with("*") {
                Err(Error::from_variant(Variant::InternalError))
            } else {
                TreeIdx::from_str(&s[1..].trim())
            }
            .map(|i| ExprOrIdx::Idx(i))
            .or_else(|_| Expression::from_str(s).map(|e| ExprOrIdx::Exp(e)))
    }
}

impl Cmd {
    pub fn execute(self, last: Option<&Math>, history: &Vec<Math>) -> Result<Return, Error> {
        match (self, last) {
            (Cmd::New(e), _) => Ok(Return::Math(e)),
            (Cmd::Make(indices, new_expr), Some(old_expr)) => {
                let mut expr = old_expr.clone();
                if indices.len() == 1 {
                    expr.make(&indices[0], new_expr.as_expr(last)?)?;
                } else {
                    let sibs = old_expr.sibling_indices(indices.as_slice())?;
                    println!("Make location {:?} into {:?}",
                             sibs,
                             new_expr.as_expr_ref(last)?);
                    expr.make_siblings(sibs, new_expr.as_expr(last)?)?;
                }
                Ok(Return::Math(expr))
            }
            (Cmd::Delete(indices), Some(old_expr)) => {
                let mut expr = old_expr.clone();
                let sibs = old_expr.sibling_indices(indices.as_slice())?;
                expr.delete(sibs)?;
                Ok(Return::Math(expr))
            }
            (Cmd::Factor(index, expr), Some(old_expr)) => {
                Ok(Return::Math(old_expr.clone().factor(&index, expr)?))
            }
            (Cmd::Cancel(indices), Some(old_expr)) => {
                let mut expr = old_expr.clone();
                let sibs = old_expr.sibling_indices(indices.as_slice())?;
                expr.cancel_inverse(sibs)?;
                Ok(Return::Math(expr))
            }
            (Cmd::Simplify(idx), Some(old_expr)) => {
                let mut expr = old_expr.clone();
                expr = expr.simplify(&idx)?;
                Ok(Return::Math(expr))
            }
            (Cmd::Swap(i1, i2), Some(old_expr)) => {
                let mut expr = old_expr.clone();
                expr.swap(&i1, &i2)?;
                Ok(Return::Math(expr))
            }
            (Cmd::Apply(op, new_expr), Some(e)) => {
                let map_string = match op {
                    Op::Times => format!("@ * ({})", new_expr.as_expr(last)?.as_khwarizmi_latex()),
                    Op::Plus => format!("@ + ({})", new_expr.as_expr(last)?.as_khwarizmi_latex()),
                    Op::Div => format!("@ / ({})", new_expr.as_expr(last)?.as_khwarizmi_latex()),
                    Op::Minus => format!("@ - ({})", new_expr.as_expr(last)?.as_khwarizmi_latex()),
                    Op::Power => format!("@ ^ ({})", new_expr.as_expr(last)?.as_khwarizmi_latex()),
                };
                let template = ExprOrIdx::from_str(&map_string).unwrap();
                Cmd::FullApply(template).execute(Some(e), history)
            }
            (Cmd::FullApply(template), Some(old_expr)) => {
                let expr = old_expr.clone();
                Ok(Return::Math(expr.map(template.as_expr(last)?)?))
            }
            (Cmd::Output(math_idxs_to_output), _) => {
                let mut latex_writer = LatexWriter::new();
                for idx in math_idxs_to_output {
                    latex_writer.add_math(history.get(idx)
                            .ok_or_else(|| invalid_hist_idx_err(idx))?)
                        .map_err(|_| internal_err())
                        .unwrap();
                }
                Ok(Return::LaTeXBlock(latex_writer.finish_str().map_err(|_| internal_err())?))
            }
            (Cmd::Recover(idx), e) => {
                println!("History is index {:?}: {:#?}", idx, history);
                let recover_math = history.get(idx)
                    .ok_or_else(|| invalid_hist_idx_err(idx))?;
                let latex_string = recover_math.as_khwarizmi_latex();
                let parsed = Math::from_str(latex_string.as_str().trim())?;
                Cmd::New(parsed).execute(e, history)
            }
            (Cmd::GetCode(idx), _) => {
                let recover_math = history.get(idx)
                    .ok_or_else(|| invalid_hist_idx_err(idx))?;
                let latex_string = recover_math.as_khwarizmi_latex();
                Ok(Return::LaTeXLine(latex_string))
            }
            (Cmd::Feedback(text, html), _) => {
                let mut f = OpenOptions::new().create(true)
                    .append(true)
                    .open(::REPORTFILE)
                    .map_err(|_| internal_err())?;
                write!(f,
                       "Feedback text:\n{}\nFeedback html:\n{}\n\n\n",
                       text,
                       html).map_err(|_| internal_err())?;
                Ok(Return::NoReturn)
            }
            (Cmd::Replace(indices, new_expr), Some(old_eqorexpr)) => {
                let mut eqorexpr = old_eqorexpr.clone();
                eqorexpr.replace_all(indices.iter(), new_expr.as_expr(last)?)?;
                Ok(Return::Math(eqorexpr))
            }
            (Cmd::Collapse(idx, howfar), Some(old)) => {
                let howfar = howfar.unwrap_or(1);

                let mut e = old.clone();

                e.collapse(&idx, howfar)?;

                Ok(Return::Math(e))
            }
            (Cmd::Flip, Some(&Math::Eq(ref eq))) => Ok(Return::Math(Math::Eq(eq.clone().flip()))),
            (Cmd::Flip, Some(_)) => {
                Err(Error::new(Variant::NeedsEquation,
                               "Flip only works on equations".to_string()))
            }
            (Cmd::Distribute(indices, last_index), Some(old_expr)) => {
                Ok(Return::Math(match indices.len() {
                    0 => old_expr.clone().distribute_power(&last_index)?,
                    1 => old_expr.clone().distribute(&indices[0], &last_index)?,
                    _ => {
                        old_expr.clone()
                            .distribute_many(&old_expr.sibling_indices(indices.as_slice())?,
                                             &last_index)?
                    }
                }))
            }
            (Cmd::DistributePower(i), Some(old_expr)) => {
                Ok(Return::Math(old_expr.clone().distribute_power(&i)?))
            }
            (Cmd::Flatten(i), Some(old_expr)) => {
                let mut cp = old_expr.clone();
                cp.flatten(&i)?;
                Ok(Return::Math(cp))
            }
            (c, None) => {
                Err(Error::new(Variant::NeedsExpression,
                               format!("The command `{:?}` needs an expression", c)))
            }
        }
    }
}

/// Parses any indices at the beginning of `s` and returns the indices and the rest of `s`
fn parse_indices(s: &str) -> Result<(Vec<TreeIdx>, &str), Error> {
    let mut indices = Vec::new();
    let mut rest = s.trim_left();
    while rest.starts_with("#") {
        let idx_end = rest.find(')')
            .ok_or_else(|| {
                Error::new(Variant::IllFormattedIndex,
                           format!("The index `{}` has an unclosed paren", s))
            })?;
        let idx = TreeIdx::from_str(&rest[..(idx_end + 1)])?;
        indices.push(idx);
        rest = &rest[(idx_end + 1)..].trim();
    }
    Ok((indices, rest))
}

impl FromStr for Cmd {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with("cmd@") {
            let s: &str = &s[4..].trim_left();
            if s.starts_with("make") {
                let (indices, rest) = parse_indices(&s[4..].trim())?;
                let expr = ExprOrIdx::from_str(rest.trim())?;
                Ok(Cmd::Make(indices, expr))
            } else if s.starts_with("delete") {
                let (indices, rest) = parse_indices(&s[6..].trim())?;
                if !rest.trim().is_empty() {
                    Err(Error::new(Variant::IllFormattedCommand,
                                   format!("Rest isn't empty, it's `{}`", rest)))
                } else {
                    Ok(Cmd::Delete(indices))
                }
            } else if s.starts_with("factor") {
                let (mut indices, rest) = parse_indices(&s[6..].trim())?;
                if indices.len() != 1 {
                    Err(Error::new(Variant::IllFormattedCommand,
                                   format!("Expected a single sum index for factor")))
                } else {
                    let expr = Expression::from_str(rest.trim())?;
                    Ok(Cmd::Factor(indices.pop().unwrap(), expr))
                }
            } else if s.starts_with("cancel") {
                let (indices, rest) = parse_indices(&s[6..].trim())?;
                if !rest.trim().is_empty() {
                    println!("Rest isn't empty, it's `{}`", rest);
                    Err(Error::from_variant(Variant::IllFormattedCommand))
                } else {
                    Ok(Cmd::Cancel(indices))
                }
            } else if s.starts_with("swap") {
                let (mut indices, rest) = parse_indices(&s[4..].trim())?;
                if indices.len() != 2 || rest.trim().len() > 0 {
                    Err(Error::new(Variant::IllFormattedCommand,
                                   "swap expects two indices".to_string()))
                } else {
                    let i2 = indices.pop().unwrap();
                    let i1 = indices.pop().unwrap();
                    Ok(Cmd::Swap(i1, i2))
                }
            } else if s.starts_with("simplify") {
                let (mut indices, rest) = parse_indices(&s[8..].trim())?;
                if indices.len() != 1 || rest.trim().len() > 0 {
                    Err(Error::from_variant(Variant::IllFormattedCommand))
                } else {
                    let idx = indices.pop().unwrap();
                    Ok(Cmd::Simplify(idx))
                }
            } else if s.starts_with("apply") {
                let template = ExprOrIdx::from_str(&s[5..].trim())?;
                Ok(Cmd::FullApply(template))
            } else if s.starts_with("output") {
                let mut indices = Vec::new();
                let mut rest: &str = &s[6..].trim();
                loop {
                    if let Some(comma_idx) = rest.find(',') {
                        indices.push(usize::from_str(&rest[..comma_idx]).map_err(|_| {
                                Error::new(Variant::IllFormattedCommand,
                                           "Output not given a list of numbers".to_string())
                            })?);
                        rest = rest[comma_idx + 1..].trim();
                    } else {
                        indices.push(usize::from_str(rest).map_err(|_| {
                                Error::new(Variant::IllFormattedCommand,
                                           "Output not given a list of numbers".to_string())
                            })?);
                        break;
                    }
                }
                Ok(Cmd::Output(indices))
            } else if s.starts_with("recover") {
                let rest = &s[7..].trim();
                let idx = usize::from_str(rest).map_err(|_| {
                        Error::new(Variant::InvalidIdx,
                                   format!("Recover expects history index, but found `{}`", rest))
                    })?;
                Ok(Cmd::Recover(idx))
            } else if s.starts_with("+") {
                let rest = &s[1..].trim();
                let expr = ExprOrIdx::from_str(rest)?;
                Ok(Cmd::Apply(Op::Plus, expr))
            } else if s.starts_with("-") {
                let rest = &s[1..].trim();
                let expr = ExprOrIdx::from_str(rest)?;
                Ok(Cmd::Apply(Op::Minus, expr))
            } else if s.starts_with("/") {
                let rest = &s[1..].trim();
                let expr = ExprOrIdx::from_str(rest)?;
                Ok(Cmd::Apply(Op::Div, expr))
            } else if s.starts_with("*") {
                let rest = &s[1..].trim();
                let expr = ExprOrIdx::from_str(rest)?;
                Ok(Cmd::Apply(Op::Times, expr))
            } else if s.starts_with("^") {
                let rest = &s[1..].trim();
                let expr = ExprOrIdx::from_str(rest)?;
                Ok(Cmd::Apply(Op::Power, expr))
            } else if s.starts_with("$") {
                Ok(Cmd::New(Math::from_str(&s[1..].trim())?))
            } else if s.starts_with("code") {
                let rest = &s[4..].trim();
                let idx = usize::from_str(rest).map_err(|_| {
                        Error::new(Variant::InvalidIdx,
                                   format!("Recover expects history index, but found `{}`", rest))
                    })?;
                Ok(Cmd::GetCode(idx))
            } else if s.starts_with("replace") {
                let (indices, rest) = parse_indices(&s[7..].trim())?;
                if indices.len() == 0 {
                    return Err(Error::new(Variant::IllFormattedCommand,
                                          "replace expects indices, but found none".to_string()));
                }
                let expr = ExprOrIdx::from_str(rest.trim())?;
                Ok(Cmd::Replace(indices, expr))
            } else if s.starts_with("collapse") {
                let rest = s[8..].trim();

                let (n, rest) = if char::from(rest.as_bytes()
                        .last()
                        .ok_or(Error::from_variant(Variant::IllFormattedCommand))?
                        .clone())
                    .is_digit(10) {
                    let splitpoint = rest.rfind(|c: char| !c.is_digit(10))
                        .ok_or(Error::from_variant(Variant::IllFormattedCommand))?;
                    let (fst, snd) = rest.split_at(splitpoint + 1);

                    (Some(snd.trim()
                         .parse()
                         .map_err(|_| Error::from_variant(Variant::IllFormattedCommand))?),
                     fst.trim())
                } else {
                    (None, rest)
                };

                let idx = TreeIdx::from_str(rest)?;

                Ok(Cmd::Collapse(idx, n))
            } else if s == "flip" {
                Ok(Cmd::Flip)
            } else if s.starts_with("distribute") {
                let (mut indices, rest) = parse_indices(&s[10..].trim())?;
                if indices.len() == 0 || rest.trim().len() > 0 {
                    Err(Error::new(Variant::IllFormattedCommand,
                                   "distribute expects indices".to_string()))
                } else {
                    let last_index = indices.pop().unwrap();
                    Ok(if indices.len() == 0 {
                        Cmd::DistributePower(last_index)
                    } else {
                        Cmd::Distribute(indices, last_index)
                    })
                }
            } else if s.starts_with("flatten") {
                let (mut indices, rest) = parse_indices(&s[7..].trim())?;
                if indices.len() != 1 || rest.trim().len() > 0 {
                    Err(Error::new(Variant::IllFormattedCommand,
                                   "flatten expects one index".to_string()))
                } else {
                    let i = indices.pop().unwrap();
                    Ok(Cmd::Flatten(i))
                }
            } else {
                Err(if Math::from_str(s).is_ok() {
                    Error::new(Variant::UnrecognizedCmd,
                               format!("Did you mean to enter Math? Try using a dollar \
                                        sign:\n\t$ {}",
                                       s))
                } else {
                    Error::new(Variant::UnrecognizedCmd,
                               format!("Did not recognize cmd `{}`", s))
                })
            }
        } else if s.starts_with("feedback@") {
            let s = &s[9..];
            let end_of_text_idx = s.find('\0')
                .ok_or(Error::from_variant(Variant::IllFormattedCommand))?;
            let text = &s[..end_of_text_idx];
            let html = &s[end_of_text_idx + 1..];
            Ok(Cmd::Feedback(text.to_string(), html.to_string()))
        } else {
            Err(Error::new(Variant::UnrecognizedCmd,
                           format!("Did not recognize cmd `{}`", s)))
        }
    }
}
