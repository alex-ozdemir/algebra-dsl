use std::str;
use std::fs::OpenOptions;
use std::io::Write;

use khwarizmi::{Expression, TreeIdx, AlgebraDSLError, Indexable, EqOrExpr, LatexWriter};

#[derive(Debug, PartialEq, Clone)]
pub enum Return {
    EqOrExpr(EqOrExpr),
    LaTeXStr(String),
    /// The code the use requested and the object it corresponds to
    LaTeXInput(String, EqOrExpr),
    NoReturn,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Op {
    Plus,
    Times,
    Div,
    Minus,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Cmd {
    New(EqOrExpr),
    Make(Vec<TreeIdx>, Expression),
    Delete(Vec<TreeIdx>),
    Map(Op, Expression),
    Output(Vec<usize>),
    Recover(usize),
    GetCode(usize),
    Feedback(String, String),
}

impl Cmd {
    pub fn execute(self,
                   e: Option<&EqOrExpr>,
                   history: &Vec<EqOrExpr>)
                   -> Result<Return, AlgebraDSLError> {
        match (self, e) {
            (Cmd::New(e), _) => Ok(Return::EqOrExpr(e)),
            (Cmd::Make(indices, new_expr), Some(old_expr)) => {
                let mut expr = old_expr.clone();
                if indices.len() == 1 {
                    expr.replace(&indices[0], new_expr)?;
                } else {
                    let sibs = old_expr.make_siblings(indices.as_slice())?;
                    println!("Make location {:?} into {:?}", sibs, new_expr);
                    expr.replace_siblings(sibs, new_expr)?;
                }
                Ok(Return::EqOrExpr(expr))
            }
            (Cmd::Make(_, _), None) => Err(AlgebraDSLError::NeedsExpression),
            (Cmd::Delete(indices), Some(old_expr)) => {
                let mut expr = old_expr.clone();
                let sibs = old_expr.make_siblings(indices.as_slice())?;
                expr.delete(sibs)?;
                Ok(Return::EqOrExpr(expr))
            }
            (Cmd::Delete(_), None) => Err(AlgebraDSLError::NeedsExpression),
            (Cmd::Map(op, new_expr), Some(e)) => {
                if let &EqOrExpr::Eq(ref e) = e {
                    let mut eq = e.clone();
                    match op {
                        Op::Times => eq.times_to_both(new_expr),
                        Op::Plus => eq.plus_to_both(new_expr),
                        Op::Div => eq.div_to_both(new_expr),
                        Op::Minus => eq.minus_to_both(new_expr),
                    }
                    Ok(Return::EqOrExpr(EqOrExpr::Eq(eq)))
                } else {
                    Err(AlgebraDSLError::MapExpression)
                }
            }
            (Cmd::Map(_, _), _) => Err(AlgebraDSLError::MapExpression),
            (Cmd::Output(math_idxs_to_output), _) => {
                let mut latex_writer = LatexWriter::new();
                for idx in math_idxs_to_output {
                    latex_writer.add_math(history.get(idx)
                            .ok_or(AlgebraDSLError::InvalidIdx)?)
                        .map_err(|_| AlgebraDSLError::InternalError)?;
                }
                Ok(Return::LaTeXStr(latex_writer.finish_str()
                    .map_err(|_| AlgebraDSLError::InternalError)?))
            }
            (Cmd::Recover(idx), e) => {
                println!("History is index {:?}: {:#?}", idx, history);
                let recover_math = history.get(idx).ok_or(AlgebraDSLError::InvalidIdx)?;
                let latex_string = recover_math.as_khwarizmi_latex();
                let parsed = EqOrExpr::from_str(latex_string.as_str().trim())?;
                Cmd::New(parsed).execute(e, history)
            }
            (Cmd::GetCode(idx), _) => {
                let recover_math = history.get(idx).ok_or(AlgebraDSLError::InvalidIdx)?;
                let latex_string = recover_math.as_khwarizmi_latex();
                let parsed = EqOrExpr::from_str(latex_string.as_str().trim())?;
                Ok(Return::LaTeXInput(latex_string, parsed))
            }
            (Cmd::Feedback(text, html),_) => {
                let mut f = OpenOptions::new().create(true).append(true).open(::REPORTFILE)
                        .map_err(|_|AlgebraDSLError::InternalError)?;
                write!(f,"Feedback text:\n{}\nFeedback html:\n{}\n\n\n",
                       text, html).map_err(|_|AlgebraDSLError::InternalError)?;
                Ok(Return::NoReturn)
            }
        }
    }
}

impl str::FromStr for Cmd {
    type Err = AlgebraDSLError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with("cmd@") {
            let s: &str = &s[4..].trim_left();
            if s.starts_with("make") {
                let mut indices = Vec::new();
                let mut rest: &str = &s[4..].trim();
                while rest.starts_with("#") {
                    let idx_end = rest.find(')').ok_or(AlgebraDSLError::IllFormattedIndex)?;
                    let idx = TreeIdx::from_str(&rest[..(idx_end + 1)])?;
                    indices.push(idx);
                    rest = &rest[(idx_end + 1)..].trim();
                }
                let expr = Expression::from_str(rest)?;
                Ok(Cmd::Make(indices, expr))
            } else if s.starts_with("delete") {
                let mut indices = Vec::new();
                let mut rest: &str = &s[6..].trim();
                while rest.starts_with("#") {
                    let idx_end = rest.find(')').ok_or(AlgebraDSLError::IllFormattedIndex)?;
                    let idx = TreeIdx::from_str(&rest[..(idx_end + 1)])?;
                    indices.push(idx);
                    rest = &rest[(idx_end + 1)..].trim();
                }
                if !rest.is_empty() {
                    println!("Rest isn't empty, it's {}", rest);
                    Err(AlgebraDSLError::IllFormattedCommand)
                } else {
                    Ok(Cmd::Delete(indices))
                }
            } else if s.starts_with("output") {
                let mut indices = Vec::new();
                let mut rest: &str = &s[6..].trim();
                loop {
                    if let Some(comma_idx) = rest.find(',') {
                        indices.push(usize::from_str(&rest[..comma_idx])
                                     .map_err(|_|AlgebraDSLError::InvalidIdx)?);
                        rest = rest[comma_idx + 1..].trim();
                    } else {
                        indices.push(usize::from_str(rest).map_err(|_| AlgebraDSLError::InvalidIdx)?);
                        break;
                    }
                }
                Ok(Cmd::Output(indices))
            } else if s.starts_with("recover") {
                let rest = &s[7..].trim();
                let idx = usize::from_str(rest).map_err(|_| AlgebraDSLError::InvalidIdx)?;
                Ok(Cmd::Recover(idx))
            } else if s.starts_with("+") {
                let rest = &s[1..].trim();
                let expr = Expression::from_str(rest)?;
                Ok(Cmd::Map(Op::Plus, expr))
            } else if s.starts_with("-") {
                let rest = &s[1..].trim();
                let expr = Expression::from_str(rest)?;
                Ok(Cmd::Map(Op::Minus, expr))
            } else if s.starts_with("/") {
                let rest = &s[1..].trim();
                let expr = Expression::from_str(rest)?;
                Ok(Cmd::Map(Op::Div, expr))
            } else if s.starts_with("*") {
                let rest = &s[1..].trim();
                let expr = Expression::from_str(rest)?;
                Ok(Cmd::Map(Op::Times, expr))
            } else if s.starts_with("$") {
                Ok(Cmd::New(EqOrExpr::from_str(&s[1..].trim())?))
            } else if s.starts_with("code") {
                let rest = &s[4..].trim();
                let idx = usize::from_str(rest).map_err(|_| AlgebraDSLError::InvalidIdx)?;
                Ok(Cmd::GetCode(idx))
            } else {
                Err(AlgebraDSLError::UnrecognizedCmd)
            }
        } else if s.starts_with("feedback@") {
            let s = &s[9..];
            let end_of_text_idx = s.find('\0').ok_or(AlgebraDSLError::IllFormattedCommand)?;
            let text = &s[..end_of_text_idx];
            let html = &s[end_of_text_idx+1..];
            Ok(Cmd::Feedback(text.to_string(), html.to_string()))
        } else {
            Err(AlgebraDSLError::IllFormattedCommand)
        }
    }
}
