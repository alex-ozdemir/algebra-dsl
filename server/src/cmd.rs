use std::fs::OpenOptions;
use std::io::Write;
use std::str;

use khwarizmi::{AlgebraDSLError as Error, ErrorVariant as Variant, Expression, Indexable,
                LatexWriter, Math, TreeIdx};


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
pub enum Cmd {
    New(Math),
    Make(Vec<TreeIdx>, Expression),
    Delete(Vec<TreeIdx>),
    Map(Op, Expression),
    FullMap(Expression),
    Swap(TreeIdx, TreeIdx),
    Output(Vec<usize>),
    Recover(usize),
    GetCode(usize),
    Feedback(String, String),
    Replace(Vec<TreeIdx>, Expression),
    Collapse(TreeIdx, Option<usize>),
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

impl Cmd {
    pub fn execute(self, e: Option<&Math>, history: &Vec<Math>) -> Result<Return, Error> {
        match (self, e) {
            (Cmd::New(e), _) => Ok(Return::Math(e)),
            (Cmd::Make(indices, new_expr), Some(old_expr)) => {
                let mut expr = old_expr.clone();
                if indices.len() == 1 {
                    expr.make(&indices[0], new_expr)?;
                } else {
                    let sibs = old_expr.sibling_indices(indices.as_slice())?;
                    println!("Make location {:?} into {:?}", sibs, new_expr);
                    expr.make_siblings(sibs, new_expr)?;
                }
                Ok(Return::Math(expr))
            }
            (Cmd::Delete(indices), Some(old_expr)) => {
                let mut expr = old_expr.clone();
                let sibs = old_expr.sibling_indices(indices.as_slice())?;
                expr.delete(sibs)?;
                Ok(Return::Math(expr))
            }
            (Cmd::Swap(i1, i2), Some(old_expr)) => {
                let mut expr = old_expr.clone();
                expr.swap(&i1, &i2)?;
                Ok(Return::Math(expr))
            }
            (Cmd::Map(op, new_expr), Some(e)) => {
                let map_string = match op {
                    Op::Times => format!("@ * ({})", new_expr.as_khwarizmi_latex()),
                    Op::Plus => format!("@ + ({})", new_expr.as_khwarizmi_latex()),
                    Op::Div => format!("@ / ({})", new_expr.as_khwarizmi_latex()),
                    Op::Minus => format!("@ - ({})", new_expr.as_khwarizmi_latex()),
                    Op::Power => format!("@ ^ ({})", new_expr.as_khwarizmi_latex()),
                };
                let template = Expression::from_str(&map_string).unwrap();
                Cmd::FullMap(template).execute(Some(e), history)
            }
            (Cmd::FullMap(template), Some(old_expr)) => {
                let expr = old_expr.clone();
                Ok(Return::Math(expr.map(template)?))
            }
            (Cmd::Output(math_idxs_to_output), _) => {
                let mut latex_writer = LatexWriter::new();
                for idx in math_idxs_to_output {
                    latex_writer.add_math(history.get(idx)
                            .ok_or_else(|| invalid_hist_idx_err(idx))?)
                        .map_err(|_| internal_err())
                        .unwrap();
                }
                Ok(Return::LaTeXBlock(latex_writer.finish_str()
                    .map_err(|_| internal_err())?))
            }
            (Cmd::Recover(idx), e) => {
                println!("History is index {:?}: {:#?}", idx, history);
                let recover_math = history.get(idx).ok_or_else(|| invalid_hist_idx_err(idx))?;
                let latex_string = recover_math.as_khwarizmi_latex();
                let parsed = Math::from_str(latex_string.as_str().trim())?;
                Cmd::New(parsed).execute(e, history)
            }
            (Cmd::GetCode(idx), _) => {
                let recover_math = history.get(idx).ok_or_else(|| invalid_hist_idx_err(idx))?;
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
                eqorexpr.replace_all(indices.iter(), new_expr)?;
                Ok(Return::Math(eqorexpr))
            }
            (Cmd::Collapse(idx, howfar), Some(old)) => {
                let howfar = howfar.unwrap_or(1);

                let mut e = old.clone();

                e.collapse(&idx, howfar)?;

                Ok(Return::Math(e))
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

impl str::FromStr for Cmd {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with("cmd@") {
            let s: &str = &s[4..].trim_left();
            if s.starts_with("make") {
                let (indices, rest) = parse_indices(&s[4..].trim())?;
                let expr = Expression::from_str(rest.trim())?;
                Ok(Cmd::Make(indices, expr))
            } else if s.starts_with("delete") {
                let (indices, rest) = parse_indices(&s[6..].trim())?;
                if !rest.trim().is_empty() {
                    Err(Error::new(Variant::IllFormattedCommand,
                                   format!("Rest isn't empty, it's `{}`", rest)))
                } else {
                    Ok(Cmd::Delete(indices))
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
            } else if s.starts_with("map") {
                let template = Expression::from_str(&s[3..].trim())?;
                Ok(Cmd::FullMap(template))
            } else if s.starts_with("output") {
                let mut indices = Vec::new();
                let mut rest: &str = &s[6..].trim();
                loop {
                    if let Some(comma_idx) = rest.find(',') {
                        indices.push(usize::from_str(&rest[..comma_idx])
                                     .map_err(|_| internal_err())?);
                        rest = rest[comma_idx + 1..].trim();
                    } else {
                        indices.push(usize::from_str(rest).map_err(|_| internal_err())?);
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
            } else if s.starts_with("^") {
                let rest = &s[1..].trim();
                let expr = Expression::from_str(rest)?;
                Ok(Cmd::Map(Op::Power, expr))
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
                                          "replace expects indices, but found nonw".to_string()));
                }
                let expr = Expression::from_str(rest.trim())?;
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
            } else {
                Err(Error::new(Variant::UnrecognizedCmd,
                               format!("Did not recognize cmd `{}`", s)))
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
