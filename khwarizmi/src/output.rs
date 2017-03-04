use std::fmt::{self, Write};
use super::{Expression, Atom, Math, Equation};

const MROW: &'static str = "mrow";

#[derive(Clone, Copy)]
enum LaTeXOutputType {
    ForUs,
    ForOther,
}

pub struct LatexWriter(String);

impl fmt::Display for Math {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Math::Eq(ref eq) => write!(f, "{}", eq),
            &Math::Ex(ref ex) => write!(f, "{}", ex),
        }
    }
}

impl fmt::Display for Equation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">")?;
        write!(f, "<mrow mathTreeNode=\"0\">")?;
        fmt_as_math_ml(&self.left, f, "0,0", (&self.left, false, true))?;
        write!(f, "<mo>=</mo>")?;
        fmt_as_math_ml(&self.right, f, "0,1", (&self.right, false, true))?;
        write!(f, "</mrow>")?;
        write!(f, "</math>")
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">")?;
        fmt_as_math_ml(&self, f, "0", (&self, false, true))?;
        write!(f, "</math>")
    }
}
impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Atom::PlainVariable(c) => write!(f, "<mi>{}</mi>", c),
            &Atom::Natural(n) => write!(f, "<mn>{}</mn>", n),
            &Atom::Floating(r) => write!(f, "<mn>{}</mn>", r),
            &Atom::Symbol(sym) => write!(f, "<mo>{}</mo>", sym.as_math_ml()),
            &Atom::Escaped(ref s) => write!(f, "<mi mathvariant=\"monospace\">{}</mi>", s),
        }
    }
}

impl Math {
    pub fn as_khwarizmi_latex(&self) -> String {
        struct DisplaysAsLatex<'a>(&'a Math);
        impl<'a> fmt::Display for DisplaysAsLatex<'a> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "")?;
                match self.0 {
                    &Math::Eq(ref eq) => {
                        fmt_as_latex(&eq.left, f, (&eq.left, false, true), LaTeXOutputType::ForUs)?;
                        write!(f, " = ")?;
                        fmt_as_latex(&eq.right,
                                     f,
                                     (&eq.right, false, true),
                                     LaTeXOutputType::ForUs)?;
                    }
                    &Math::Ex(ref ex) => {
                        fmt_as_latex(&ex, f, (&ex, false, true), LaTeXOutputType::ForUs)?;
                    }
                }
                write!(f, "")
            }
        }
        format!("{}", DisplaysAsLatex(self))
    }
}

impl LatexWriter {
    pub fn new() -> LatexWriter {
        LatexWriter("\\begin{align*}\n".to_string())
    }

    pub fn add_math(&mut self, e: &Math) -> fmt::Result {
        use std::fmt::Write;

        struct DisplaysAsLatex<'a>(&'a Math);
        impl<'a> fmt::Display for DisplaysAsLatex<'a> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "  ")?;
                match self.0 {
                    &Math::Eq(ref eq) => {
                        fmt_as_latex(&eq.left,
                                     f,
                                     (&eq.left, false, true),
                                     LaTeXOutputType::ForOther)?;
                        write!(f, " &= ")?;
                        fmt_as_latex(&eq.right,
                                     f,
                                     (&eq.right, false, true),
                                     LaTeXOutputType::ForOther)?;
                    }
                    &Math::Ex(ref ex) => {
                        fmt_as_latex(&ex, f, (&ex, false, true), LaTeXOutputType::ForOther)?;
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

// alt is usually for right expressions, when they need different rules (power & subscript)
// Outputs whether expr1 captures expr2 with parentheses. I.e. whether to output (Expr2) vs Expr2.
//
// If `expr1` is a Divsion, `alt` is true when `expr2` is in the numerator. otherwise it is in the
// denominators and `alt` should be false.
fn capture(expr1: &Expression, expr2: &Expression, alt: bool, top: bool) -> bool {
    if top {
        // Did we just come from the top level call?
        return false;
    }
    if !alt {
        // Left capture
        match expr1 {
            &Expression::Sum(_) => {
                match expr2 {
                    &Expression::Sum(_) => true,
                    _ => false,
                }
            }
            ref d @ &Expression::Division(_, _) if d.is_product() => {
                match expr2 {
                    ref d2 @ &Expression::Division(_, _) if d2.is_product() => true,
                    &Expression::Sum(_) => true,
                    &Expression::Negation(_) => true,
                    _ => false,
                }
            }
            &Expression::Negation(_) => {
                match expr2 {
                    &Expression::Sum(_) => true,
                    _ => false,
                }
            }
            &Expression::Application(_, _) => {
                match expr2 {
                    _ => false,
                }
            }
            &Expression::LimitOp(_, _, _, _) => {
                match expr2 {
                    _ => false,
                }
            }
            &Expression::Division(_, ref d) => {
                match expr2 {
                    ref d2 @ &Expression::Division(_, _) if d2.is_product() => true,
                    &Expression::Sum(_) |
                    &Expression::Negation(_) => d.len() > 1,
                    _ => false,
                }
            }
            &Expression::Power(_, _) => {
                match expr2 {
                    &Expression::Sum(_) => true,
                    &Expression::Power(_, _) => true,
                    &Expression::Negation(_) => true,
                    &Expression::Division(_, _) => true,
                    _ => false,
                }
            }
            &Expression::Subscript(_, _) => {
                match expr2 {
                    &Expression::Atom(_) => false,
                    _ => true,
                }
            }
            &Expression::Atom(_) => false,
        }
    } else {
        // right capture
        match expr1 {
            &Expression::Sum(_) => {
                match expr2 {
                    _ => false,
                }
            }
            ref d @ &Expression::Division(_, _) if d.is_product() => {
                match expr2 {
                    _ => false,
                }
            }
            &Expression::Negation(_) => {
                match expr2 {
                    _ => false,
                }
            }
            &Expression::Application(_, _) => {
                match expr2 {
                    _ => true,
                }
            }
            &Expression::LimitOp(_, _, _, _) => {
                match expr2 {
                    _ => false,
                }
            }
            &Expression::Division(ref n, _) => {
                match expr2 {
                    ref d2 @ &Expression::Division(_, _) if d2.is_product() => true,
                    &Expression::Sum(_) |
                    &Expression::Negation(_) => n.len() > 1,
                    _ => false,
                }
            }
            &Expression::Power(_, _) => {
                match expr2 {
                    _ => false,
                }
            }
            &Expression::Subscript(_, _) => {
                match expr2 {
                    _ => false,
                }
            }
            &Expression::Atom(_) => false,
        }
    }
}

fn fmt_prod_as_math_ml(exprs: &[Expression],
                       f: &mut fmt::Formatter,
                       prev_index: &str,
                       prev_precedence: (&Expression, bool, bool),
                       start_idx: usize)
                       -> Result<(), fmt::Error> {
    let len = exprs.len();
    let iter = exprs.iter().enumerate();
    for (i, e) in iter {
        let mut base_string = String::from(prev_index);
        write!(base_string, ",{}", i + start_idx)?;
        fmt_as_math_ml(e, f, &base_string, prev_precedence)?;
        if i < len - 1 {
            let e_next = &exprs[i + 1];
            match e_next {
                &Expression::Atom(Atom::Floating(_)) |
                &Expression::Atom(Atom::Natural(_)) |
                &Expression::Power(box Expression::Atom(Atom::Floating(_)), _) |
                &Expression::Power(box Expression::Atom(Atom::Natural(_)), _) => {
                    write!(f, "<mo>&#x022C5;</mo>")?
                }
                _ => write!(f, "<mo>&#8290;</mo>")?,
            };
        }
    }
    Ok(())
}

fn fmt_as_math_ml(expr: &Expression,
                  f: &mut fmt::Formatter,
                  prev_index: &str,
                  prev_precedence: (&Expression, bool, bool))
                  -> Result<(), fmt::Error> {
    write!(f,
           "<{} mathTreeNode=\"{}\"{}>",
           MROW,
           prev_index,
           match expr {
               &Expression::Sum(..) => " multiparent=\"true\"",
               &Expression::Division(_, ref b) if b.len() == 0 => " multiparent=\"true\"",
               _ => "",
           })?;

    if capture(prev_precedence.0,
               expr,
               prev_precedence.1,
               prev_precedence.2) {
        write!(f, "<mo form=\"prefix\">(</mo>")?;
    }
    match expr {
        &Expression::Atom(ref atom) => {
            match prev_precedence.0 {
                &Expression::Sum(_) => {
                    match atom {
                        &Atom::Natural(n) => {
                            if n < 0 {
                                write!(f, "<mo>-</mo><mn>{}</mn>", -n)?
                            } else {
                                write!(f, "{}", atom)?
                            }
                        }
                        &Atom::Floating(n) => {
                            if n < 0.0 {
                                write!(f, "<mo>-</mo><mn>{}</mn>", -n)?
                            } else {
                                write!(f, "{}", atom)?
                            }
                        }
                        _ => write!(f, "{}", atom)?,
                    }
                }
                _ => write!(f, "{}", atom)?,
            };
        }
        &Expression::Power(ref b, ref p) => {
            let mut base_string = String::from(prev_index);
            write!(f, "<msup>")?;
            base_string.push_str(",0");
            fmt_as_math_ml(b, f, &base_string, (expr, false, false))?;
            let mut base_string = String::from(prev_index);
            base_string.push_str(",1");
            fmt_as_math_ml(p, f, &base_string, (expr, true, false))?;
            write!(f, "</msup>")?;
        }
        &Expression::Negation(ref n) => {
            let mut base_string = String::from(prev_index);
            write!(f, "<mo>-</mo>")?;
            base_string.push_str(",0");
            fmt_as_math_ml(n, f, &base_string, (expr, false, false))?;
        }
        &Expression::Division(ref n, ref d) => {
            if d.len() == 0 {
                fmt_prod_as_math_ml(n.as_slice(), f, prev_index, (expr, false, false), 0)?;
            } else {
                write!(f, "<mfrac>")?;
                write!(f, "<{} multiparent=\"true\">", MROW)?;
                fmt_prod_as_math_ml(n.as_slice(), f, prev_index, (expr, true, false), 0)?;
                write!(f, "</{}>", MROW)?;
                write!(f, "<{} multiparent=\"true\">", MROW)?;
                fmt_prod_as_math_ml(d.as_slice(), f, prev_index, (expr, false, false), n.len())?;
                write!(f, "</{}>", MROW)?;
                write!(f, "</mfrac>")?;
            }
        }
        &Expression::Subscript(ref e, ref s) => {
            let mut base_string = String::from(prev_index);
            write!(f, "<msub>")?;
            base_string.push_str(",0");
            fmt_as_math_ml(e, f, &base_string, (expr, false, false))?;
            let mut base_string = String::from(prev_index);
            base_string.push_str(",1");
            fmt_as_math_ml(s, f, &base_string, (expr, true, false))?;
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
                    fmt_as_math_ml(e, f, &base_string, (expr, false, false))?;
                } else {
                    fmt_as_math_ml(e, f, &base_string, (expr, false, false))?;
                    let e_next = &s[i + 1];
                    match e_next {
                        &Expression::Negation(_) => {}
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
                        _ => write!(f, "<mo>+</mo>")?,
                    };
                }
            }
        }
        &Expression::Application(ref func, ref arg) => {
            let mut base_string = String::from(prev_index);
            base_string.push_str(",0");
            fmt_as_math_ml(func, f, &base_string, (expr, false, false))?;
            let mut base_string = String::from(prev_index);
            base_string.push_str(",1");
            fmt_as_math_ml(arg, f, &base_string, (expr, true, false))?;
        }
        &Expression::LimitOp(ref op, None, None, ref expr) => {
            let mut base_string = String::from(prev_index);
            write!(f, "{}", op.as_math_ml())?;

            base_string.push_str(",2");
            fmt_as_math_ml(expr, f, &base_string, (expr, false, false))?;
        }
        &Expression::LimitOp(ref op, Some(ref sub), None, ref expr) => {
            let mut base_string = String::from(prev_index);
            let orig_len = base_string.len();
            write!(f, "<munder>")?;
            write!(f, "{}", op.as_math_ml())?;

            base_string.push_str(",0");
            fmt_as_math_ml(sub, f, &base_string, (expr, false, false))?;
            base_string.truncate(orig_len);

            write!(f, "</munder>")?;
            base_string.push_str(",2");
            fmt_as_math_ml(expr, f, &base_string, (expr, false, false))?;
        }
        &Expression::LimitOp(ref op, None, Some(ref sup), ref expr) => {
            let mut base_string = String::from(prev_index);
            let orig_len = base_string.len();
            write!(f, "<mover>")?;
            write!(f, "{}", op.as_math_ml())?;

            base_string.push_str(",1");
            fmt_as_math_ml(sup, f, &base_string, (expr, false, false))?;
            base_string.truncate(orig_len);

            write!(f, "</mover>")?;
            base_string.push_str(",2");
            fmt_as_math_ml(expr, f, &base_string, (expr, false, false))?;
        }
        &Expression::LimitOp(ref op, Some(ref sub), Some(ref sup), ref expr) => {
            let mut base_string = String::from(prev_index);
            let orig_len = base_string.len();
            write!(f, "<munderover>")?;
            write!(f, "{}", op.as_math_ml())?;

            base_string.push_str(",0");

            fmt_as_math_ml(sub, f, &base_string, (expr, false, false))?;

            base_string.truncate(orig_len);

            base_string.push_str(",1");

            fmt_as_math_ml(sup, f, &base_string, (expr, false, false))?;

            base_string.truncate(orig_len);

            write!(f, "</munderover>")?;
            base_string.push_str(",2");
            fmt_as_math_ml(expr, f, &base_string, (expr, false, false))?;
        }
    }
    if capture(prev_precedence.0,
               expr,
               prev_precedence.1,
               prev_precedence.2) {
        write!(f, "<mo form=\"postfix\">)</mo>")?;
    }
    write!(f, "</mrow>")
}

fn fmt_prod_as_latex(exprs: &[Expression],
                     f: &mut fmt::Formatter,
                     prev_precedence: (&Expression, bool, bool),
                     output: LaTeXOutputType)
                     -> Result<(), fmt::Error> {
    let len = exprs.len();
    let iter = exprs.iter().enumerate();
    for (i, e) in iter {
        if i == len - 1 {
            fmt_as_latex(e, f, prev_precedence, output)?;
        } else {
            fmt_as_latex(e, f, prev_precedence, output)?;
            let e_next = &exprs[i + 1];
            match e_next {
                &Expression::Atom(Atom::Floating(_)) |
                &Expression::Atom(Atom::Natural(_)) |
                &Expression::Power(box Expression::Atom(Atom::Floating(_)), _) |
                &Expression::Power(box Expression::Atom(Atom::Natural(_)), _) => {
                    write!(f, "\\cdot")?
                }
                _ => {}
            };
        }
    }
    Ok(())
}

/// Format this expression as LaTeX.
///
///    * `output`: whether this is bound for a LaTeX compiler (true) or our system (false)
fn fmt_as_latex(expr: &Expression,
                f: &mut fmt::Formatter,
                prev_precedence: (&Expression, bool, bool),
                output: LaTeXOutputType)
                -> Result<(), fmt::Error> {
    if capture(prev_precedence.0,
               expr,
               prev_precedence.1,
               prev_precedence.2) {
        write!(f, "\\left(")?;
    }
    match expr {
        &Expression::Atom(ref atom) => {
            match atom {
                &Atom::PlainVariable(c) => write!(f, "{}", c)?,
                &Atom::Natural(n) => write!(f, "{}", n)?,
                &Atom::Floating(r) => write!(f, "{}", r)?,
                &Atom::Symbol(sym) => write!(f, "{} ", sym.as_latex())?,
                &Atom::Escaped(ref s) => {
                    match output {
                        LaTeXOutputType::ForUs => write!(f, "%{}%", s),
                        LaTeXOutputType::ForOther => write!(f, "{}", s),
                    }?
                }
            }
        }
        &Expression::Power(ref b, ref p) => {
            write!(f, "{{")?;
            fmt_as_latex(b, f, (expr, false, false), output)?;
            write!(f, "}}^{{")?;
            fmt_as_latex(p, f, (expr, true, false), output)?;
            write!(f, "}}")?;
        }
        &Expression::Negation(ref n) => {
            write!(f, "-")?;
            fmt_as_latex(n, f, (expr, false, false), output)?;
        }
        &Expression::Division(ref n, ref d) => {
            if d.len() == 0 {
                fmt_prod_as_latex(n, f, (expr, false, false), output)?;
            } else {
                write!(f, "\\frac{{")?;
                fmt_prod_as_latex(n, f, (expr, true, false), output)?;
                write!(f, "}}{{")?;
                fmt_prod_as_latex(d, f, (expr, false, false), output)?;
                write!(f, "}}")?;
            }
        }
        &Expression::Subscript(ref e, ref s) => {
            write!(f, "{{")?;
            fmt_as_latex(e, f, (expr, false, false), output)?;
            write!(f, "}}_{{")?;
            fmt_as_latex(s, f, (expr, true, false), output)?;
            write!(f, "}}")?;
        }
        &Expression::Sum(ref s) => {
            let len = s.len();
            let iter = s.iter().enumerate();
            for (i, e) in iter {
                if i == len - 1 {
                    fmt_as_latex(e, f, (expr, false, false), output)?;
                } else {
                    fmt_as_latex(e, f, (expr, false, false), output)?;
                    let e_next = &s[i + 1];
                    match e_next {
                        &Expression::Negation(_) => {}
                        &Expression::Atom(Atom::Floating(n)) => {
                            if n >= 0.0 {
                                write!(f, "+")?;
                            }
                        }
                        &Expression::Atom(Atom::Natural(n)) => {
                            if n >= 0 {
                                write!(f, "+")?;
                            }
                        }
                        _ => write!(f, "+")?,
                    };
                }
            }
        }
        &Expression::Application(ref func, ref arg) => {
            fmt_as_latex(func, f, (expr, false, false), output)?;
            write!(f, " ")?;
            fmt_as_latex(arg, f, (expr, true, false), output)?;
        }
        &Expression::LimitOp(ref op, None, None, ref expr) => {
            write!(f, "{}", op.as_latex())?;
            fmt_as_latex(expr, f, (expr, false, false), output)?;
        }
        &Expression::LimitOp(ref op, Some(ref sub), None, ref expr) => {
            write!(f, "{}", op.as_latex())?;
            write!(f, "_{{")?;
            fmt_as_latex(sub, f, (expr, false, false), output)?;
            write!(f, "}} ")?;
            fmt_as_latex(expr, f, (expr, false, false), output)?;
        }
        &Expression::LimitOp(ref op, None, Some(ref sup), ref expr) => {
            write!(f, "{}", op.as_latex())?;
            write!(f, "^{{")?;
            fmt_as_latex(sup, f, (expr, false, false), output)?;
            write!(f, "}} ")?;
            fmt_as_latex(expr, f, (expr, false, false), output)?;
        }
        &Expression::LimitOp(ref op, Some(ref sub), Some(ref sup), ref expr) => {
            write!(f, "{}", op.as_latex())?;
            write!(f, "_{{")?;
            fmt_as_latex(sub, f, (expr, false, false), output)?;
            write!(f, "}}^{{")?;
            fmt_as_latex(sup, f, (expr, false, false), output)?;
            write!(f, "}} ")?;
            fmt_as_latex(expr, f, (expr, false, false), output)?;
        }
    }
    if capture(prev_precedence.0,
               expr,
               prev_precedence.1,
               prev_precedence.2) {
        write!(f, "\\right)")?;
    }
    Ok(())
}


pub fn precedence(expr: &Expression) -> u8 {
    match expr {
        &Expression::Atom(_) => u8::max_value(),
        &Expression::Application(_, _) => u8::max_value(),
        &Expression::LimitOp(_, _, _, _) => 60,
        &Expression::Subscript(_, _) => u8::min_value(),
        &Expression::Power(_, _) => 45,
        ref d @ &Expression::Division(_, _) if d.is_product() => 25,
        &Expression::Division(_, _) => u8::min_value(),
        &Expression::Sum(_) => 15,
        &Expression::Negation(_) => 15,
    }
}
