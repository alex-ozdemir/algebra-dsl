use std::fmt;
use super::{Expression, Atom, EqOrExpr, Equation};

impl fmt::Display for EqOrExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &EqOrExpr::Eq(ref eq) => write!(f, "{}", eq),
            &EqOrExpr::Ex(ref ex) => write!(f, "{}", ex),
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
        }
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
                        fmt_as_latex(&eq.left, f, (&eq.left, false, true))?;
                        write!(f, " &= ")?;
                        fmt_as_latex(&eq.right, f, (&eq.right, false, true))?;
                    }
                    &EqOrExpr::Ex(ref ex) => {
                        fmt_as_latex(&ex, f, (&ex, false, true))?;
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

// alt is usually for right expressions, when they need different rules
// Outputs whether expr1 captures expr2 with parenthesis
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
                    _ => false,
                }
            }
            &Expression::Product(_) => {
                match expr2 {
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
            &Expression::Division(_, _) => {
                match expr2 {
                    _ => false,
                }
            }
            &Expression::Power(_, _) => {
                match expr2 {
                    &Expression::Sum(_) => true,
                    &Expression::Product(_) => true,
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
            &Expression::Product(_) => {
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
            &Expression::Division(_, _) => {
                match expr2 {
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

fn fmt_as_math_ml(expr: &Expression,
                  f: &mut fmt::Formatter,
                  prev_index: &str,
                  prev_precedence: (&Expression, bool, bool))
                  -> Result<(), fmt::Error> {
    write!(f, "<mrow mathTreeNode=\"{}\">", prev_index)?;
    if capture(prev_precedence.0,
               expr,
               prev_precedence.1,
               prev_precedence.2) {
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
            let mut base_string = String::from(prev_index);
            write!(f, "<mfrac>")?;
            base_string.push_str(",0");
            fmt_as_math_ml(n, f, &base_string, (expr, false, false))?;
            let mut base_string = String::from(prev_index);
            base_string.push_str(",1");
            fmt_as_math_ml(d, f, &base_string, (expr, false, false))?;
            write!(f, "</mfrac>")?;
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
        &Expression::Product(ref s) => {
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
                        &Expression::Atom(Atom::Floating(_)) => write!(f, "<mo>&#x022C5;</mo>")?,
                        &Expression::Atom(Atom::Natural(_)) => write!(f, "<mo>&#x022C5;</mo>")?,
                        _ => write!(f, "<mo>&#8290;</mo>")?,
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
        write!(f,"<mo form=\"postfix\">)</mo>")?;
    }
    write!(f, "</mrow>")
}

fn fmt_as_latex(expr: &Expression,
                f: &mut fmt::Formatter,
                prev_precedence: (&Expression, bool, bool))
                -> Result<(), fmt::Error> {
    if capture(prev_precedence.0,
               expr,
               prev_precedence.1,
               prev_precedence.2) {
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
            fmt_as_latex(b, f, (expr, false, false))?;
            write!(f,"}}^{{")?;
            fmt_as_latex(p, f, (expr, true, false))?;
            write!(f,"}}")?;
        }
        &Expression::Negation(ref n) => {
            write!(f,"-")?;
            fmt_as_latex(n, f, (expr, false, false))?;
        }
        &Expression::Division(ref n, ref d) => {
            write!(f,"\\frac{{")?;
            fmt_as_latex(n, f, (expr, false, false))?;
            write!(f,"}}{{")?;
            fmt_as_latex(d, f, (expr, false, false))?;
            write!(f,"}}")?;
        }
        &Expression::Subscript(ref e, ref s) => {
            write!(f,"{{")?;
            fmt_as_latex(e, f, (expr, false, false))?;
            write!(f,"}}_{{")?;
            fmt_as_latex(s, f, (expr, true, false))?;
            write!(f,"}}")?;
        }
        &Expression::Sum(ref s) => {
            let len = s.len();
            let iter = s.iter().enumerate();
            for (i, e) in iter {
                if i == len - 1 {
                    fmt_as_latex(e, f, (expr, false, false))?;
                } else {
                    fmt_as_latex(e, f, (expr, false, false))?;
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
        &Expression::Product(ref s) => {
            let len = s.len();
            let iter = s.iter().enumerate();
            for (i, e) in iter {
                if i == len - 1 {
                    fmt_as_latex(e, f, (expr, false, false))?;
                } else {
                    fmt_as_latex(e, f, (expr, false, false))?;
                    let e_next = &s[i + 1];
                    match e_next {
                        &Expression::Atom(Atom::Floating(_)) => write!(f,"\\cdot")?,
                        &Expression::Atom(Atom::Natural(_)) => write!(f,"\\cdot")?,
                        _ => {}
                    };
                }
            }
        }
        &Expression::Application(ref func, ref arg) => {
            fmt_as_latex(func, f, (expr, false, false))?;
            write!(f," ")?;
            fmt_as_latex(arg, f, (expr, true, false))?;
        }
        &Expression::LimitOp(ref op, None, None, ref expr) => {
            write!(f,"{}", op.as_latex())?;
            fmt_as_latex(expr, f, (expr, false, false))?;
        }
        &Expression::LimitOp(ref op, Some(ref sub), None, ref expr) => {
            write!(f,"{}", op.as_latex())?;
            write!(f,"_{{")?;
            fmt_as_latex(sub, f, (expr, false, false))?;
            write!(f,"}} ")?;
            fmt_as_latex(expr, f, (expr, false, false))?;
        }
        &Expression::LimitOp(ref op, None, Some(ref sup), ref expr) => {
            write!(f,"{}", op.as_latex())?;
            write!(f,"^{{")?;
            fmt_as_latex(sup, f, (expr, false, false))?;
            write!(f,"}} ")?;
            fmt_as_latex(expr, f, (expr, false, false))?;
        }
        &Expression::LimitOp(ref op, Some(ref sub), Some(ref sup), ref expr) => {
            write!(f,"{}", op.as_latex())?;
            write!(f,"_{{")?;
            fmt_as_latex(sub, f, (expr, false, false))?;
            write!(f,"}}^{{")?;
            fmt_as_latex(sup, f, (expr, false, false))?;
            write!(f,"}} ")?;
            fmt_as_latex(expr, f, (expr, false, false))?;
        }
    }
    if capture(prev_precedence.0,
               expr,
               prev_precedence.1,
               prev_precedence.2) {
        write!(f,"\\right)")?;
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
        &Expression::Division(_, _) => u8::min_value(),
        &Expression::Product(_) => 25,
        &Expression::Sum(_) => 15,
        &Expression::Negation(_) => 15,
    }
}
