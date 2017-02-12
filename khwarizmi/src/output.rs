use std::fmt;
use super::{Expression, Atom, EqOrExpr, Equation};

impl fmt::Display for Equation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">")?;
        write!(f, "<mrow mathTreeNode=\"0\">")?;
        fmt_as_math_ml(&self.left, f, "0,0", 0)?;
        write!(f, "<mo>=</mo>")?;
        fmt_as_math_ml(&self.right, f, "0,1", 0)?;
        write!(f, "</mrow>")?;
        write!(f, "</math>")
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">")?;
        fmt_as_math_ml(&self, f, "0", 0)?;
        write!(f, "</math>")
    }
}

pub struct LatexWriter(String);

impl LatexWriter {
    pub fn new() -> LatexWriter {
        LatexWriter("\\begin{{align*}}\n".to_string())
    }

    pub fn add_math(&mut self, e: &EqOrExpr) -> fmt::Result {
        use std::fmt::Write;

        struct DisplaysAsLatex<'a>(&'a EqOrExpr);
        impl<'a> fmt::Display for DisplaysAsLatex<'a> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "  ")?;
                match self.0 {
                    &EqOrExpr::Eq(ref eq) => {
                        fmt_as_latex(&eq.left, f, 0)?;
                        write!(f, " &= ")?;
                        fmt_as_latex(&eq.right, f, 0)?;
                    }
                    &EqOrExpr::Ex(ref ex) => {
                        fmt_as_latex(&ex, f, 0)?;
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


pub fn fmt_as_math_ml(expr: &Expression,
                  f: &mut fmt::Formatter,
                  prev_index: &str,
                  prev_precedence: u8)
                  -> Result<(), fmt::Error> {
    let prec = precedence(expr);
    write!(f, "<mrow mathTreeNode=\"{}\">", prev_index)?;
    if prec <= prev_precedence {
        write!(f, "<mo form=\"prefix\">(</mo>")?;
    }
    match expr {
        &Expression::Atom(atom) => {
            write!(f, "{}", atom)?;
        }
        &Expression::Power(ref b, ref p) => {
            let mut base_string = String::from(prev_index);
            write!(f, "<msup>")?;
            base_string.push_str(",0");
            fmt_as_math_ml(b, f, &base_string, prec)?;
            let mut base_string = String::from(prev_index);
            base_string.push_str(",1");
            fmt_as_math_ml(p, f, &base_string, prec)?;
            write!(f, "</msup>")?;
        }
        &Expression::Negation(ref n) => {
            let mut base_string = String::from(prev_index);
            write!(f, "<mo>-</mo>")?;
            base_string.push_str(",0");
            fmt_as_math_ml(n, f, &base_string, prec)?;
        }
        &Expression::Division(ref n, ref d) => {
            let mut base_string = String::from(prev_index);
            write!(f, "<mfrac>")?;
            base_string.push_str(",0");
            fmt_as_math_ml(n, f, &base_string, prec)?;
            let mut base_string = String::from(prev_index);
            base_string.push_str(",1");
            fmt_as_math_ml(d, f, &base_string, prec)?;
            write!(f, "</mfrac>")?;
        }
        &Expression::Subscript(ref e, ref s) => {
            let mut base_string = String::from(prev_index);
            write!(f, "<msub>")?;
            base_string.push_str(",0");
            fmt_as_math_ml(e, f, &base_string, prec)?;
            let mut base_string = String::from(prev_index);
            base_string.push_str(",1");
            fmt_as_math_ml(s, f, &base_string, prec)?;
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
                    fmt_as_math_ml(e, f, &base_string, prec)?;
                } else {
                    fmt_as_math_ml(e, f, &base_string, prec)?;
                    write!(f, "<mo>+</mo>")?;
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
                    fmt_as_math_ml(e, f, &base_string, prec)?;
                } else {
                    fmt_as_math_ml(e, f, &base_string, prec)?;
                    write!(f, "<mo>&#8290;</mo>")?;
                }
            }
        }
        &Expression::Application(ref func, ref arg) => {
            let mut base_string = String::from(prev_index);
            base_string.push_str(",0");
            fmt_as_math_ml(func, f, &base_string, prec)?;
            let mut base_string = String::from(prev_index);
            base_string.push_str(",1");
            write!(f, "<mo>(</mo>")?;
            fmt_as_math_ml(arg, f, &base_string, prec)?;
            write!(f, "<mo>)</mo>")?;
        }
        &Expression::LimitOp(ref op, None, None, ref expr) => {
            let mut base_string = String::from(prev_index);
            write!(f, "{}", op.as_math_ml())?;

            base_string.push_str(",2");
            fmt_as_math_ml(expr, f, &base_string, prec)?;
        }
        &Expression::LimitOp(ref op, Some(ref sub), None, ref expr) => {
            let mut base_string = String::from(prev_index);
            let orig_len = base_string.len();
            write!(f, "<munder>")?;
            write!(f, "{}", op.as_math_ml())?;

            base_string.push_str(",0");
            fmt_as_math_ml(sub, f, &base_string, prec)?;
            base_string.truncate(orig_len);

            write!(f, "</munder>")?;
            base_string.push_str(",2");
            fmt_as_math_ml(expr, f, &base_string, prec)?;
        }
        &Expression::LimitOp(ref op, None, Some(ref sup), ref expr) => {
            let mut base_string = String::from(prev_index);
            let orig_len = base_string.len();
            write!(f, "<mover>")?;
            write!(f, "{}", op.as_math_ml())?;

            base_string.push_str(",1");
            fmt_as_math_ml(sup, f, &base_string, prec)?;
            base_string.truncate(orig_len);

            write!(f, "</mover>")?;
            base_string.push_str(",2");
            fmt_as_math_ml(expr, f, &base_string, prec)?;
        }
        &Expression::LimitOp(ref op, Some(ref sub), Some(ref sup), ref expr) => {
            let mut base_string = String::from(prev_index);
            let orig_len = base_string.len();
            write!(f, "<munderover>")?;
            write!(f, "{}", op.as_math_ml())?;

            base_string.push_str(",0");
            fmt_as_math_ml(sub, f, &base_string, prec)?;
            base_string.truncate(orig_len);

            base_string.push_str(",1");
            fmt_as_math_ml(sup, f, &base_string, prec)?;
            base_string.truncate(orig_len);

            write!(f, "</munderover>")?;
            base_string.push_str(",2");
            fmt_as_math_ml(expr, f, &base_string, prec)?;
        }
    }
    if prec <= prev_precedence {
        write!(f, "<mo form=\"postfix\">)</mo>")?;
    }
    write!(f, "</mrow>")
}

pub fn fmt_as_latex(expr: &Expression,
                f: &mut fmt::Formatter,
                prev_precedence: u8)
                -> Result<(), fmt::Error> {
    let prec = precedence(expr);
    if prec <= prev_precedence {
        write!(f, "\\left(")?;
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
            fmt_as_latex(b, f, prec)?;
            write!(f, "^{{")?;
            fmt_as_latex(p, f, 0)?; // TODO: Is this 0 ok?
            write!(f, "}}")?;
        }
        &Expression::Negation(ref n) => {
            write!(f, "-")?;
            fmt_as_latex(n, f, prec)?;
        }
        &Expression::Division(ref n, ref d) => {
            write!(f, "\\frac{{")?;
            fmt_as_latex(n, f, prec)?;
            write!(f, "}}{{")?;
            fmt_as_latex(d, f, prec)?;
            write!(f, "}}")?;
        }
        &Expression::Subscript(ref e, ref s) => {
            fmt_as_latex(e, f, prec)?;
            write!(f, "_{{")?;
            fmt_as_latex(s, f, prec)?;
            write!(f, "}}")?;
        }
        &Expression::Sum(ref s) => {
            let mut first = true;
            for e in s {
                if !first {
                    write!(f, "+")?;
                }
                first = false;
                fmt_as_latex(e, f, prec)?;
            }
        }
        &Expression::Product(ref s) => {
            for e in s {
                fmt_as_latex(e, f, prec)?;
            }
        }
        &Expression::Application(ref func, ref arg) => {
            fmt_as_latex(func, f, prec)?;
            write!(f, "(")?;
            fmt_as_latex(arg, f, prec)?;
            write!(f, ")")?;
        }
        &Expression::LimitOp(ref op, ref sub, ref sup, ref expr) => {
            write!(f, "{}", op.as_latex())?;
            if let &Some(ref s) = sub {
                write!(f, "_{{")?;
                fmt_as_latex(s, f, prec)?;
                write!(f, "}}")?;
            }
            if let &Some(ref s) = sup {
                write!(f, "^{{")?;
                fmt_as_latex(s, f, prec)?;
                write!(f, "}}")?;
            }
            fmt_as_latex(expr, f, prec)?;
        }
    }
    if prec <= prev_precedence {
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
        &Expression::Division(_, _) => u8::min_value(),
        &Expression::Product(_) => 25,
        &Expression::Sum(_) => 15,
        &Expression::Negation(_) => 15,
    }
}

