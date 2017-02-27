use super::{Atom, Expression};

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
    pub fn as_latex(&self) -> &'static str {
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
    pub fn as_math_ml(&self) -> &'static str {
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
    pub fn as_expr(self) -> Expression {
        Expression::Atom(Atom::Symbol(Symbol::Operator(OperatorSymbol::Function(self))))
    }
    pub fn accepts_subscripts(self) -> bool {
        use FunctionSymbol::*;
        match self {
            lg | ln | log => true,
            exp => false,
            sin | cos | tan | csc | sec | cot | arcsin | arccos | arctan | sinh | cosh | tanh |
            coth => false,
        }
    }
    pub fn accepts_superscripts(self) -> bool {
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
    pub fn as_math_ml(&self) -> &'static str {
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
    pub fn as_latex(&self) -> &'static str {
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
    pub fn as_expr(self) -> Expression {
        Expression::Atom(Atom::Symbol(Symbol::Operator(self)))
    }
}

impl StandaloneSymbol {
    pub fn as_math_ml(&self) -> &'static str {
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
    pub fn as_latex(&self) -> &'static str {
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
    pub fn from_str(s: &str) -> Option<Self> {
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
    pub fn as_math_ml(&self) -> &'static str {
        match self {
            &Symbol::Standalone(ref sym) => sym.as_math_ml(),
            &Symbol::Operator(ref sym) => sym.as_math_ml(),
        }
    }
    pub fn as_latex(&self) -> &'static str {
        match self {
            &Symbol::Standalone(ref sym) => sym.as_latex(),
            &Symbol::Operator(ref sym) => sym.as_latex(),
        }
    }
}
