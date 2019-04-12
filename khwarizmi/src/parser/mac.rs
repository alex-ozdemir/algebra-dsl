use super::{latex, OperatorSymbol, ParseError, Special, StandaloneSymbol, Symbol, Token};

/// The Control Sequences that our system handles. A small set of macros along with a *bunch* of
/// symbols.
#[allow(non_camel_case_types)]
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum KnownCS {
    // TODO: Greek letters
    frac,
    dfrac,
    sqrt,
    cdot,
    times,
    div,
    left,
    right,
    Preserved(Symbol),
}

impl KnownCS {
    fn from_str(s: &str) -> Option<KnownCS> {
        match s {
            "frac" => Some(KnownCS::frac),
            "dfrac" => Some(KnownCS::dfrac),
            "cdot" => Some(KnownCS::cdot),
            "times" => Some(KnownCS::times),
            "sqrt" => Some(KnownCS::sqrt),
            "div" => Some(KnownCS::div),
            "left" => Some(KnownCS::left),
            "right" => Some(KnownCS::right),
            s => Symbol::from_str(s).map(KnownCS::Preserved),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Operator {
    Times,
    Plus,
    Minus,
    Neg,
    Div,
    Underscore,
    Caret,
    LGroup,
    RGroup,
    /// Used only within the Operator Parser. Indicates beginning of expression
    Begin,
    /// Used only within the Operator Parser. Indicates end of expression
    End,
}

impl Operator {
    pub fn left_precedence(&self) -> u8 {
        use self::Operator::*;
        match *self {
            Begin => panic!("We should never look left of Begin"),
            End => u8::min_value(),
            RGroup => u8::min_value() + 1,
            Plus | Minus => 15,
            Neg => 20,
            Times | Div => 25,
            Caret => 45,
            Underscore => 55,
            LGroup => u8::max_value() - 1,
        }
    }
    pub fn right_precedence(&self) -> u8 {
        use self::Operator::*;
        match *self {
            Begin => u8::min_value(),
            End => panic!("We should never look right of End"),
            LGroup => u8::min_value() + 1,
            Plus | Minus => 16,
            Neg => 19,
            Times | Div => 26,
            Caret => 44,
            Underscore => 54,
            RGroup => u8::max_value() - 1,
        }
    }
    pub fn arity(&self) -> Option<u8> {
        use self::Operator::*;
        match *self {
            Begin | End | LGroup | RGroup => None,
            Plus | Minus | Times | Div | Caret | Underscore => Some(2),
            Neg => Some(1),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum PostMac {
    Frac(Box<PostMac>, Box<PostMac>),
    Sqrt(u64, Box<PostMac>),
    List(Vec<PostMac>),
    Standalone(StandaloneSymbol),
    Op(UniOp),
    Char(char),
    Escaped(String),
    Placeholder,
    Num(Numeric),
}

/// Stuff before the decimal, whether there has been a decimal, stuff after.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Numeric(pub String, pub Option<String>);

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum UniOp {
    Std(Operator),
    LimitOp(OperatorSymbol, Option<Box<PostMac>>, Option<Box<PostMac>>),
}

impl UniOp {
    pub fn arity(&self) -> Option<u8> {
        match self {
            &UniOp::Std(op) => op.arity(),
            &UniOp::LimitOp(_, _, _) => Some(1),
        }
    }
    pub fn left_precedence(&self) -> u8 {
        match self {
            &UniOp::Std(op) => op.left_precedence(),
            &UniOp::LimitOp(_, _, _) => 20,
        }
    }
    pub fn right_precedence(&self) -> u8 {
        match self {
            &UniOp::Std(op) => op.right_precedence(),
            &UniOp::LimitOp(_, _, _) => 19,
        }
    }
}

impl PostMac {
    /// Return whether there should be an operator after this type of LaTeX construct.
    pub fn expects_op_after(&self) -> bool {
        match self {
            &PostMac::List(ref list) => list.last().expect("No empty lists!").expects_op_after(),
            &PostMac::Frac(_, _)
            | &PostMac::Sqrt(_, _)
            | &PostMac::Op(UniOp::Std(Operator::RGroup))
            | &PostMac::Standalone(_)
            | &PostMac::Char(_)
            | &PostMac::Escaped(_)
            | &PostMac::Placeholder
            | &PostMac::Num(_) => true,
            &PostMac::Op(_) => false,
        }
    }
    /// Return whether there should be an operator before this type of LaTeX construct.
    pub fn expects_op_before(&self) -> bool {
        match self {
            &PostMac::List(ref list) => list.first().expect("No empty lists!").expects_op_before(),
            &PostMac::Frac(_, _)
            | &PostMac::Sqrt(_, _)
            | &PostMac::Op(UniOp::Std(Operator::LGroup))
            | &PostMac::Standalone(_)
            | &PostMac::Char(_)
            | &PostMac::Escaped(_)
            | &PostMac::Placeholder
            | &PostMac::Num(_) => true,
            &PostMac::Op(_) => false,
        }
    }
}

pub fn to_known(input: latex::Token) -> Result<PostMac, ParseError> {
    match input {
        Token::Special(Special::Star) => Ok(PostMac::Op(UniOp::Std(Operator::Times))),
        Token::Special(Special::LParen) => Ok(PostMac::Op(UniOp::Std(Operator::LGroup))),
        Token::Special(Special::RParen) => Ok(PostMac::Op(UniOp::Std(Operator::RGroup))),
        Token::Special(Special::Plus) => Ok(PostMac::Op(UniOp::Std(Operator::Plus))),
        Token::Special(Special::LSquareBracket) => Ok(PostMac::Op(UniOp::Std(Operator::LGroup))),
        Token::Special(Special::RSquareBracket) => Ok(PostMac::Op(UniOp::Std(Operator::RGroup))),
        Token::Special(Special::Dash) => Ok(PostMac::Op(UniOp::Std(Operator::Minus))),
        Token::Special(Special::Divide) => Ok(PostMac::Op(UniOp::Std(Operator::Div))),
        Token::Special(Special::Caret) => Ok(PostMac::Op(UniOp::Std(Operator::Caret))),
        Token::Special(Special::Underscore) => Ok(PostMac::Op(UniOp::Std(Operator::Underscore))),
        Token::Special(Special::Period) => {
            Ok(PostMac::Num(Numeric("".to_string(), Some("".to_string()))))
        }
        Token::Escaped(string) => Ok(PostMac::Escaped(string)),
        Token::Special(x) => Err(ParseError::UnknownSpecialChar(x)),
        Token::Char('@') => Ok(PostMac::Placeholder),
        Token::Char(c) if c.is_alphabetic() => Ok(PostMac::Char(c)),
        Token::Char(c) if c.is_numeric() => Ok(PostMac::Num(Numeric(c.to_string(), None))),
        Token::Char(c) => Err(ParseError::UnimplementedCharacter(c)),
        Token::ControlSequence(cs) => {
            let known_cs =
                KnownCS::from_str(cs.as_str()).ok_or(ParseError::UnknownControlSequence(cs))?;
            match known_cs {
                KnownCS::div => Ok(PostMac::Op(UniOp::Std(Operator::Div))),
                KnownCS::cdot | KnownCS::times => Ok(PostMac::Op(UniOp::Std(Operator::Times))),
                KnownCS::Preserved(Symbol::Standalone(sym)) => Ok(PostMac::Standalone(sym)),
                KnownCS::Preserved(Symbol::Operator(sym)) => {
                    Ok(PostMac::Op(UniOp::LimitOp(sym, None, None)))
                }
                x => Err(ParseError::LoneControlSequence(x)),
            }
        }
        Token::List(mut list) => {
            list.reverse();
            let mut result_list = vec![];
            while let Some(next) = list.pop() {
                let next = match to_known(next) {
                    Err(ParseError::LoneControlSequence(KnownCS::dfrac))
                    | Err(ParseError::LoneControlSequence(KnownCS::frac)) => {
                        let (first, second) = two_expressions(&mut list)?;
                        PostMac::Frac(box first, box second)
                    }
                    Err(ParseError::LoneControlSequence(KnownCS::sqrt)) => {
                        let optional_radical = opt_num_arg(&mut list)?;
                        let argument = one_expression(&mut list)?;
                        PostMac::Sqrt(optional_radical.unwrap_or(2), box argument)
                    }
                    Err(ParseError::LoneControlSequence(KnownCS::left))
                    | Err(ParseError::LoneControlSequence(KnownCS::right)) => {
                        continue;
                    }
                    e @ Err(_) => return e,
                    Ok(underscore @ PostMac::Op(UniOp::Std(Operator::Underscore))) => {
                        let last = result_list.pop();
                        if let Some(PostMac::Op(UniOp::LimitOp(op, l, u))) = last {
                            if l.is_some() {
                                return Err(ParseError::DoubleUnderscore);
                            }
                            let argument = one_expression(&mut list)?;
                            PostMac::Op(UniOp::LimitOp(op, Some(box argument), u))
                        } else {
                            last.map(|l| result_list.push(l));
                            underscore
                        }
                    }
                    Ok(caret @ PostMac::Op(UniOp::Std(Operator::Caret))) => {
                        let last = result_list.pop();
                        if let Some(PostMac::Op(UniOp::LimitOp(op, l, u))) = last {
                            if u.is_some() {
                                return Err(ParseError::DoubleCaret);
                            }
                            let argument = one_expression(&mut list)?;
                            PostMac::Op(UniOp::LimitOp(op, l, Some(box argument)))
                        } else {
                            last.map(|l| result_list.push(l));
                            caret
                        }
                    }
                    Ok(x) => x,
                };

                // If there are two adjacent 'numeric' tokens, merge them
                let last = result_list.pop();
                match (last, next) {
                    (Some(PostMac::Num(last_num)), PostMac::Num(next_num)) => {
                        result_list.push(PostMac::Num(merge_numerics(last_num, next_num)?));
                    }
                    (last, mut next) => {
                        last.map(|last| result_list.push(last));
                        // Potentially insert multiplication
                        let op_expected = result_list
                            .last()
                            .map(PostMac::expects_op_after)
                            .unwrap_or(false);
                        let implicit_times = op_expected && next.expects_op_before();
                        if implicit_times {
                            result_list.push(PostMac::Op(UniOp::Std(Operator::Times)));
                        }
                        if next == PostMac::Op(UniOp::Std(Operator::Minus)) && !op_expected {
                            next = PostMac::Op(UniOp::Std(Operator::Neg));
                        }
                        result_list.push(next);
                    }
                }
            }
            match result_list.len() {
                0 => Err(ParseError::EmptyList),
                1 => Ok(result_list.pop().unwrap()),
                _ => Ok(PostMac::List(result_list)),
            }
        }
    }
}

/// Parses [n] where n is a natural.
fn opt_num_arg(input: &mut Vec<latex::Token>) -> Result<Option<u64>, ParseError> {
    let mut num: u64 = 0;
    if input.last() == Some(&latex::Token::Special(Special::LSquareBracket)) {
        input.pop();
        loop {
            match input.pop() {
                Some(latex::Token::Char(c)) if c.is_digit(10) => {
                    num = 10 * num + (c.to_digit(10).unwrap() as u64);
                }
                Some(latex::Token::Special(Special::RSquareBracket)) => {
                    break;
                }
                _ => {
                    return Err(ParseError::UnclosedOptionalArgument);
                }
            }
        }
        Ok(Some(num))
    } else {
        Ok(None)
    }
}

fn one_expression(input: &mut Vec<latex::Token>) -> Result<PostMac, ParseError> {
    let first = to_known(input.pop().ok_or(ParseError::FracNotFollowedByTwoExprs)?)?;
    Ok(first)
}

fn two_expressions(input: &mut Vec<latex::Token>) -> Result<(PostMac, PostMac), ParseError> {
    let first = to_known(input.pop().ok_or(ParseError::FracNotFollowedByTwoExprs)?)?;
    let second = to_known(input.pop().ok_or(ParseError::FracNotFollowedByTwoExprs)?)?;
    Ok((first, second))
}

pub fn merge_numerics(
    Numeric(mut l_pre, l_post): Numeric,
    Numeric(r_pre, r_post): Numeric,
) -> Result<Numeric, ParseError> {
    match (l_post, r_post) {
        (Some(_), Some(_)) => Err(ParseError::DoubleDecimalPoint),
        (Some(mut l_post), None) => {
            l_post.push_str(r_pre.as_str());
            Ok(Numeric(l_pre, Some(l_post)))
        }
        (None, Some(r_post)) => {
            l_pre.push_str(r_pre.as_str());
            Ok(Numeric(l_pre, Some(r_post)))
        }
        (None, None) => {
            l_pre.push_str(r_pre.as_str());
            Ok(Numeric(l_pre, None))
        }
    }
}
