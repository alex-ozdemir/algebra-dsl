//! Contains a LaTeX parser of sorts.

use nom::{alpha, digit, sp, IResult};
use std::str::{self, FromStr};

named!(single_char<char>,
    one_of!("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
);

named!(identifier<String>,
    map!(
        map_res!(
            alpha,
            str::from_utf8
        ),
        String::from
    )
);

named!(control_sequence<String>,
    do_parse!(
        char!('\\') >>
        cs: identifier >>
        (cs)
    )
);

named!(number<u64>,
    map_res!(
        map_res!(
            ws!(digit),
            str::from_utf8
        ),
        FromStr::from_str
    )
);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Special {
    Tilde,
    Backtick,
    Exclamation,
    Percent,
    Caret,
    Star,
    LParen,
    RParen,
    Dash,
    Underscore,
    Plus,
    Equal,
    LSquareBracket,
    RSquareBracket,
    VertBar,
    Colon,
    SemiColon,
    Quote,
    DoubleQuote,
    Comma,
    Period,
    LAngleBracket,
    RAngleBracket,
    Question,
    Divide,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Special(Special),
    Char(char),
    ControlSequence(String),
    Natural(u64),
    List(Vec<Token>),
}

named!(special_character<Special>,
    alt!(
        value!(Special::Tilde, char!('~')) |
        value!(Special::Backtick, char!('`')) |
        value!(Special::Exclamation, char!('!')) |
        value!(Special::Percent, char!('%')) |
        value!(Special::Caret, char!('^')) |
        value!(Special::Star, char!('*')) |
        value!(Special::LParen, char!('(')) |
        value!(Special::RParen, char!(')')) |
        value!(Special::Dash, char!('-')) |
        value!(Special::Underscore, char!('_')) |
        value!(Special::Plus, char!('+')) |
        value!(Special::Equal, char!('=')) |
        value!(Special::LSquareBracket, char!('[')) |
        value!(Special::RSquareBracket, char!(']')) |
        value!(Special::VertBar, char!('|')) |
        value!(Special::SemiColon, char!(';')) |
        value!(Special::Colon, char!(':')) |
        value!(Special::DoubleQuote, char!('"')) |
        value!(Special::Quote, char!('\'')) |
        value!(Special::Comma, char!(',')) |
        value!(Special::Period, char!('.')) |
        value!(Special::LAngleBracket, char!('<')) |
        value!(Special::RAngleBracket, char!('>')) |
        value!(Special::Divide, char!('/')) |
        value!(Special::Question, char!('?'))
    )
);

named!(token<Token>,
    preceded!(sp,
        alt!(
            map!(special_character, Token::Special) |
            map!(control_sequence, Token::ControlSequence) |
            map!(single_char, Token::Char) |
            map!(number, Token::Natural) |
            map!(delimited!(char!('{'), many0!(token), char!('}')), Token::List)
        )
    )
);

named!(tokens<Token>, map!(many0!(token), Token::List));

pub fn parse_tokens(input: &str) -> Result<Token, String> {
    match tokens(input.as_bytes()) {
        IResult::Done(b"", token) => Ok(token),
        result => Err(format!("{:?}", result)),
    }
}

#[cfg(test)]
mod tests {

    // This macro is an assertion with nicely formatted failure output
    macro_rules! assert_expected_eq_actual {
        ($a:expr, $b:expr) => ({
            let (a, b) = (&$a, &$b);
            assert!(*a == *b,
                    "\nExpected `{:?}` is not equal to Actual `{:?}`\
                    \nAssertion: `assert_expected_eq_actual!({}, {})`",
                    *a,
                    *b,
                    stringify!($a),
                    stringify!($b));
        })
    }

    use nom::{IResult};
    use super::*;

    #[test]
    fn test_ident() {
        let input = &b"hi"[..];
        let expected = IResult::Done(&b""[..], String::from("hi"));
        assert_expected_eq_actual!(identifier(input), expected);
    }

    #[test]
    fn test_ident_break_space() {
        let expected = IResult::Done(&b" there"[..], String::from("hi"));
        assert_expected_eq_actual!(identifier(&b"hi there"[..]), expected);
    }

    #[test]
    fn test_ident_break_number() {
        let expected = IResult::Done(&b"1"[..], String::from("hi"));
        assert_expected_eq_actual!(identifier(&b"hi1"[..]), expected);
    }

    #[test]
    fn test_control_sequence() {
        let input = &b"\\hi"[..];
        let expected = IResult::Done(&b""[..], String::from("hi"));
        assert_expected_eq_actual!(control_sequence(input), expected);
    }

    #[test]
    fn test_control_sequence_break_number() {
        let input = &b"\\hi1"[..];
        let expected = IResult::Done(&b"1"[..], String::from("hi"));
        assert_expected_eq_actual!(control_sequence(input), expected);
    }

    #[test]
    fn test_number() {
        let input = &b"134"[..];
        let expected = IResult::Done(&b""[..], 134);
        assert_expected_eq_actual!(number(input), expected);
    }

    #[test]
    fn test_chars() {
        let input = &b"hi12"[..];
        let expected = IResult::Done(&b""[..], Token::List(vec![Token::Char('h'),
                                                                Token::Char('i'),
                                                                Token::Natural(12)]));
        assert_expected_eq_actual!(tokens(input), expected);
    }

    #[test]
    fn test_number_sequence() {
        let input = &b"1 3 4"[..];
        let expected = IResult::Done(&b""[..], Token::List(vec![Token::Natural(1),
                                                                Token::Natural(3),
                                                                Token::Natural(4)]));
        assert_expected_eq_actual!(tokens(input), expected);
    }

    #[test]
    fn test_1() {
        let input = &b"5 + \\frac {6 + 3} 4"[..];
        let answer = Token::List(vec![Token::Natural(5),
                                      Token::Special(Special::Plus),
                                      Token::ControlSequence(String::from("frac")),
                                      Token::List(vec![Token::Natural(6),
                                                       Token::Special(Special::Plus),
                                                       Token::Natural(3)]),
                                      Token::Natural(4)]);
        let expected = IResult::Done(&b""[..], answer);
        assert_expected_eq_actual!(tokens(input), expected);
    }
}
