//! Contains a LaTeX parser of sorts.

use nom::{alpha, sp, IResult};
use std::str;

named!(single_char<char>, preceded!(sp,
    one_of!("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890")
));

named!(identifier<String>,
    preceded!(sp,
        map!(
            map_res!(
                alpha,
                str::from_utf8
            ),
            String::from
        )
    )
);

named!(control_sequence<String>,
    do_parse!(
        sp >>
        char!('\\') >>
        cs: identifier >>
        (cs)
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
    List(Vec<Token>),
    Escaped(String),
}

named!(special_character<Special>,
    preceded!(sp, alt!(
        value!(Special::Tilde, char!('~')) |
        value!(Special::Backtick, char!('`')) |
        value!(Special::Exclamation, char!('!')) |
        value!(Special::Percent, tag!("\\%")) |
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
    ))
);

named!(open<char>, preceded!(sp, char!('{')));
named!(close<char>, preceded!(sp, char!('}')));

named!(open_escape<char>, preceded!(sp, char!('%')));
named!(close_escape<char>, char!('%'));

named!(escaped<String>,
    fold_many0!(
        none_of!("%"),
        String::new(),
        |mut s: String, c: char| {
            s.push(c);
            s
        }
    )
);

named!(token<Token>,
    alt!(
        map!(special_character, Token::Special) |
        map!(control_sequence, Token::ControlSequence) |
        map!(single_char, Token::Char) |
        map!(delimited!(open_escape, escaped, close_escape), Token::Escaped) |
        map!(delimited!(open, many0!(token), close), Token::List)
    )
);

named!(tokens<Token>, map!(many0!(token), Token::List));

pub fn parse_tokens(input: &str) -> Result<Token, String> {
    match tokens(input.trim().as_bytes()) {
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

    use nom::IResult;
    use super::*;

    #[test]
    fn test_ident() {
        let input = &b"hi"[..];
        let expected = IResult::Done(&b""[..], String::from("hi"));
        assert_expected_eq_actual!(expected, identifier(input));
    }

    #[test]
    fn test_ident_break_space() {
        let expected = IResult::Done(&b" there"[..], String::from("hi"));
        assert_expected_eq_actual!(expected, identifier(&b"hi there"[..]));
    }

    #[test]
    fn test_ident_break_number() {
        let expected = IResult::Done(&b"1"[..], String::from("hi"));
        assert_expected_eq_actual!(expected, identifier(&b"hi1"[..]));
    }

    #[test]
    fn test_control_sequence() {
        let input = &b"\\hi"[..];
        let expected = IResult::Done(&b""[..], String::from("hi"));
        assert_expected_eq_actual!(expected, control_sequence(input));
    }

    #[test]
    fn test_control_sequence_break_number() {
        let input = &b"\\hi1"[..];
        let expected = IResult::Done(&b"1"[..], String::from("hi"));
        assert_expected_eq_actual!(expected, control_sequence(input));
    }

    #[test]
    fn test_escaped() {
        let input = &b" %hi%"[..];
        let expected = IResult::Done(&b""[..],
                                     Token::List(vec![Token::Escaped(String::from("hi"))]));
        assert_expected_eq_actual!(expected, tokens(input));
    }

    #[test]
    fn test_multi_escaped() {
        let input = &b" 5 %hi% \\hi%% h % hi\t\n % %!@#$^&*()_+%"[..];
        let expected = IResult::Done(&b""[..],
                                     Token::List(vec![
            Token::Char('5'),
            Token::Escaped(String::from("hi")),
            Token::ControlSequence(String::from("hi")),
            Token::Escaped(String::from("")),
            Token::Char('h'),
            Token::Escaped(String::from(" hi\t\n ")),
            Token::Escaped(String::from("!@#$^&*()_+")) ]));
        assert_expected_eq_actual!(expected, tokens(input));
    }

    #[test]
    fn test_chars() {
        let input = &b"hi1"[..];
        let expected = IResult::Done(&b""[..],
                                     Token::List(vec![Token::Char('h'),
                                                                Token::Char('i'),
                                                                Token::Char('1')]));
        assert_expected_eq_actual!(expected, tokens(input));
    }

    #[test]
    fn test_digit_split() {
        let input = &b"\\frac12"[..];
        let answer = Token::List(vec![Token::ControlSequence(String::from("frac")),
                                      Token::Char('1'),
                                      Token::Char('2')]);
        let expected = IResult::Done(&b""[..], answer);
        assert_expected_eq_actual!(expected, tokens(input));
    }

    #[test]
    fn test_number_sequence() {
        let input = &b"1 3 4"[..];
        let expected = IResult::Done(&b""[..],
                                     Token::List(vec![Token::Char('1'),
                                                                Token::Char('3'),
                                                                Token::Char('4')]));
        assert_expected_eq_actual!(expected, tokens(input));
    }

    #[test]
    fn test_1() {
        let input = &b"5 + \\frac {6 + 3} 4"[..];
        let answer = Token::List(vec![Token::Char('5'),
                                      Token::Special(Special::Plus),
                                      Token::ControlSequence(String::from("frac")),
                                      Token::List(vec![Token::Char('6'),
                                                       Token::Special(Special::Plus),
                                                       Token::Char('3')]),
                                      Token::Char('4')]);
        let expected = IResult::Done(&b""[..], answer);
        assert_expected_eq_actual!(expected, tokens(input));
    }

    #[test]
    fn test_2() {
        let input = &b" 5 + \\frac { 6 + 3 } { 4 - 5 }"[..];
        let answer = Token::List(vec![Token::Char('5'),
                                      Token::Special(Special::Plus),
                                      Token::ControlSequence(String::from("frac")),
                                      Token::List(vec![Token::Char('6'),
                                                       Token::Special(Special::Plus),
                                                       Token::Char('3')]),
                                      Token::List(vec![Token::Char('4'),
                                                       Token::Special(Special::Dash),
                                                       Token::Char('5')])]);
        let expected = IResult::Done(&b""[..], answer);
        assert_expected_eq_actual!(expected, tokens(input));
    }
}
