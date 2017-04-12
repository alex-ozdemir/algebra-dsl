use super::*;

use Expression as Ex;
use FunctionSymbol;

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

fn nat(i: i64) -> Ex {
    Ex::Atom(Atom::Natural(i))
}

fn var(c: char) -> Ex {
    Ex::Atom(Atom::PlainVariable(c))
}

fn float(f: f64) -> Ex {
    Ex::Atom(Atom::Floating(f))
}

#[test]
fn just_a_variable() {
    assert_expected_eq_actual!(Ok(var('x')), parse_expr("x"));
}

#[test]
fn single_digit_natural() {
    assert_expected_eq_actual!(Ok(nat(1)), parse_expr("1"));
}

#[test]
fn multi_digit_natural() {
    assert_expected_eq_actual!(Ok(nat(123465)), parse_expr("123465"));
}

#[test]
fn single_addition() {
    let expected = Ex::Sum(vec![var('x'), nat(2)]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("x +  2"));
}

#[test]
fn multi_addition() {
    let expected = Ex::Sum(vec![var('x'),
                                nat(2),
                                nat(9)]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("x +  2+9"));
}

#[test]
fn negation() {
    let expected = Ex::Negation(box var('y'));
    assert_expected_eq_actual!(Ok(expected), parse_expr("-y"));
}

#[test]
fn negation_and_addition() {
    let expected = Ex::Sum(vec![Ex::Negation(box var('x')),
                                nat(2),
                                nat(9),
                                Ex::Negation(box nat(7))]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("-x +  2+9-7"));
}

#[test]
fn double_negation_and_addition() {
    let expected =
        Ex::Sum(vec![Ex::Negation(box Ex::Negation(box var('x'))),
                     nat(2),
                     nat(9),
                     Ex::Negation(box nat(7))]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("--x +  2+9-7"));
}

#[test]
fn single_multiplication() {
    let expected = Ex::Division(vec![var('x'), nat(2)],
                                vec![]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("x \\times  2"));
}

#[test]
fn multi_multiplication() {
    let expected = Ex::Division(vec![var('x'),
                                     nat(2),
                                     nat(9),
                                     nat(8)],
                                vec![]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("x \\times  2 \\cdot 9 * 8"));
}

#[test]
fn implicit_multiplication() {
    let expected = Ex::Division(vec![nat(2),
                                     var('x'),
                                     nat(9),
                                     nat(8)],
                                vec![]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("2x9(8)"));
}

#[test]
fn implicit_multiplication_adjacent_parens() {
    let expected = Ex::Division(vec![nat(2),
                                     var('x'),
                                     nat(9),
                                     nat(8)],
                                vec![]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("2x(9)(8)"));
}

#[test]
fn addition_and_multiplication() {
    let expected = Ex::Sum(vec![Ex::Division(vec![nat(4),
                                                  var('x')],
                                             vec![]),
                                nat(2)]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("4x + 2"));
}

#[test]
fn addition_and_multiplication_grouping() {
    let expected = Ex::Division(vec![Ex::Sum(vec![nat(4),
                                                  var('x')]),
                                     nat(2)],
                                vec![]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("(4 + x)2"));
}

#[test]
fn single_div() {
    let expected = Ex::Division(vec![var('x')],
                                vec![nat(2)]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("x /2"));
}

#[test]
fn single_frac() {
    let expected = Ex::Division(vec![var('x')],
                                vec![nat(2)]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("\\frac x 2"));
}

#[test]
fn single_frac_with_braces() {
    let expected = Ex::Division(vec![var('x')],
                                vec![nat(2)]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("\\dfrac{x}{2}"));
}

#[test]
fn multi_frac() {
    let expected = Ex::Division(vec![Ex::Division(vec![nat(4)],
                                                  vec![var('x')])],
                                vec![nat(2)]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("\\frac{\\frac4x}{2}"));
}

#[test]
fn multi_frac_spaces() {
    let expected = Ex::Division(vec![Ex::Division(vec![nat(4)],
                                                  vec![var('x')])],
                                vec![nat(2)]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("  \\frac { \\frac 4 x } { 2 } "));
}

#[test]
fn power() {
    let expected = Ex::Power(box nat(4),
                             box var('x'));
    assert_expected_eq_actual!(Ok(expected), parse_expr("4^x"));
}

#[test]
fn power_grouped_exponent() {
    let expected = Ex::Power(box nat(4),
                             box Ex::Sum(vec![var('x'),
                                              nat(1)]));
    assert_expected_eq_actual!(Ok(expected), parse_expr("4^{x+1}"));
}

#[test]
fn subscript() {
    let expected = Ex::Subscript(box nat(4),
                                 box var('x'));
    assert_expected_eq_actual!(Ok(expected), parse_expr("4_x"));
}

#[test]
fn subscript_grouped() {
    let expected = Ex::Subscript(box nat(4),
                                 box Ex::Sum(vec![var('x'),
                                                  nat(1)]));
    assert_expected_eq_actual!(Ok(expected), parse_expr("4_{x+1}"));
}

#[test]
fn equation() {
    let expected = Equation {
        left: var('x'),
        right: nat(4),
    };
    assert_expected_eq_actual!(Ok(expected), parse_equation("x=4"));
}

#[test]
fn sqrt() {
    let expected = Ex::Power(box var('x'),
                             box Ex::Division(vec![nat(1)],
                                              vec![nat(2)]));
    assert_expected_eq_actual!(Ok(expected), parse_expr("\\sqrt x"));
}

#[test]
fn sin() {
    let expected = Ex::Application(box FunctionSymbol::sin.as_expr(),
                                   box var('x'));
    assert_expected_eq_actual!(Ok(expected), parse_expr("\\sin x"));
}

#[test]
fn sin_with_power() {
    let expected = Ex::Power(box Ex::Application(box FunctionSymbol::sin.as_expr(),
                                                 box var('x')),
                             box nat(2));
    assert_expected_eq_actual!(Ok(expected), parse_expr("\\sin^2 x"));
}

#[test]
fn log_with_power_and_sub() {
    let f = Ex::Subscript(box FunctionSymbol::log.as_expr(),
                          box nat(5));
    let expected = Ex::Power(box Ex::Application(box f, box var('x')),
                             box nat(2));
    assert_expected_eq_actual!(Ok(expected), parse_expr("\\log^2_5 x"));
}

#[test]
fn cot_with_power() {
    let expected = Ex::Power(box Ex::Application(box FunctionSymbol::cot.as_expr(),
                                                 box var('x')),
                             box float(2.6));
    assert_expected_eq_actual!(Ok(expected), parse_expr("\\cot^{2.6} x"));
}

#[test]
fn integral_with_bounds() {
    let expected = Ex::LimitOp(OperatorSymbol::int,
                               Some(box nat(0)),
                               Some(box nat(2)),
                               box Ex::Division(vec![var('x'),
                                                     var('d'),
                                                     var('x')],
                                                vec![]));
    assert_expected_eq_actual!(Ok(expected), parse_expr("\\int_0^2 x dx"));
}

#[test]
fn powers_and_fracs() {
    let expected = Ex::Power(box Ex::Sum(vec![var('x'),
                                              Ex::Division(vec![nat(1)],
                                                           vec![nat(2)])]),
                             box Ex::Division(vec![nat(3)],
                                              vec![nat(4)]));
    assert_expected_eq_actual!(Ok(expected), parse_expr("(x + \\frac12)^{3/4}"));
}

#[test]
fn float_() {
    let expected = float(5.1);
    assert_expected_eq_actual!(Ok(expected), parse_expr("5.1"));
}

#[test]
fn float_no_natural() {
    let expected = float(0.1);
    assert_expected_eq_actual!(Ok(expected), parse_expr(".1"));
}

#[test]
fn float_no_dec() {
    let expected = float(5.);
    assert_expected_eq_actual!(Ok(expected), parse_expr("5."));
}

#[test]
fn error_on_unmatch() {
    let expected = Err(ParseError::UnmatchGrouping(UniOp::Std(Operator::LGroup)));
    assert_expected_eq_actual!(expected, parse_expr("( a"));
    let expected = Err(ParseError::UnmatchGrouping(UniOp::Std(Operator::RGroup)));
    assert_expected_eq_actual!(expected, parse_expr("a )"));
}

#[test]
fn negation_and_multiplication() {
    let expected = Ex::Negation(box Ex::Division(vec![var('x'),
                                                      nat(2),
                                                      nat(9),
                                                      nat(7)],
                                                 vec![]));
    assert_expected_eq_actual!(Ok(expected), parse_expr("-x*2*9*7"));
}

#[test]
fn radicals() {
    let expected = Ex::Power(box nat(2), box Ex::Division(vec![nat(1)], vec![nat(2)]));
    assert_expected_eq_actual!(Ok(expected), parse_expr("\\sqrt{2}"));
    let expected = Ex::Power(box nat(2), box Ex::Division(vec![nat(1)], vec![nat(2)]));
    assert_expected_eq_actual!(Ok(expected), parse_expr("\\sqrt[2]{2}"));
    let expected = Ex::Power(box nat(2), box Ex::Division(vec![nat(1)], vec![nat(4)]));
    assert_expected_eq_actual!(Ok(expected), parse_expr("\\sqrt[4]{2}"));
}
