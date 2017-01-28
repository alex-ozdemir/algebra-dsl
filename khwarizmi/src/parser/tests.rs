use super::*;

use Expression as Ex;
use Atom as At;

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

#[test]
fn just_a_variable() {
    assert_expected_eq_actual!(Ok(Ex::Atom(At::PlainVariable('x'))), parse_expr("x"));
}

#[test]
fn single_digit_natural() {
    assert_expected_eq_actual!(Ok(Ex::Atom(At::Natural(1))), parse_expr("1"));
}

#[test]
fn multi_digit_natural() {
    assert_expected_eq_actual!(Ok(Ex::Atom(At::Natural(123465))), parse_expr("123465"));
}

#[test]
fn single_addition() {
    let expected = Ex::Sum(vec![Ex::Atom(At::PlainVariable('x')), Ex::Atom(At::Natural(2))]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("x +  2"));
}

#[test]
fn multi_addition() {
    let expected = Ex::Sum(vec![Ex::Atom(At::PlainVariable('x')),
                                Ex::Atom(At::Natural(2)),
                                Ex::Atom(At::Natural(9))]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("x +  2+9"));
}

#[test]
fn negation() {
    let expected = Ex::Negation(box Ex::Atom(At::PlainVariable('y')));
    assert_expected_eq_actual!(Ok(expected), parse_expr("-y"));
}

#[test]
fn negation_and_addition() {
    let expected = Ex::Sum(vec![Ex::Negation(box Ex::Atom(At::PlainVariable('x'))),
                                Ex::Atom(At::Natural(2)),
                                Ex::Atom(At::Natural(9)),
                                Ex::Negation(box Ex::Atom(At::Natural(7)))]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("-x +  2+9-7"));
}

#[test]
fn double_negation_and_addition() {
    let expected =
        Ex::Sum(vec![Ex::Negation(box Ex::Negation(box Ex::Atom(At::PlainVariable('x')))),
                     Ex::Atom(At::Natural(2)),
                     Ex::Atom(At::Natural(9)),
                     Ex::Negation(box Ex::Atom(At::Natural(7)))]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("--x +  2+9-7"));
}

#[test]
fn single_multiplication() {
    let expected = Ex::Product(vec![Ex::Atom(At::PlainVariable('x')), Ex::Atom(At::Natural(2))]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("x \\times  2"));
}

#[test]
fn multi_multiplication() {
    let expected = Ex::Product(vec![Ex::Atom(At::PlainVariable('x')),
                                    Ex::Atom(At::Natural(2)),
                                    Ex::Atom(At::Natural(9)),
                                    Ex::Atom(At::Natural(8))]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("x \\times  2 \\cdot 9 * 8"));
}

#[test]
fn implicit_multiplication() {
    let expected = Ex::Product(vec![Ex::Atom(At::Natural(2)),
                                    Ex::Atom(At::PlainVariable('x')),
                                    Ex::Atom(At::Natural(9)),
                                    Ex::Atom(At::Natural(8))]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("2x9(8)"));
}

#[test]
fn implicit_multiplication_adjacent_parens() {
    let expected = Ex::Product(vec![Ex::Atom(At::Natural(2)),
                                    Ex::Atom(At::PlainVariable('x')),
                                    Ex::Atom(At::Natural(9)),
                                    Ex::Atom(At::Natural(8))]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("2x(9)(8)"));
}

#[test]
fn addition_and_multiplication() {
    let expected = Ex::Sum(vec![Ex::Product(vec![Ex::Atom(At::Natural(4)),
                                                 Ex::Atom(At::PlainVariable('x'))]),
                                Ex::Atom(At::Natural(2))]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("4x + 2"));
}

#[test]
fn addition_and_multiplication_grouping() {
    let expected = Ex::Product(vec![Ex::Sum(vec![Ex::Atom(At::Natural(4)),
                                                 Ex::Atom(At::PlainVariable('x'))]),
                                    Ex::Atom(At::Natural(2))]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("(4 + x)2"));
}

#[test]
fn single_div() {
    let expected = Ex::Division(box Ex::Atom(At::PlainVariable('x')),
                                box Ex::Atom(At::Natural(2)));
    assert_expected_eq_actual!(Ok(expected), parse_expr("x /2"));
}

#[test]
fn single_frac() {
    let expected = Ex::Division(box Ex::Atom(At::PlainVariable('x')),
                                box Ex::Atom(At::Natural(2)));
    assert_expected_eq_actual!(Ok(expected), parse_expr("\\frac x 2"));
}

#[test]
fn single_frac_with_braces() {
    let expected = Ex::Division(box Ex::Atom(At::PlainVariable('x')),
                                box Ex::Atom(At::Natural(2)));
    assert_expected_eq_actual!(Ok(expected), parse_expr("\\dfrac{x}{2}"));
}

#[test]
fn multi_frac() {
    let expected = Ex::Division(box Ex::Division(box Ex::Atom(At::Natural(4)),
                                                 box Ex::Atom(At::PlainVariable('x'))),
                                box Ex::Atom(At::Natural(2)));
    assert_expected_eq_actual!(Ok(expected), parse_expr("\\frac{\\frac4x}{2}"));
}

#[test]
fn multi_frac_spaces() {
    let expected = Ex::Division(box Ex::Division(box Ex::Atom(At::Natural(4)),
                                                 box Ex::Atom(At::PlainVariable('x'))),
                                box Ex::Atom(At::Natural(2)));
    assert_expected_eq_actual!(Ok(expected), parse_expr("  \\frac { \\frac 4 x } { 2 } "));
}

#[test]
fn power() {
    let expected = Ex::Power(box Ex::Atom(At::Natural(4)),
                             box Ex::Atom(At::PlainVariable('x')));
    assert_expected_eq_actual!(Ok(expected), parse_expr("4^x"));
}

#[test]
fn power_grouped_exponent() {
    let expected = Ex::Power(box Ex::Atom(At::Natural(4)),
                             box Ex::Sum(vec![Ex::Atom(At::PlainVariable('x')),
                                              Ex::Atom(At::Natural(1))]));
    assert_expected_eq_actual!(Ok(expected), parse_expr("4^{x+1}"));
}

#[test]
fn subscript() {
    let expected = Ex::Subscript(box Ex::Atom(At::Natural(4)),
                                 box Ex::Atom(At::PlainVariable('x')));
    assert_expected_eq_actual!(Ok(expected), parse_expr("4_x"));
}

#[test]
fn subscript_grouped() {
    let expected = Ex::Subscript(box Ex::Atom(At::Natural(4)),
                                 box Ex::Sum(vec![Ex::Atom(At::PlainVariable('x')),
                                                  Ex::Atom(At::Natural(1))]));
    assert_expected_eq_actual!(Ok(expected), parse_expr("4_{x+1}"));
}

#[test]
fn equation() {
    let expected = Equation {
        left: Ex::Atom(At::PlainVariable('x')),
        right: Ex::Atom(At::Natural(4)),
    };
    assert_expected_eq_actual!(Ok(expected), parse_equation("x=4"));
}

#[test]
fn sqrt() {
    let expected = Ex::Power(box Ex::Atom(At::PlainVariable('x')),
                             box Ex::Atom(At::Floating(0.5)));
    assert_expected_eq_actual!(Ok(expected), parse_expr("\\sqrt x"));
}

#[test]
fn sin() {
    let expected = Ex::Application(box Ex::Atom(At::Symbol(Symbol::Operator(OperatorSymbol::sin))),
                                   box Ex::Atom(At::PlainVariable('x')));
    assert_expected_eq_actual!(Ok(expected), parse_expr("\\sin x"));
}

#[test]
#[ignore]
fn sin_with_power() {
    let expected = Ex::Application(box Ex::Atom(At::Symbol(Symbol::Operator(OperatorSymbol::sin))),
                                   box Ex::Atom(At::PlainVariable('x')));
    assert_expected_eq_actual!(Ok(expected), parse_expr("\\sin^2 x"));
}

#[test]
fn powers_and_fracs() {
    let expected = Ex::Power(box Ex::Sum(vec![Ex::Atom(At::PlainVariable('x')),
                                              Ex::Division(box Ex::Atom(At::Natural(1)),
                                                           box Ex::Atom(At::Natural(2)))]),
                             box Ex::Division(box Ex::Atom(At::Natural(3)),
                                              box Ex::Atom(At::Natural(4))));
    assert_expected_eq_actual!(Ok(expected), parse_expr("(x + \\frac12)^{3/4}"));
}