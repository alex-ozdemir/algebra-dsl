use super::*;

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
    assert_expected_eq_actual!(Ok(Expression::Atom(Atom::PlainVariable('x'))), parse_expr("x"));
}

#[test]
fn single_digit_natural() {
    assert_expected_eq_actual!(Ok(Expression::Atom(Atom::Natural(1))), parse_expr("1"));
}

#[test]
fn multi_digit_natural() {
    assert_expected_eq_actual!(Ok(Expression::Atom(Atom::Natural(123465))), parse_expr("123465"));
}

#[test]
fn single_addition() {
    let expected = Expression::Sum(vec![Expression::Atom(Atom::PlainVariable('x')),
                                        Expression::Atom(Atom::Natural(2))]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("x +  2"));
}

#[test]
fn multi_addition() {
    let expected = Expression::Sum(vec![Expression::Atom(Atom::PlainVariable('x')),
                                        Expression::Atom(Atom::Natural(2)),
                                        Expression::Atom(Atom::Natural(9))]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("x +  2+9"));
}

#[test]
fn negation() {
    let expected = Expression::Negation(box Expression::Atom(Atom::PlainVariable('y')));
    assert_expected_eq_actual!(Ok(expected), parse_expr("-y"));
}

#[test]
fn negation_and_addition() {
    let expected =
        Expression::Sum(vec![Expression::Negation(box Expression::Atom(Atom::PlainVariable('x'))),
                             Expression::Atom(Atom::Natural(2)),
                             Expression::Atom(Atom::Natural(9)),
                             Expression::Negation(box Expression::Atom(Atom::Natural(7)))]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("-x +  2+9-7"));
}

#[test]
fn double_negation_and_addition() {
    let expected =
        Expression::Sum(vec![Expression::Negation(box Expression::Negation(
                                box Expression::Atom(Atom::PlainVariable('x')))
                             ),
                             Expression::Atom(Atom::Natural(2)),
                             Expression::Atom(Atom::Natural(9)),
                             Expression::Negation(box Expression::Atom(Atom::Natural(7)))]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("--x +  2+9-7"));
}

#[test]
fn single_multiplication() {
    let expected = Expression::Product(vec![Expression::Atom(Atom::PlainVariable('x')),
                                        Expression::Atom(Atom::Natural(2))]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("x \\times  2"));
}

#[test]
fn multi_multiplication() {
    let expected = Expression::Product(vec![Expression::Atom(Atom::PlainVariable('x')),
                                        Expression::Atom(Atom::Natural(2)),
                                        Expression::Atom(Atom::Natural(9)),
                                        Expression::Atom(Atom::Natural(8))]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("x \\times  2 \\cdot 9 * 8"));
}

#[test]
fn implicit_multiplication() {
    let expected = Expression::Product(vec![Expression::Atom(Atom::Natural(2)),
                                        Expression::Atom(Atom::PlainVariable('x')),
                                        Expression::Atom(Atom::Natural(9)),
                                        Expression::Atom(Atom::Natural(8))]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("2x9(8)"));
}

#[test]
fn implicit_multiplication_adjacent_parens() {
    let expected = Expression::Product(vec![Expression::Atom(Atom::Natural(2)),
                                        Expression::Atom(Atom::PlainVariable('x')),
                                        Expression::Atom(Atom::Natural(9)),
                                        Expression::Atom(Atom::Natural(8))]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("2x(9)(8)"));
}

#[test]
fn addition_and_multiplication() {
    let expected =
        Expression::Sum(vec![Expression::Product(vec![Expression::Atom(Atom::Natural(4)),
                                                      Expression::Atom(Atom::PlainVariable('x'))]),
                             Expression::Atom(Atom::Natural(2))]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("4x + 2"));
}

#[test]
fn addition_and_multiplication_grouping() {
    let expected =
        Expression::Product(vec![Expression::Sum(vec![Expression::Atom(Atom::Natural(4)),
                                                      Expression::Atom(Atom::PlainVariable('x'))]),
                             Expression::Atom(Atom::Natural(2))]);
    assert_expected_eq_actual!(Ok(expected), parse_expr("(4 + x)2"));
}

#[test]
fn single_div() {
    let expected = Expression::Division(box Expression::Atom(Atom::PlainVariable('x')),
                                        box Expression::Atom(Atom::Natural(2)));
    assert_expected_eq_actual!(Ok(expected), parse_expr("x /2"));
}

#[test]
fn single_frac() {
    let expected = Expression::Division(box Expression::Atom(Atom::PlainVariable('x')),
                                        box Expression::Atom(Atom::Natural(2)));
    assert_expected_eq_actual!(Ok(expected), parse_expr("\\frac x 2"));
}

#[test]
fn single_frac_with_braces() {
    let expected = Expression::Division(box Expression::Atom(Atom::PlainVariable('x')),
                                        box Expression::Atom(Atom::Natural(2)));
    assert_expected_eq_actual!(Ok(expected), parse_expr("\\dfrac{x}{2}"));
}

#[test]
fn multi_frac() {
    let expected =
        Expression::Division(box Expression::Division(box Expression::Atom(Atom::Natural(4)),
                                                      box Expression::Atom(Atom::PlainVariable('x'))),
                             box Expression::Atom(Atom::Natural(2)));
    assert_expected_eq_actual!(Ok(expected), parse_expr("\\frac{\\frac4x}{2}"));
}

#[test]
fn power() {
    let expected = Expression::Power(box Expression::Atom(Atom::Natural(4)),
                                     box Expression::Atom(Atom::PlainVariable('x')));
    assert_expected_eq_actual!(Ok(expected), parse_expr("4^x"));
}

#[test]
fn power_grouped_exponent() {
    let expected = Expression::Power(box Expression::Atom(Atom::Natural(4)),
                                     box Expression::Sum(vec![
                                        Expression::Atom(Atom::PlainVariable('x')),
                                        Expression::Atom(Atom::Natural(1))]));
    assert_expected_eq_actual!(Ok(expected), parse_expr("4^{x+1}"));
}

#[test]
fn subscript() {
    let expected = Expression::Subscript(box Expression::Atom(Atom::Natural(4)),
                                         box Expression::Atom(Atom::PlainVariable('x')));
    assert_expected_eq_actual!(Ok(expected), parse_expr("4_x"));
}

#[test]
fn subscript_grouped() {
    let expected = Expression::Subscript(box Expression::Atom(Atom::Natural(4)),
                                         box Expression::Sum(vec![
                                            Expression::Atom(Atom::PlainVariable('x')),
                                            Expression::Atom(Atom::Natural(1))]));
    assert_expected_eq_actual!(Ok(expected), parse_expr("4_{x+1}"));
}

#[test]
fn equation() {
    let expected = Equation{ left: Expression::Atom(Atom::PlainVariable('x')),
            right: Expression::Atom(Atom::Natural(4)) };
    assert_expected_eq_actual!(Ok(expected), parse_equation("x=4"));
}

#[test]
fn sqrt() {
    let expected = Expression::Power(box Expression::Atom(Atom::PlainVariable('x')), box Expression::Atom(Atom::Floating(0.5)));
    assert_expected_eq_actual!(Ok(expected), parse_expr("\\sqrt x"));
}

#[test]
fn sin() {
    let expected = Expression::Application(box Expression::Atom(Atom::Symbol(Symbol::Operator(OperatorSymbol::sin))), box Expression::Atom(Atom::PlainVariable('x')));
    assert_expected_eq_actual!(Ok(expected), parse_expr("\\sin x"));
}
