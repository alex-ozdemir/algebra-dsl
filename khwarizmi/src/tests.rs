use super::*;
use Expression as Ex;
use Equation as Eq;

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
fn format_power() {
    let expr = Ex::Power(box var('x'), box var('y'));
    let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow \
                    mathTreeNode=\"0\"><msup><mrow \
                    mathTreeNode=\"0,0\"><mi>x</mi></mrow><mrow \
                    mathTreeNode=\"0,1\"><mi>y</mi></mrow></msup></mrow></math>";
    let test = format!("{}", expr);
    assert_expected_eq_actual!(expected, test);
}

#[test]
fn index_power() {
    let expr = Ex::Power(box var('x'), box var('y'));
    let r = expr.get(TreeIdx(vec![1]).as_ref());
    let rr = &var('y');
    let e = Ok(rr);
    assert_expected_eq_actual!(e, r);
}

#[test]
fn format_frac() {
    let expr = Ex::Division(vec![var('x')], vec![var('y')]);
    let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow \
                    mathTreeNode=\"0\"><mfrac><mrow><mrow \
                    mathTreeNode=\"0,0\"><mi>x</mi></mrow></mrow><mrow><mrow \
                    mathTreeNode=\"0,1\"><mi>y</mi></mrow></mrow></mfrac></mrow></math>";
    let test = format!("{}", expr);
    assert_expected_eq_actual!(expected, test);
}

#[test]
fn format_negation() {
    let expr = Ex::Negation(box var('x'));
    let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow \
                    mathTreeNode=\"0\"><mo>-</mo><mrow \
                    mathTreeNode=\"0,0\"><mi>x</mi></mrow></mrow></math>";
    let test = format!("{}", expr);
    assert_expected_eq_actual!(expected, test);
}

#[test]
fn format_sub() {
    let expr = Ex::Subscript(box var('x'), box var('y'));
    let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow \
                    mathTreeNode=\"0\"><msub><mrow \
                    mathTreeNode=\"0,0\"><mi>x</mi></mrow><mrow \
                    mathTreeNode=\"0,1\"><mi>y</mi></mrow></msub></mrow></math>";
    let test = format!("{}", expr);
    assert_expected_eq_actual!(expected, test);
}

#[test]
fn format_add() {
    let expr = Ex::Sum(vec![var('x'),
                            var('y'),
                            var('z')]);
    let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow \
                    mathTreeNode=\"0\"><mrow \
                    mathTreeNode=\"0,0\"><mi>x</mi></mrow><mo>+</mo><mrow \
                    mathTreeNode=\"0,1\"><mi>y</mi></mrow><mo>+</mo><mrow \
                    mathTreeNode=\"0,2\"><mi>z</mi></mrow></mrow></math>";
    let test = format!("{}", expr);
    assert_expected_eq_actual!(expected, test);
}

#[test]
fn format_add_w_paren() {
    let expr = Ex::Sum(vec![Ex::Atom(Atom::PlainVariable('x')),
                            Ex::Sum(vec![Ex::Atom(Atom::PlainVariable('y')),
                            Ex::Atom(Atom::PlainVariable('z'))])]);
    let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow \
                    mathTreeNode=\"0\"><mrow \
                    mathTreeNode=\"0,0\"><mi>x</mi></mrow><mo>+</mo><mrow \
                    mathTreeNode=\"0,1\"><mo form=\"prefix\">(</mo><mrow \
                    mathTreeNode\"0,1,0\"><mi>y</mi></mrow><mo>+</mo><mrow \
                    mathTreeNode\"0,1,1\"><mi>z</mi></mrow></mrow><mo form=\"postfix\">)</mo></mrow></math>";
    let test = format!("{}", expr);
}

#[test]
fn format_prod() {
    let expr = Ex::Division(vec![var('x'),
                                var('y'),
                                var('z')],
                            vec![]);
    let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow \
                    mathTreeNode=\"0\"><mrow \
                    mathTreeNode=\"0,0\"><mi>x</mi></mrow><mo>&#8290;</mo><mrow \
                    mathTreeNode=\"0,1\"><mi>y</mi></mrow><mo>&#8290;</mo><mrow \
                    mathTreeNode=\"0,2\"><mi>z</mi></mrow></mrow></math>";
    let test = format!("{}", expr);
    assert_expected_eq_actual!(expected, test);
}

#[test]
fn format_prod_w_paren() {
    let expr = Ex::Division(vec![Ex::Atom(Atom::PlainVariable('x')),
                            Ex::Division(vec![Ex::Atom(Atom::PlainVariable('y')),
                            Ex::Atom(Atom::PlainVariable('z'))],vec![])],vec![]);
    let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow \
                    mathTreeNode=\"0\"><mrow \
                    mathTreeNode=\"0,0\"><mi>x</mi></mrow><mo>&#8290;</mo><mrow \
                    mathTreeNode=\"0,1\"><mo form=\"prefix\">(</mo><mrow \
                    mathTreeNode\"0,1,0\"><mi>y</mi></mrow><mo>&#8290;</mo><mrow \
                    mathTreeNode\"0,1,1\"><mi>z</mi></mrow></mrow><mo form=\"postfix\">)</mo></mrow></math>";
    let test = format!("{}", expr);
}

#[test]
fn format_prod_add() {
    let expr = Ex::Division(vec![Ex::Sum(vec![var('x'),
                                             var('y')]),
                                Ex::Sum(vec![var('z'),
                                             var('a')])],
                            vec![]);
    let expected =
        "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow mathTreeNode=\"0\"><mrow \
         mathTreeNode=\"0,0\"><mo form=\"prefix\">(</mo><mrow \
         mathTreeNode=\"0,0,0\"><mi>x</mi></mrow><mo>+</mo><mrow \
         mathTreeNode=\"0,0,1\"><mi>y</mi></mrow><mo \
         form=\"postfix\">)</mo></mrow><mo>&#8290;</mo><mrow mathTreeNode=\"0,1\"><mo \
         form=\"prefix\">(</mo><mrow mathTreeNode=\"0,1,0\"><mi>z</mi></mrow><mo>+</mo><mrow \
         mathTreeNode=\"0,1,1\"><mi>a</mi></mrow><mo form=\"postfix\">)</mo></mrow></mrow></math>";
    let test = format!("{}", expr);
    assert_expected_eq_actual!(expected, test);
}

#[test]
fn replace_siblings() {
    let mut before = Ex::Division(vec![nat(3),
                                      nat(4),
                                      var('x'),
                                      nat(7)],
                                  vec![]);
    let index_strings = vec!["#(mtn:0,0)", "#(mtn:0,1)", "#(mtn:0,3)"];
    let after = Ex::Division(vec![nat(84), var('x')], vec![]);
    let indices: Vec<_> =
        index_strings.into_iter().map(TreeIdx::from_str).map(Result::unwrap).collect();
    println!("{:#?}", indices);
    let siblings = before.make_siblings(indices.as_slice()).unwrap();
    let replacement = nat(84);
    before.replace_siblings(siblings, replacement).unwrap();
    assert_expected_eq_actual!(after, before);
}

#[test]
fn replace_division_all_top() {
    let mut before = Ex::Division(vec![nat(3),
                                      nat(4),
                                      nat(7)],
                                  vec![]);
    let index_strings = vec!["#(mtn:0,0)", "#(mtn:0,1)", "#(mtn:0,2)"];
    let after = nat(84);
    let indices: Vec<_> =
        index_strings.into_iter().map(TreeIdx::from_str).map(Result::unwrap).collect();
    let siblings = before.make_siblings(indices.as_slice()).unwrap();
    let replacement = nat(84);
    before.replace_siblings(siblings, replacement).unwrap();
    assert_expected_eq_actual!(after, before);
}

#[test]
fn replace_division_end_of_top() {
    let mut before = Ex::Division(vec![nat(3),
                                      nat(4),
                                      nat(7)],
                                  vec![]);
    let index_strings = vec!["#(mtn:0,1)", "#(mtn:0,2)"];
    let after = Ex::Division(vec![nat(3),
                                   nat(21)],
                             vec![]);
    let indices: Vec<_> =
        index_strings.into_iter().map(TreeIdx::from_str).map(Result::unwrap).collect();
    let siblings = before.make_siblings(indices.as_slice()).unwrap();
    let replacement = nat(21);
    before.replace_siblings(siblings, replacement).unwrap();
    assert_expected_eq_actual!(after, before);
}

#[test]
fn replace_division_all_bottom() {
    let mut before = Ex::Division(vec![nat(3),
                                       nat(4)],
                                  vec![nat(6),
                                       nat(7)]);
    let index_strings = vec!["#(mtn:0,2)", "#(mtn:0,3)"];
    let after = Ex::Division(vec![nat(3),
                                  nat(4)],
                             vec![nat(42)]);
    let indices: Vec<_> =
        index_strings.into_iter().map(TreeIdx::from_str).map(Result::unwrap).collect();
    let siblings = before.make_siblings(indices.as_slice()).unwrap();
    let replacement = nat(42);
    before.replace_siblings(siblings, replacement).unwrap();
    assert_expected_eq_actual!(after, before);
}

#[test]
fn replace_division_mid_top_mid_bottom() {
    let mut before = Ex::Division(vec![nat(3),
                                       nat(4),
                                       nat(4)],
                                  vec![nat(6),
                                       nat(4),
                                       nat(7)]);
    let index_strings = vec!["#(mtn:0,1)", "#(mtn:0,4)"];
    let after = Ex::Division(vec![nat(3),
                                  nat(42),
                                  nat(4)],
                             vec![nat(6),
                                  nat(7)]);
    let indices: Vec<_> =
        index_strings.into_iter().map(TreeIdx::from_str).map(Result::unwrap).collect();
    let siblings = before.make_siblings(indices.as_slice()).unwrap();
    let replacement = nat(42);
    before.replace_siblings(siblings, replacement).unwrap();
    assert_expected_eq_actual!(after, before);
}

#[test]
fn replace_siblings_sum() {
    let mut before = Ex::Sum(vec![nat(3),
                                  nat(4),
                                  var('x'),
                                  nat(7)]);
    let index_strings = vec!["#(mtn:0,0)", "#(mtn:0,1)", "#(mtn:0,3)"];
    let after = Ex::Sum(vec![nat(84), var('x')]);
    let indices: Vec<_> =
        index_strings.into_iter().map(TreeIdx::from_str).map(Result::unwrap).collect();
    println!("{:#?}", indices);
    let siblings = before.make_siblings(indices.as_slice()).unwrap();
    let replacement = nat(84);
    before.replace_siblings(siblings, replacement).unwrap();
    assert_expected_eq_actual!(after, before);
}

#[test]
fn delete_expr() {
    let mut before = Ex::Division(vec![nat(3),
                                      nat(4),
                                      var('x'),
                                      nat(7)],
                                  vec![]);
    let v = vec![TreeIdx::from_str("#(mtn:0,1)").unwrap()];
    let idx = before.make_siblings(v.as_slice()).unwrap();
    let after = Ex::Division(vec![nat(3),
                                 var('x'),
                                 nat(7)],
                             vec![]);
    before.delete(idx).unwrap();
    assert_expected_eq_actual!(after, before);
}

#[test]
fn simplify_product() {
    let before = Ex::Division(vec![nat(3),
                                  nat(4),
                                  nat(7)],
                              vec![]);
    let after = nat(84);
    assert_expected_eq_actual!(after, before.simplify_constants());

    let before = Ex::Division(vec![nat(3),
                                  float(4.0),
                                  nat(7)],
                              vec![]);
    let after = float(84.0);
    assert_expected_eq_actual!(after, before.simplify_constants());
}

#[test]
fn simplify_sum() {
    let before = Ex::Sum(vec![nat(3),
                              nat(4),
                              nat(7)]);
    let after = nat(14);
    assert_expected_eq_actual!(after, before.simplify_constants());

    let before = Ex::Sum(vec![nat(3),
                              float(4.0),
                              nat(7)]);
    let after = float(14.0);
    assert_expected_eq_actual!(after, before.simplify_constants());
}

#[test]
fn simplify_division() {
    let before = Ex::Division(vec![nat(8)], vec![nat(4)]);
    let after = nat(2);
    assert_expected_eq_actual!(after, before.simplify_constants());

    let before = Ex::Division(vec![float(8.)], vec![float(4.)]);
    let after = float(2.);
    assert_expected_eq_actual!(after, before.simplify_constants());

}

#[test]
fn simplify_power() {
    let before = Ex::Power(box nat(3), box nat(4));
    let after = nat(81);
    assert_expected_eq_actual!(after, before.simplify_constants());

    let before = Ex::Power(box float(3.), box nat(4));
    let after = float(81.);
    assert_expected_eq_actual!(after, before.simplify_constants());

}

#[test]
fn make_in_sum_flatten() {
    let mut b = Ex::Sum(vec![nat(1), nat(2), nat(3)]);
    let a = Ex::Sum(vec![nat(0), nat(1), nat(2), nat(3)]);
    let v = vec![TreeIdx::from_str("#(mtn:0,0)").unwrap()];
    let idx = b.make_siblings(v.as_slice()).unwrap();
    b.replace_siblings(idx, Ex::Sum(vec![nat(0), nat(1)])).unwrap();
    assert_expected_eq_actual!(a, b);
}


#[test]
fn make_in_numerator_flatten() {
    let mut b = Ex::Division(vec![nat(1), nat(2)], vec![nat(4)]);
    let a = Ex::Division(vec![nat(0), nat(1), nat(2)], vec![nat(4)]);
    let v = vec![TreeIdx::from_str("#(mtn:0,0)").unwrap()];
    let idx = b.make_siblings(v.as_slice()).unwrap();
    b.replace_siblings(idx, Ex::Division(vec![nat(0), nat(1)], vec![])).unwrap();
    assert_expected_eq_actual!(a, b);
}

#[test]
fn make_in_denominator_flatten() {
    let mut b = Ex::Division(vec![nat(1), nat(2)], vec![nat(4)]);
    let a = Ex::Division(vec![nat(1), nat(2)], vec![nat(0), nat(4)]);
    let v = vec![TreeIdx::from_str("#(mtn:0,2)").unwrap()];
    let idx = b.make_siblings(v.as_slice()).unwrap();
    b.replace_siblings(idx, Ex::Division(vec![nat(0), nat(4)], vec![])).unwrap();
    assert_expected_eq_actual!(a, b);
}

#[test]
fn subtract_to_0() {
    let mut b = Eq{ left: var('x'), right: nat(-5) };
    let a = Eq{ left: Ex::Sum(vec![var('x'), nat(5)]), right: nat(0) };
    b.plus_to_both(nat(5));
    b = b.simplify_constants();
    assert_expected_eq_actual!(a, b);
}
