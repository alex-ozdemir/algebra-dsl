use super::*;
use Expression as Ex;

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
fn format_power() {
    let expr = Ex::Power(box Ex::Atom(Atom::PlainVariable('x')),
                         box Ex::Atom(Atom::PlainVariable('y')));
    let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow \
                    mathTreeNode=\"0\"><msup><mrow \
                    mathTreeNode=\"0,0\"><mi>x</mi></mrow><mrow \
                    mathTreeNode=\"0,1\"><mi>y</mi></mrow></msup></mrow></math>";
    let test = format!("{}", expr);
    assert_expected_eq_actual!(expected, test);
}

#[test]
fn index_power() {
    let expr = Ex::Power(box Ex::Atom(Atom::PlainVariable('x')),
                         box Ex::Atom(Atom::PlainVariable('y')));
    let r = expr.get(TreeIdx(vec![1]).as_ref());
    let rr = &Ex::Atom(Atom::PlainVariable('y'));
    let e = Ok(rr);
    assert_expected_eq_actual!(e, r);
}

#[test]
fn format_frac() {
    let expr = Ex::Division(vec![Ex::Atom(Atom::PlainVariable('x'))],
                            vec![Ex::Atom(Atom::PlainVariable('y'))]);
    let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow \
                    mathTreeNode=\"0\"><mfrac><mrow><mrow mathTreeNode=\"0,0\"><mi>x</mi></mrow></mrow><mrow><mrow \
                    mathTreeNode=\"0,1\"><mi>y</mi></mrow></mrow></mfrac></mrow></math>";
    let test = format!("{}", expr);
    assert_expected_eq_actual!(expected, test);
}

#[test]
fn format_negation() {
    let expr = Ex::Negation(box Ex::Atom(Atom::PlainVariable('x')));
    let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow \
                    mathTreeNode=\"0\"><mo>-</mo><mrow \
                    mathTreeNode=\"0,0\"><mi>x</mi></mrow></mrow></math>";
    let test = format!("{}", expr);
    assert_expected_eq_actual!(expected, test);
}

#[test]
fn format_sub() {
    let expr = Ex::Subscript(box Ex::Atom(Atom::PlainVariable('x')),
                             box Ex::Atom(Atom::PlainVariable('y')));
    let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow \
                    mathTreeNode=\"0\"><msub><mrow \
                    mathTreeNode=\"0,0\"><mi>x</mi></mrow><mrow \
                    mathTreeNode=\"0,1\"><mi>y</mi></mrow></msub></mrow></math>";
    let test = format!("{}", expr);
    assert_expected_eq_actual!(expected, test);
}

#[test]
fn format_add() {
    let expr = Ex::Sum(vec![Ex::Atom(Atom::PlainVariable('x')),
                            Ex::Atom(Atom::PlainVariable('y')),
                            Ex::Atom(Atom::PlainVariable('z'))]);
    let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow \
                    mathTreeNode=\"0\"><mrow \
                    mathTreeNode=\"0,0\"><mi>x</mi></mrow><mo>+</mo><mrow \
                    mathTreeNode=\"0,1\"><mi>y</mi></mrow><mo>+</mo><mrow \
                    mathTreeNode=\"0,2\"><mi>z</mi></mrow></mrow></math>";
    let test = format!("{}", expr);
    assert_expected_eq_actual!(expected, test);
}

#[test]
fn format_prod() {
    let expr = Ex::Division(vec![Ex::Atom(Atom::PlainVariable('x')),
                                Ex::Atom(Atom::PlainVariable('y')),
                                Ex::Atom(Atom::PlainVariable('z'))], vec![]);
    let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow \
                    mathTreeNode=\"0\"><mrow \
                    mathTreeNode=\"0,0\"><mi>x</mi></mrow><mo>&#8290;</mo><mrow \
                    mathTreeNode=\"0,1\"><mi>y</mi></mrow><mo>&#8290;</mo><mrow \
                    mathTreeNode=\"0,2\"><mi>z</mi></mrow></mrow></math>";
    let test = format!("{}", expr);
    assert_expected_eq_actual!(expected, test);
}

#[test]
fn format_prod_add() {
    let expr = Ex::Division(vec![Ex::Sum(vec![Ex::Atom(Atom::PlainVariable('x')),
                                             Ex::Atom(Atom::PlainVariable('y'))]),
                                Ex::Sum(vec![Ex::Atom(Atom::PlainVariable('z')),
                                             Ex::Atom(Atom::PlainVariable('a'))])], vec![]);
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
    let mut before = Ex::Division(vec![Ex::Atom(Atom::Natural(3)),
                                      Ex::Atom(Atom::Natural(4)),
                                      Ex::Atom(Atom::PlainVariable('x')),
                                      Ex::Atom(Atom::Natural(7))], vec![]);
    let index_strings = vec!["#(mtn:0,0)", "#(mtn:0,1)", "#(mtn:0,3)"];
    let after = Ex::Division(vec![Ex::Atom(Atom::Natural(84)), Ex::Atom(Atom::PlainVariable('x'))], vec![]);
    let indices: Vec<_> =
        index_strings.into_iter().map(TreeIdx::from_str).map(Result::unwrap).collect();
    println!("{:#?}", indices);
    let siblings = before.make_siblings(indices.as_slice()).unwrap();
    let replacement = Ex::Atom(Atom::Natural(84));
    before.replace_siblings(siblings, replacement).unwrap();
    assert_expected_eq_actual!(after, before);
}

#[test]
fn replace_division_all_top() {
    let mut before = Ex::Division(vec![Ex::Atom(Atom::Natural(3)),
                                      Ex::Atom(Atom::Natural(4)),
                                      Ex::Atom(Atom::Natural(7))], vec![]);
    let index_strings = vec!["#(mtn:0,0)", "#(mtn:0,1)", "#(mtn:0,2)"];
    let after = Ex::Atom(Atom::Natural(84));
    let indices: Vec<_> =
        index_strings.into_iter().map(TreeIdx::from_str).map(Result::unwrap).collect();
    let siblings = before.make_siblings(indices.as_slice()).unwrap();
    let replacement = Ex::Atom(Atom::Natural(84));
    before.replace_siblings(siblings, replacement).unwrap();
    assert_expected_eq_actual!(after, before);
}

#[test]
fn replace_division_end_of_top() {
    let mut before = Ex::Division(vec![Ex::Atom(Atom::Natural(3)),
                                      Ex::Atom(Atom::Natural(4)),
                                      Ex::Atom(Atom::Natural(7))], vec![]);
    let index_strings = vec!["#(mtn:0,1)", "#(mtn:0,2)"];
    let after = Ex::Division(vec![Ex::Atom(Atom::Natural(3)),
                                   Ex::Atom(Atom::Natural(21))], vec![]);
    let indices: Vec<_> =
        index_strings.into_iter().map(TreeIdx::from_str).map(Result::unwrap).collect();
    let siblings = before.make_siblings(indices.as_slice()).unwrap();
    let replacement = Ex::Atom(Atom::Natural(21));
    before.replace_siblings(siblings, replacement).unwrap();
    assert_expected_eq_actual!(after, before);
}

#[test]
fn replace_division_all_bottom() {
    let mut before = Ex::Division(vec![Ex::Atom(Atom::Natural(3)),
                                       Ex::Atom(Atom::Natural(4))],
                                  vec![Ex::Atom(Atom::Natural(6)),
                                       Ex::Atom(Atom::Natural(7))]);
    let index_strings = vec!["#(mtn:0,2)", "#(mtn:0,3)"];
    let after = Ex::Division(vec![Ex::Atom(Atom::Natural(3)),
                                  Ex::Atom(Atom::Natural(4))],
                             vec![Ex::Atom(Atom::Natural(42))]);
    let indices: Vec<_> =
        index_strings.into_iter().map(TreeIdx::from_str).map(Result::unwrap).collect();
    let siblings = before.make_siblings(indices.as_slice()).unwrap();
    let replacement = Ex::Atom(Atom::Natural(42));
    before.replace_siblings(siblings, replacement).unwrap();
    assert_expected_eq_actual!(after, before);
}

#[test]
fn replace_division_mid_top_mid_bottom() {
    let mut before = Ex::Division(vec![Ex::Atom(Atom::Natural(3)),
                                       Ex::Atom(Atom::Natural(4)),
                                       Ex::Atom(Atom::Natural(4))],
                                  vec![Ex::Atom(Atom::Natural(6)),
                                       Ex::Atom(Atom::Natural(4)),
                                       Ex::Atom(Atom::Natural(7))]);
    let index_strings = vec!["#(mtn:0,1)", "#(mtn:0,4)"];
    let after = Ex::Division(vec![Ex::Atom(Atom::Natural(3)),
                                  Ex::Atom(Atom::Natural(42)),
                                  Ex::Atom(Atom::Natural(4))],
                             vec![Ex::Atom(Atom::Natural(6)),
                                  Ex::Atom(Atom::Natural(7))]);
    let indices: Vec<_> =
        index_strings.into_iter().map(TreeIdx::from_str).map(Result::unwrap).collect();
    let siblings = before.make_siblings(indices.as_slice()).unwrap();
    let replacement = Ex::Atom(Atom::Natural(42));
    before.replace_siblings(siblings, replacement).unwrap();
    assert_expected_eq_actual!(after, before);
}

#[test]
fn replace_siblings_sum() {
    let mut before = Ex::Sum(vec![Ex::Atom(Atom::Natural(3)),
                                  Ex::Atom(Atom::Natural(4)),
                                  Ex::Atom(Atom::PlainVariable('x')),
                                  Ex::Atom(Atom::Natural(7))]);
    let index_strings = vec!["#(mtn:0,0)", "#(mtn:0,1)", "#(mtn:0,3)"];
    let after = Ex::Sum(vec![Ex::Atom(Atom::Natural(84)), Ex::Atom(Atom::PlainVariable('x'))]);
    let indices: Vec<_> =
        index_strings.into_iter().map(TreeIdx::from_str).map(Result::unwrap).collect();
    println!("{:#?}", indices);
    let siblings = before.make_siblings(indices.as_slice()).unwrap();
    let replacement = Ex::Atom(Atom::Natural(84));
    before.replace_siblings(siblings, replacement).unwrap();
    assert_expected_eq_actual!(after, before);
}

#[test]
fn delete_expr() {
    let mut before = Ex::Division(vec![Ex::Atom(Atom::Natural(3)),
                                      Ex::Atom(Atom::Natural(4)),
                                      Ex::Atom(Atom::PlainVariable('x')),
                                      Ex::Atom(Atom::Natural(7))], vec![]);
    let v = vec![TreeIdx::from_str("#(mtn:0,1)").unwrap()];
    let idx = before.make_siblings(v.as_slice()).unwrap();
    let after = Ex::Division(vec![Ex::Atom(Atom::Natural(3)),
                                 Ex::Atom(Atom::PlainVariable('x')),
                                 Ex::Atom(Atom::Natural(7))], vec![]);
    before.delete(idx).unwrap();
    assert_expected_eq_actual!(after, before);
}

#[test]
fn simplify_product() {
    let before = Ex::Division(vec![Ex::Atom(Atom::Natural(3)),
                                  Ex::Atom(Atom::Natural(4)),
                                  Ex::Atom(Atom::Natural(7))], vec![]);
    let after = Ex::Atom(Atom::Natural(84));
    assert_expected_eq_actual!(after, before.simplify_constants());

    let before = Ex::Division(vec![Ex::Atom(Atom::Natural(3)),
                                  Ex::Atom(Atom::Floating(4.0)),
                                  Ex::Atom(Atom::Natural(7))], vec![]);
    let after = Ex::Atom(Atom::Floating(84.0));
    assert_expected_eq_actual!(after, before.simplify_constants());
}

#[test]
fn simplify_sum() {
    let before = Ex::Sum(vec![Ex::Atom(Atom::Natural(3)),
                              Ex::Atom(Atom::Natural(4)),
                              Ex::Atom(Atom::Natural(7))]);
    let after = Ex::Atom(Atom::Natural(14));
    assert_expected_eq_actual!(after, before.simplify_constants());

    let before = Ex::Sum(vec![Ex::Atom(Atom::Natural(3)),
                              Ex::Atom(Atom::Floating(4.0)),
                              Ex::Atom(Atom::Natural(7))]);
    let after = Ex::Atom(Atom::Floating(14.0));
    assert_expected_eq_actual!(after, before.simplify_constants());
}

#[test]
fn simplify_division() {
    let before = Ex::Division(vec![Ex::Atom(Atom::Natural(8))],
                              vec![Ex::Atom(Atom::Natural(4))]);
    let after = Ex::Atom(Atom::Natural(2));
    assert_expected_eq_actual!(after, before.simplify_constants());

    let before = Ex::Division(vec![Ex::Atom(Atom::Floating(8.))],
                              vec![Ex::Atom(Atom::Floating(4.))]);
    let after = Ex::Atom(Atom::Floating(2.));
    assert_expected_eq_actual!(after, before.simplify_constants());

}

#[test]
fn simplify_power() {
    let before = Ex::Power(box Ex::Atom(Atom::Natural(3)),
                           box Ex::Atom(Atom::Natural(4)));
    let after = Ex::Atom(Atom::Natural(81));
    assert_expected_eq_actual!(after, before.simplify_constants());

    let before = Ex::Power(box Ex::Atom(Atom::Floating(3.)),
                           box Ex::Atom(Atom::Natural(4)));
    let after = Ex::Atom(Atom::Floating(81.));
    assert_expected_eq_actual!(after, before.simplify_constants());

}
