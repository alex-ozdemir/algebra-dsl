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
                    mathTreeNode=\"0\"><mfrac><mrow multiparent=\"true\"><mrow \
                    mathTreeNode=\"0,0\"><mi>x</mi></mrow></mrow><mrow multiparent=\"true\"><mrow \
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
    let expr = Ex::Sum(vec![var('x'), var('y'), var('z')]);
    let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow \
                    mathTreeNode=\"0\" multiparent=\"true\"><mrow \
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
    let expected =
        "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow mathTreeNode=\"0\" \
         multiparent=\"true\"><mrow mathTreeNode=\"0,0\"><mi>x</mi></mrow><mo>+</mo><mrow \
         mathTreeNode=\"0,1\" multiparent=\"true\"><mo form=\"prefix\">(</mo><mrow \
         mathTreeNode=\"0,1,0\"><mi>y</mi></mrow><mo>+</mo><mrow \
         mathTreeNode=\"0,1,1\"><mi>z</mi></mrow><mo form=\"postfix\">)</mo></mrow></mrow></math>";
    let test = format!("{}", expr);
    assert_expected_eq_actual!(expected, test);
}

#[test]
fn format_prod() {
    let expr = Ex::Division(vec![var('x'), var('y'), var('z')], vec![]);
    let expected = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow \
                    mathTreeNode=\"0\" multiparent=\"true\"><mrow \
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
                                                   Ex::Atom(Atom::PlainVariable('z'))],
                                              vec![])],
                            vec![]);
    let expected =
        "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow mathTreeNode=\"0\" \
         multiparent=\"true\"><mrow mathTreeNode=\"0,0\"><mi>x</mi></mrow><mo>&#8290;</mo><mrow \
         mathTreeNode=\"0,1\" multiparent=\"true\"><mo form=\"prefix\">(</mo><mrow \
         mathTreeNode=\"0,1,0\"><mi>y</mi></mrow><mo>&#8290;</mo><mrow \
         mathTreeNode=\"0,1,1\"><mi>z</mi></mrow><mo form=\"postfix\">)</mo></mrow></mrow></math>";
    let test = format!("{}", expr);
    assert_expected_eq_actual!(expected, test);
}

#[test]
fn format_prod_add() {
    let expr = Ex::Division(vec![Ex::Sum(vec![var('x'), var('y')]),
                                 Ex::Sum(vec![var('z'), var('a')])],
                            vec![]);
    let expected =
        "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow mathTreeNode=\"0\" \
         multiparent=\"true\"><mrow mathTreeNode=\"0,0\" multiparent=\"true\"><mo \
         form=\"prefix\">(</mo><mrow mathTreeNode=\"0,0,0\"><mi>x</mi></mrow><mo>+</mo><mrow \
         mathTreeNode=\"0,0,1\"><mi>y</mi></mrow><mo \
         form=\"postfix\">)</mo></mrow><mo>&#8290;</mo><mrow mathTreeNode=\"0,1\" \
         multiparent=\"true\"><mo form=\"prefix\">(</mo><mrow \
         mathTreeNode=\"0,1,0\"><mi>z</mi></mrow><mo>+</mo><mrow \
         mathTreeNode=\"0,1,1\"><mi>a</mi></mrow><mo form=\"postfix\">)</mo></mrow></mrow></math>";
    let test = format!("{}", expr);
    assert_expected_eq_actual!(expected, test);
}

#[test]
fn make_siblings() {
    let mut before = Ex::Division(vec![nat(3), nat(4), var('x'), nat(7)], vec![]);
    let index_strings = vec!["#(mtn:0,0)", "#(mtn:0,1)", "#(mtn:0,3)"];
    let after = Ex::Division(vec![nat(84), var('x')], vec![]);
    let indices: Vec<_> =
        index_strings.into_iter().map(TreeIdx::from_str).map(Result::unwrap).collect();
    println!("{:#?}", indices);
    let siblings = before.sibling_indices(indices.as_slice()).unwrap();
    let replacement = nat(84);
    before.make_siblings(siblings, replacement).unwrap();
    assert_expected_eq_actual!(after, before);
}

#[test]
fn replace_division_all_top() {
    let mut before = Ex::Division(vec![nat(3), nat(4), nat(7)], vec![]);
    let index_strings = vec!["#(mtn:0,0)", "#(mtn:0,1)", "#(mtn:0,2)"];
    let after = nat(84);
    let indices: Vec<_> =
        index_strings.into_iter().map(TreeIdx::from_str).map(Result::unwrap).collect();
    let siblings = before.sibling_indices(indices.as_slice()).unwrap();
    let replacement = nat(84);
    before.make_siblings(siblings, replacement).unwrap();
    assert_expected_eq_actual!(after, before);
}

#[test]
fn replace_division_end_of_top() {
    let mut before = Ex::Division(vec![nat(3), nat(4), nat(7)], vec![]);
    let index_strings = vec!["#(mtn:0,1)", "#(mtn:0,2)"];
    let after = Ex::Division(vec![nat(3), nat(21)], vec![]);
    let indices: Vec<_> =
        index_strings.into_iter().map(TreeIdx::from_str).map(Result::unwrap).collect();
    let siblings = before.sibling_indices(indices.as_slice()).unwrap();
    let replacement = nat(21);
    before.make_siblings(siblings, replacement).unwrap();
    assert_expected_eq_actual!(after, before);
}

#[test]
fn replace_division_all_bottom() {
    let mut before = Ex::Division(vec![nat(3), nat(4)], vec![nat(6), nat(7)]);
    let index_strings = vec!["#(mtn:0,2)", "#(mtn:0,3)"];
    let after = Ex::Division(vec![nat(3), nat(4)], vec![nat(42)]);
    let indices: Vec<_> =
        index_strings.into_iter().map(TreeIdx::from_str).map(Result::unwrap).collect();
    let siblings = before.sibling_indices(indices.as_slice()).unwrap();
    let replacement = nat(42);
    before.make_siblings(siblings, replacement).unwrap();
    assert_expected_eq_actual!(after, before);
}

#[test]
fn replace_division_mid_top_mid_bottom() {
    let mut before = Ex::Division(vec![nat(3), nat(4), nat(4)], vec![nat(6), nat(4), nat(7)]);
    let index_strings = vec!["#(mtn:0,1)", "#(mtn:0,4)"];
    let after = Ex::Division(vec![nat(3), nat(42), nat(4)], vec![nat(6), nat(7)]);
    let indices: Vec<_> =
        index_strings.into_iter().map(TreeIdx::from_str).map(Result::unwrap).collect();
    let siblings = before.sibling_indices(indices.as_slice()).unwrap();
    let replacement = nat(42);
    before.make_siblings(siblings, replacement).unwrap();
    assert_expected_eq_actual!(after, before);
}

#[test]
fn make_siblings_sum() {
    let mut before = Ex::Sum(vec![nat(3), nat(4), var('x'), nat(7)]);
    let index_strings = vec!["#(mtn:0,0)", "#(mtn:0,1)", "#(mtn:0,3)"];
    let after = Ex::Sum(vec![nat(84), var('x')]);
    let indices: Vec<_> =
        index_strings.into_iter().map(TreeIdx::from_str).map(Result::unwrap).collect();
    println!("{:#?}", indices);
    let siblings = before.sibling_indices(indices.as_slice()).unwrap();
    let replacement = nat(84);
    before.make_siblings(siblings, replacement).unwrap();
    assert_expected_eq_actual!(after, before);
}

#[test]
fn make_siblings_sum_flatten() {
    let mut before = Ex::Sum(vec![nat(3), nat(4), var('x'), nat(7)]);
    let index_strings = vec!["#(mtn:0,0)", "#(mtn:0,1)", "#(mtn:0,3)"];
    let after = Ex::Sum(vec![var('z'), nat(84), var('x')]);
    let indices: Vec<_> =
        index_strings.into_iter().map(TreeIdx::from_str).map(Result::unwrap).collect();
    println!("{:#?}", indices);
    let siblings = before.sibling_indices(indices.as_slice()).unwrap();
    let replacement = Ex::Sum(vec![var('z'), nat(84)]);
    before.make_siblings(siblings, replacement).unwrap();
    assert_expected_eq_actual!(after, before);
}

#[test]
fn delete_expr() {
    let mut before = Ex::Division(vec![nat(3), nat(4), var('x'), nat(7)], vec![]);
    let v = vec![TreeIdx::from_str("#(mtn:0,1)").unwrap()];
    let idx = before.sibling_indices(v.as_slice()).unwrap();
    let after = Ex::Division(vec![nat(3), var('x'), nat(7)], vec![]);
    before.delete(idx).unwrap();
    assert_expected_eq_actual!(after, before);
}

#[test]
fn simplify_product() {
    let before = Ex::Division(vec![nat(3), nat(4), nat(7)], vec![]);
    let after = nat(84);
    assert_expected_eq_actual!(after, before.simplify_constants());

    let before = Ex::Division(vec![nat(3), float(4.0), nat(7)], vec![]);
    let after = float(84.0);
    assert_expected_eq_actual!(after, before.simplify_constants());
}

#[test]
fn simplify_sum() {
    let before = Ex::Sum(vec![nat(3), nat(4), nat(7)]);
    let after = nat(14);
    assert_expected_eq_actual!(after, before.simplify_constants());

    let before = Ex::Sum(vec![nat(3), float(4.0), nat(7)]);
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
    let idx = b.sibling_indices(v.as_slice()).unwrap();
    b.make_siblings(idx, Ex::Sum(vec![nat(0), nat(1)])).unwrap();
    assert_expected_eq_actual!(a, b);
}


#[test]
fn make_in_numerator_flatten() {
    let mut b = Ex::Division(vec![nat(1), nat(2)], vec![nat(4)]);
    let a = Ex::Division(vec![nat(0), nat(1), nat(2)], vec![nat(4)]);
    let v = vec![TreeIdx::from_str("#(mtn:0,0)").unwrap()];
    let idx = b.sibling_indices(v.as_slice()).unwrap();
    b.make_siblings(idx, Ex::Division(vec![nat(0), nat(1)], vec![])).unwrap();
    assert_expected_eq_actual!(a, b);
}

#[test]
fn make_in_denominator_flatten() {
    let mut b = Ex::Division(vec![nat(1), nat(2)], vec![nat(4)]);
    let a = Ex::Division(vec![nat(1), nat(2)], vec![nat(0), nat(4)]);
    let v = vec![TreeIdx::from_str("#(mtn:0,2)").unwrap()];
    let idx = b.sibling_indices(v.as_slice()).unwrap();
    b.make_siblings(idx, Ex::Division(vec![nat(0), nat(4)], vec![])).unwrap();
    assert_expected_eq_actual!(a, b);
}

#[test]
fn subtract_to_0() {
    let mut b = Eq {
        left: var('x'),
        right: nat(-5),
    };
    let a = Eq {
        left: Ex::Sum(vec![var('x'), nat(5)]),
        right: nat(0),
    };
    b.plus_to_both(nat(5));
    b = b.simplify_constants();
    assert_expected_eq_actual!(a, b);
}

#[test]
fn simple_ex_iter_test() {
    let expr = Ex::Power(box var('x'), box nat(2));
    let mut count = 0;
    for (idx, e) in expr.expr_iter() {
        count += 1;
        assert_eq!(e, expr.get(idx.as_ref()).unwrap());
    }
    assert_expected_eq_actual!(3, count);
}

#[test]
fn simple_eq_iter_test() {
    let equation = Eq {
        left: var('x'),
        right: nat(5),
    };
    let mut count = 0;
    for (idx, e) in equation.expr_iter() {
        count += 1;
        assert_eq!(e, equation.get(idx.as_ref()).unwrap());
    }
    assert_expected_eq_actual!(2, count);
}

#[test]
fn complex_eq_iter_test() {
    let sum = OperatorSymbol::sum;
    let equation = Eq {
        left: Ex::Power(box Ex::Subscript(box var('x'), box nat(0)), box nat(2)),
        right: Ex::Sum(vec![Ex::Division(vec![nat(7), var('y')], vec![]),
                            nat(8),
                            Ex::LimitOp(sum, None, None, box nat(9)),
                            Ex::LimitOp(sum, Some(box nat(5)), None, box nat(8)),
                            Ex::LimitOp(sum, None, Some(box nat(6)), box nat(7)),
                            Ex::LimitOp(sum, Some(box nat(4)), Some(box nat(3)), box nat(2))]),
    };
    let mut count = 0;
    println!("eq:\n{:#?}\n", equation);
    for (idx, e) in equation.expr_iter() {
        count += 1;
        println!("Idx `{:?}`, Expr:\n{:#?}\n", idx, e);
        assert_eq!(e, equation.get(idx.as_ref()).unwrap());
    }
    assert_expected_eq_actual!(5 + 4 + 1 + 2 + 3 + 3 + 4, count);
}

#[test]
fn simple_swap() {
    let mut e1 = Ex::Sum(vec![nat(1), nat(2)]);
    let e2 = Ex::Sum(vec![nat(2), nat(1)]);
    let i1 = TreeIdx::from_str("#(mtn:0,0)").unwrap();
    let i2 = TreeIdx::from_str("#(mtn:0,1)").unwrap();
    e1.swap(&i1, &i2).unwrap();
    assert_expected_eq_actual!(e2, e1);
}

#[test]
fn non_sibling_swap() {
    let mut e1 = Ex::Sum(vec![Ex::Power(box nat(5), box nat(1)), nat(2)]);
    let e2 = Ex::Sum(vec![Ex::Power(box nat(2), box nat(1)), nat(5)]);
    let i1 = TreeIdx::from_str("#(mtn:0,0,0)").unwrap();
    let i2 = TreeIdx::from_str("#(mtn:0,1)").unwrap();
    e1.swap(&i1, &i2).unwrap();
    assert_expected_eq_actual!(e2, e1);
}

#[test]
fn nested_swap() {
    let mut e1 = Ex::Sum(vec![Ex::Power(box nat(5), box nat(1)), nat(2)]);
    let i1 = TreeIdx::from_str("#(mtn:0,0,0)").unwrap();
    let i2 = TreeIdx::from_str("#(mtn:0,0)").unwrap();
    assert!(e1.swap(&i1, &i2).is_err());
    assert!(e1.swap(&i2, &i1).is_err());
}

#[test]
fn simple_replace_all() {
    let mut e = Ex::Power(box var('x'), box var('x'));
    let after = Ex::Power(box var('y'), box var('y'));
    let i1 = TreeIdx::from_str("#(mtn:0,0)").unwrap();
    let i2 = TreeIdx::from_str("#(mtn:0,1)").unwrap();
    let v = vec![i1.as_ref(), i2.as_ref()];
    e.replace_all(v.as_slice(), var('y')).unwrap();
    assert_expected_eq_actual!(after, e);
}

#[test]
fn sum_flatten_replace_all() {
    let mut e = Ex::Sum(vec![nat(2), nat(2)]);
    let after = Ex::Sum(vec![nat(1), nat(1), nat(1), nat(1)]);
    let i1 = TreeIdx::from_str("#(mtn:0,0)").unwrap();
    let i2 = TreeIdx::from_str("#(mtn:0,1)").unwrap();
    let v = vec![i1.as_ref(), i2.as_ref()];
    e.replace_all(v.as_slice(), Ex::Sum(vec![nat(1), nat(1)])).unwrap();
    assert_expected_eq_actual!(after, e);
}

#[test]
fn division_top_flatten_replace_all() {
    let mut e = Ex::Division(vec![nat(2), nat(2)], vec![var('t')]);
    let after = Ex::Division(vec![nat(1), nat(1), nat(1), nat(1)], vec![var('t')]);
    let i1 = TreeIdx::from_str("#(mtn:0,0)").unwrap();
    let i2 = TreeIdx::from_str("#(mtn:0,1)").unwrap();
    let v = vec![i1.as_ref(), i2.as_ref()];
    e.replace_all(v.as_slice(), Ex::Division(vec![nat(1), nat(1)], vec![])).unwrap();
    assert_expected_eq_actual!(after, e);
}

#[test]
fn division_bottom_flatten_replace_all() {
    let mut e = Ex::Division(vec![var('t')], vec![nat(2), nat(2)]);
    let after = Ex::Division(vec![var('t')], vec![nat(1), nat(1), nat(1), nat(1)]);
    let i1 = TreeIdx::from_str("#(mtn:0,1)").unwrap();
    let i2 = TreeIdx::from_str("#(mtn:0,2)").unwrap();
    let v = vec![i1.as_ref(), i2.as_ref()];
    e.replace_all(v.as_slice(), Ex::Division(vec![nat(1), nat(1)], vec![])).unwrap();
    assert_expected_eq_actual!(after, e);
}

#[test]
fn complex_replace_all() {
    let mut e = Ex::Sum(vec![nat(2), Ex::Power(box var('x'), box nat(2))]);
    let after = Ex::Sum(vec![nat(1), nat(1), Ex::Power(box var('x'), box Ex::Sum(vec![nat(1), nat(1)]))]);
    let i1 = TreeIdx::from_str("#(mtn:0,0)").unwrap();
    let i2 = TreeIdx::from_str("#(mtn:0,1,1)").unwrap();
    let v = vec![i1.as_ref(), i2.as_ref()];
    e.replace_all(v.as_slice(), Ex::Sum(vec![nat(1), nat(1)])).unwrap();
    assert_expected_eq_actual!(after, e);
}

