#[cfg(test)]
use flatpint_parser as fyp;

#[cfg(test)]
use lalrpop_util::lalrpop_mod;

#[cfg(test)]
lalrpop_mod!(#[allow(unused, clippy::empty_line_after_outer_attr)] pub flatpint_parser);

#[cfg(test)]
fn check(actual: &str, expect: expect_test::Expect) {
    expect.assert_eq(actual);
}

#[cfg(test)]
macro_rules! run_solver {
    ($parser: expr, $source: expr) => {{
        let flatpint = $parser.parse($source).unwrap();
        let solver = crate::solver(&flatpint);
        match solver.solve() {
            Ok(solver) => solver.display_solution_raw(),
            Err(err) => format!("{err}"),
        }
    }};
}

#[test]
fn trivial_bool() {
    let src = r#"
var x: bool;
var y: bool;
constraint ((x && y) == true);
solve satisfy;
"#;

    check(
        &run_solver!(fyp::FlatPintParser::new(), src),
        expect_test::expect![[r#"
            x: 1
            y: 1
        "#]],
    );
}

#[test]
fn trivial_int() {
    let src = r#"
var x: int;
var y: int;
constraint ((x + y) == 4);
constraint (x > 1);
solve satisfy;
"#;

    check(
        &run_solver!(fyp::FlatPintParser::new(), src),
        expect_test::expect![[r#"
            x: 2
            y: 2
        "#]],
    );
}

#[test]
fn trivial_real() {
    let src = r#"
var x: real;
var y: real;
constraint ((x + y) == 4.5);
constraint (x >= 1.0);
solve satisfy;
"#;

    check(
        &run_solver!(fyp::FlatPintParser::new(), src),
        expect_test::expect![[r#"
            x: 4.500
            y: 0.000
        "#]],
    );
}

#[test]
fn maximize() {
    let src = r#"
var x: real;
var y: real;
var obj: real;
constraint ((x + y) == 4.5);
constraint (obj == (x * y));
solve maximize obj;
"#;

    check(
        &run_solver!(fyp::FlatPintParser::new(), src),
        expect_test::expect![[r#"
            x: 2.250
            y: 2.250
            obj: 5.063
            objective: 5.063"#]],
    );
}

#[test]
fn minimize() {
    let src = r#"
var x: int;
var obj: int;
constraint (x >= -10);
constraint (obj == (((x * x) + (2 * x)) - 8));
solve minimize obj;
"#;

    check(
        &run_solver!(fyp::FlatPintParser::new(), src),
        expect_test::expect![[r#"
            x: -1
            obj: -9
            objective: -9.000"#]],
    );
}
