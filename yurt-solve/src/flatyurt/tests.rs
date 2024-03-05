#[cfg(test)]
use flatyurt_parser as fyp;

#[cfg(test)]
use lalrpop_util::lalrpop_mod;

#[cfg(test)]
lalrpop_mod!(#[allow(unused)] pub flatyurt_parser);

#[cfg(test)]
fn check(actual: &str, expect: expect_test::Expect) {
    expect.assert_eq(actual);
}

#[cfg(test)]
macro_rules! run_parser {
    ($parser: expr, $source: expr) => {{
        match $parser.parse($source) {
            Ok(parsed) => format!("{parsed}"),
            Err(err) => format!("{err}"),
        }
    }};
}

#[test]
fn types() {
    let type_ = fyp::TypeParser::new();

    check(&run_parser!(type_, "int"), expect_test::expect!["int"]);

    check(&run_parser!(type_, "real"), expect_test::expect!["real"]);

    check(&run_parser!(type_, "bool"), expect_test::expect!["bool"]);
}

#[test]
fn immediates() {
    let immediate = fyp::ImmediateParser::new();

    // Bools
    check(
        &run_parser!(immediate, "true"),
        expect_test::expect!["true"],
    );

    check(
        &run_parser!(immediate, "false"),
        expect_test::expect!["false"],
    );

    // Integers
    check(&run_parser!(immediate, "1"), expect_test::expect!["1"]);

    check(
        &run_parser!(immediate, "1152921504606846975"),
        expect_test::expect!["1152921504606846975"],
    );

    // Reals
    check(
        &run_parser!(immediate, "1.3"),
        expect_test::expect!["1.3e0"],
    );

    check(&run_parser!(immediate, "1e6"), expect_test::expect!["1e6"]);

    check(
        &run_parser!(immediate, "1.3e6"),
        expect_test::expect!["1.3e6"],
    );

    check(
        &run_parser!(immediate, "111152921504606846975"),
        expect_test::expect!["integer literal is too large"],
    );
    // Errors
}

#[test]
fn var_decls() {
    let var = fyp::VarParser::new();
    check(
        &run_parser!(var, "var my_var: bool;"),
        expect_test::expect!["var my_var: bool;"],
    );

    check(
        &run_parser!(var, "var my_var: int;"),
        expect_test::expect!["var my_var: int;"],
    );

    check(
        &run_parser!(var, "var my_var: real;"),
        expect_test::expect!["var my_var: real;"],
    );

    check(
        &run_parser!(var, "var my_var: real"),
        expect_test::expect![[r#"
            Unrecognized EOF found at 16
            Expected one of ";""#]],
    );

    check(
        &run_parser!(var, "var my_var: some_type;"),
        expect_test::expect![[r#"
            Unrecognized token `some_type` found at 12:21
            Expected one of "bool", "int" or "real""#]],
    );

    check(
        &run_parser!(var, "my_var: int;"),
        expect_test::expect![[r#"
            Unrecognized token `my_var` found at 0:6
            Expected one of "var""#]],
    );
}

#[test]
fn constraint_decls() {
    // Argument just needs to be any expression, as far as the parser is concerned.
    let constraint = fyp::ConstraintParser::new();

    check(
        &run_parser!(constraint, "constraint (blah && true);"),
        expect_test::expect!["constraint (blah && true);"],
    );

    check(
        &run_parser!(constraint, "constraint (blah && true)"),
        expect_test::expect![[r#"
            Unrecognized EOF found at 25
            Expected one of ";""#]],
    );
}

#[test]
fn solve_decls() {
    let solve = fyp::SolveParser::new();

    check(
        &run_parser!(solve, "solve satisfy;"),
        expect_test::expect!["solve satisfy;"],
    );

    check(
        &run_parser!(solve, "solve minimize foo;"),
        expect_test::expect!["solve minimize foo;"],
    );

    check(
        &run_parser!(solve, "solve maximize foo;"),
        expect_test::expect!["solve maximize foo;"],
    );

    check(
        &run_parser!(solve, "solve maximize foo"),
        expect_test::expect![[r#"
            Unrecognized EOF found at 18
            Expected one of "::" or ";""#]],
    );

    check(
        &run_parser!(solve, "solve maximize (x + y);"),
        expect_test::expect![[r##"
            Unrecognized token `(` found at 15:16
            Expected one of "::" or r#"[A-Za-z_][A-Za-z_0-9\\[\\]\\.]*"#"##]],
    );

    check(
        &run_parser!(solve, "solve world hunger;"),
        expect_test::expect![[r#"
            Unrecognized token `world` found at 6:11
            Expected one of "maximize", "minimize" or "satisfy""#]],
    );
}

#[test]
fn basic_exprs() {
    let expr = fyp::ExprParser::new();
    check(&run_parser!(expr, "true"), expect_test::expect!["true"]);

    check(&run_parser!(expr, "false"), expect_test::expect!["false"]);

    check(&run_parser!(expr, "123"), expect_test::expect!["123"]);

    check(&run_parser!(expr, "12.3"), expect_test::expect!["1.23e1"]);

    check(&run_parser!(expr, "foo"), expect_test::expect!["foo"]);
}

#[test]
fn unary_op_exprs() {
    let expr = fyp::ExprParser::new();

    check(&run_parser!(expr, "!a"), expect_test::expect!["!a"]);

    check(&run_parser!(expr, "-a"), expect_test::expect!["-a"]);

    check(&run_parser!(expr, "-1.0"), expect_test::expect!["-1e0"]);

    check(&run_parser!(expr, "-1"), expect_test::expect!["-1"]);

    check(
        &run_parser!(expr, "! - - !  --  -1"),
        expect_test::expect!["!--!---1"],
    );

    check(&run_parser!(expr, "! (- x)"), expect_test::expect!["!-x"]);

    check(
        &run_parser!(expr, "+x"),
        expect_test::expect![[r##"
            Unrecognized token `+` found at 0:1
            Expected one of "!", "(", "-", "::", "false", "true", r#"[0-9]+"#, r#"[0-9]+\\.[0-9]+([Ee][-+]?[0-9]+)?|[0-9]+[Ee][-+]?[0-9]+"# or r#"[A-Za-z_][A-Za-z_0-9\\[\\]\\.]*"#"##]],
    );
}

#[test]
fn binary_op_exprs() {
    let expr = fyp::ExprParser::new();

    check(
        &run_parser!(expr, "(a * 2.0)"),
        expect_test::expect!["(a * 2e0)"],
    );

    check(
        &run_parser!(expr, "(a / 2.0)"),
        expect_test::expect!["(a / 2e0)"],
    );

    check(
        &run_parser!(expr, "(a % 2.0)"),
        expect_test::expect!["(a % 2e0)"],
    );

    check(
        &run_parser!(expr, "(a + 2.0)"),
        expect_test::expect!["(a + 2e0)"],
    );

    check(
        &run_parser!(expr, "(a - 2.0)"),
        expect_test::expect!["(a - 2e0)"],
    );

    check(
        &run_parser!(expr, "(a+2.0)"),
        expect_test::expect!["(a + 2e0)"],
    );

    check(
        &run_parser!(expr, "(a-2.0)"),
        expect_test::expect!["(a - 2e0)"],
    );

    check(
        &run_parser!(expr, "(a < 2.0)"),
        expect_test::expect!["(a < 2e0)"],
    );

    check(
        &run_parser!(expr, "(a > 2.0)"),
        expect_test::expect!["(a > 2e0)"],
    );

    check(
        &run_parser!(expr, "(a <= 2.0)"),
        expect_test::expect!["(a <= 2e0)"],
    );

    check(
        &run_parser!(expr, "(a >= 2.0)"),
        expect_test::expect!["(a >= 2e0)"],
    );

    check(
        &run_parser!(expr, "(a == 2.0)"),
        expect_test::expect!["(a == 2e0)"],
    );

    check(
        &run_parser!(expr, "(a != 2.0)"),
        expect_test::expect!["(a != 2e0)"],
    );

    check(
        &run_parser!(expr, "(a && b)"),
        expect_test::expect!["(a && b)"],
    );

    check(
        &run_parser!(expr, "(a || b)"),
        expect_test::expect!["(a || b)"],
    );

    check(
        &run_parser!(expr, "a || b"),
        expect_test::expect!["Unrecognized token `||` found at 2:4"],
    );
}

#[test]
fn complex_exprs() {
    let expr = fyp::ExprParser::new();

    check(
        &run_parser!(expr, "(2 + (b * 3))"),
        expect_test::expect!["(2 + (b * 3))"],
    );

    check(
        &run_parser!(expr, "((2 + b) * 3)"),
        expect_test::expect!["((2 + b) * 3)"],
    );

    check(
        &run_parser!(expr, "((2 < b) * 3)"),
        expect_test::expect!["((2 < b) * 3)"],
    );

    check(
        &run_parser!(expr, "(2.0 > (b * 3.0))"),
        expect_test::expect!["(2e0 > (b * 3e0))"],
    );

    check(
        &run_parser!(expr, "((2.0 * b) < 3.0)"),
        expect_test::expect!["((2e0 * b) < 3e0)"],
    );

    check(
        &run_parser!(expr, "(2 > (b < 3))"),
        expect_test::expect!["(2 > (b < 3))"],
    );

    check(
        &run_parser!(expr, "((2 != b) < 3)"),
        expect_test::expect!["((2 != b) < 3)"],
    );

    check(
        &run_parser!(expr, "((2 < b) != 3)"),
        expect_test::expect!["((2 < b) != 3)"],
    );

    check(
        &run_parser!(expr, "(a > ((b * c) < d))"),
        expect_test::expect!["(a > ((b * c) < d))"],
    );

    check(
        &run_parser!(expr, "((2 + 3) * 4)"),
        expect_test::expect!["((2 + 3) * 4)"],
    );

    check(
        &run_parser!(expr, "(10 - (8 / 4))"),
        expect_test::expect!["(10 - (8 / 4))"],
    );

    check(
        &run_parser!(expr, "((10 + 8) % 4)"),
        expect_test::expect!["((10 + 8) % 4)"],
    );

    check(
        &run_parser!(expr, "(((2 + 3) * 4) < 5)"),
        expect_test::expect!["(((2 + 3) * 4) < 5)"],
    );

    check(
        &run_parser!(expr, "(((2 + 3) * (4 - 5)) / (2 > 1))"),
        expect_test::expect!["(((2 + 3) * (4 - 5)) / (2 > 1))"],
    );

    check(
        &run_parser!(expr, "((a || b) && ((c || d) && !e))"),
        expect_test::expect!["((a || b) && ((c || d) && !e))"],
    );

    check(
        &run_parser!(expr, "(a || b || c)"),
        expect_test::expect![[r#"
            Unrecognized token `||` found at 8:10
            Expected one of ")""#]],
    );
}

#[test]
fn names() {
    let name = fyp::NameParser::new();

    check(
        &run_parser!(name, "::foobar"),
        expect_test::expect!["::foobar"],
    );

    check(
        &run_parser!(name, "foo::_bar"),
        expect_test::expect!["foo::_bar"],
    );

    check(
        &run_parser!(name, "FOO_bar"),
        expect_test::expect!["FOO_bar"],
    );

    check(&run_parser!(name, "__FOO"), expect_test::expect!["__FOO"]);

    check(
        &run_parser!(name, "_2_FOO1[]"),
        expect_test::expect!["_2_FOO1[]"],
    );

    check(&run_parser!(name, "_"), expect_test::expect!["_"]);

    check(
        &run_parser!(name, "__12_ab.::_3[1.2][.8"),
        expect_test::expect!["__12_ab.::_3[1.2][.8"],
    );

    check(
        &run_parser!(name, "foo::"),
        expect_test::expect![[r##"
        Unrecognized EOF found at 5
        Expected one of r#"[A-Za-z_][A-Za-z_0-9\\[\\]\\.]*"#"##]],
    );

    check(
        &run_parser!(name, "12_12"),
        expect_test::expect![[r##"
        Unrecognized token `12` found at 0:2
        Expected one of "::" or r#"[A-Za-z_][A-Za-z_0-9\\[\\]\\.]*"#"##]],
    );

    check(
        // Lexer will split this into 3 tokens, ident will parse the first one.
        // This shows that we're not able to parser `ab*cd` as a single identifier
        &run_parser!(name, "ab*cd"),
        expect_test::expect![[r#"
            Unrecognized token `*` found at 2:3
            Expected one of "::""#]],
    );
}

#[test]
fn basic_program() {
    let src = r#"
var low_val: real;
var high_val: int;
var mid: bool;

constraint (low_val == 1.23);
constraint (high_val == 4);

constraint (mid > (low_val * 2.0));
constraint (mid < high_val);

solve minimize mid;
"#;

    check(
        &run_parser!(fyp::FlatYurtParser::new(), src),
        expect_test::expect![[r#"
            var low_val: real;
            var high_val: int;
            var mid: bool;
            constraint (low_val == 1.23e0);
            constraint (high_val == 4);
            constraint (mid > (low_val * 2e0));
            constraint (mid < high_val);
            solve minimize mid;
        "#]],
    );
}
