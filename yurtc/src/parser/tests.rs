use crate::{
    error::ReportableError,
    lexer::{self, KEYWORDS},
    parser::*,
};
use chumsky::Stream;
use std::{path::Path, rc::Rc};

#[cfg(test)]
macro_rules! run_parser {
    ($parser: expr, $source: expr) => {{
        let filepath = Rc::from(Path::new("test"));
        let (toks, errs) = lexer::lex($source, Rc::clone(&filepath));
        assert!(errs.is_empty());
        let token_stream = Stream::from_iter(
            Span::new(filepath, $source.len()..$source.len()),
            toks.into_iter(),
        );
        match ($parser.then_ignore(end())).parse(token_stream) {
            Ok(ast) => format!("{ast}"),
            Err(errors) => format!(
                "{}",
                // Print each error on one line. For each error, start with the span.
                errors.iter().fold(String::new(), |acc, error| {
                    let mut all_diagnostics = format!("{}{}", acc, error);
                    all_diagnostics = format!(
                        "{}{}",
                        all_diagnostics,
                        error.labels().iter().fold(String::new(), |acc, label| {
                            format!(
                                "\n{}@{}..{}: {}\n",
                                acc,
                                label.span.start(),
                                label.span.end(),
                                label.message
                            )
                        })
                    );
                    if let Some(note) = error.note() {
                        all_diagnostics = format!("{}{}", all_diagnostics, note);
                    }
                    if let Some(help) = error.help() {
                        all_diagnostics = format!("{}{}", all_diagnostics, help);
                    }
                    all_diagnostics
                })
            ),
        }
    }};
}

#[cfg(test)]
fn check(actual: &str, expect: expect_test::Expect) {
    expect.assert_eq(actual);
}

#[test]
fn types() {
    check(
        &run_parser!(type_(expr()), "int"),
        expect_test::expect!["int"],
    );
    check(
        &run_parser!(type_(expr()), "real"),
        expect_test::expect!["real"],
    );
    check(
        &run_parser!(type_(expr()), "bool"),
        expect_test::expect!["bool"],
    );
    check(
        &run_parser!(type_(expr()), "string"),
        expect_test::expect!["string"],
    );
    check(
        &run_parser!(type_(expr()), "{int, real, string}"),
        expect_test::expect!["{int, real, string}"],
    );
    check(
        &run_parser!(type_(expr()), "{int, {real, int}, string}"),
        expect_test::expect!["{int, {real, int}, string}"],
    );
}

#[test]
fn immediates() {
    check(
        &run_parser!(immediate(), "0x88"),
        expect_test::expect!["136"],
    );
    check(
        &run_parser!(immediate(), "0b111"),
        expect_test::expect!["7"],
    );
    check(&run_parser!(immediate(), "1"), expect_test::expect!["1"]);
    check(
        &run_parser!(immediate(), "0x4f3f4f3f4f3f4f3f4f3f4f3f4f"),
        expect_test::expect!["6278618198356320102092284837711"],
    );
    check(
        &run_parser!(immediate(), "0b1000000000000000000000000000000000000000000000000000000000000000000000000000000000000"),
        expect_test::expect!["19342813113834066795298816"],
    );
    check(
        &run_parser!(immediate(), "9223372036854775808"),
        expect_test::expect!["9223372036854775808"],
    );
    check(
        &run_parser!(immediate(), "1.3"),
        expect_test::expect!["1.3e0"],
    );
}

#[test]
fn use_statements() {
    check(
        &run_parser!(yurt_program(), "use *; use ::*;"),
        expect_test::expect!["use *; use ::*;"],
    );

    check(
        &run_parser!(yurt_program(), "use {}; use ::{};"),
        expect_test::expect!["use {}; use ::{};"],
    );

    check(
        &run_parser!(yurt_program(), "use a; use ::a; use ::a as b;"),
        expect_test::expect!["use a; use ::a; use ::a as b;"],
    );

    check(
        &run_parser!(yurt_program(), "use a::b; use ::a::b; use ::a::b as c;"),
        expect_test::expect!["use a::b; use ::a::b; use ::a::b as c;"],
    );

    check(
        &run_parser!(yurt_program(), "use a::{b, c as d};"),
        expect_test::expect!["use a::{b, c as d};"],
    );

    check(
        &run_parser!(yurt_program(), "use ::a::{*, c as d};"),
        expect_test::expect!["use ::a::{*, c as d};"],
    );

    check(
        &run_parser!(yurt_program(), "use ::a::{*, c as d};"),
        expect_test::expect!["use ::a::{*, c as d};"],
    );

    check(
        &run_parser!(yurt_program(), "use a::{{*}, {c as d, { e as f, * }}};"),
        expect_test::expect!["use a::{{*}, {c as d, {e as f, *}}};"],
    );

    // Errors - TODO: imporve these
    check(
        &run_parser!(use_statement(), "use ;"),
        expect_test::expect![[r#"
            expected `*`, `::`, or `{`, found `;`
            @4..5: expected `*`, `::`, or `{`
        "#]],
    );

    check(
        &run_parser!(use_statement(), "use ::;"),
        expect_test::expect![[r#"
            expected `*`, or `{`, found `;`
            @6..7: expected `*`, or `{`
        "#]],
    );

    check(
        &run_parser!(use_statement(), "use a::;"),
        expect_test::expect![[r#"
            expected `;`, found `::`
            @5..7: expected `;`
        "#]],
    );

    check(
        &run_parser!(use_statement(), "use * as b;"),
        expect_test::expect![[r#"
            expected `;`, found `as`
            @6..8: expected `;`
        "#]],
    );

    check(
        &run_parser!(use_statement(), "use a::{* as d};"),
        expect_test::expect![[r#"
            expected `;`, found `::`
            @5..7: expected `;`
        "#]],
    );
}

#[test]
fn let_decls() {
    check(
        &run_parser!(let_decl(expr()), "let blah;"),
        expect_test::expect![[r#"
            type annotation or initializer needed for variable `blah`
            @0..9: type annotation or initializer needed
            consider giving `blah` an explicit type or an initializer"#]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah = 1.0;"),
        expect_test::expect!["let blah = 1e0"],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: real = 1.0;"),
        expect_test::expect!["let blah: real = 1e0"],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: real;"),
        expect_test::expect!["let blah: real"],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah = 1;"),
        expect_test::expect!["let blah = 1"],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: int = 1;"),
        expect_test::expect!["let blah: int = 1"],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: int;"),
        expect_test::expect!["let blah: int"],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah = true;"),
        expect_test::expect!["let blah = true"],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: bool = false;"),
        expect_test::expect!["let blah: bool = false"],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: bool;"),
        expect_test::expect!["let blah: bool"],
    );
    check(
        &run_parser!(let_decl(expr()), r#"let blah = "hello";"#),
        expect_test::expect![[r#"let blah = "hello""#]],
    );
    check(
        &run_parser!(let_decl(expr()), r#"let blah: string = "hello";"#),
        expect_test::expect![[r#"let blah: string = "hello""#]],
    );
    check(
        &run_parser!(let_decl(expr()), r#"let blah: string;"#),
        expect_test::expect!["let blah: string"],
    );
}

#[test]
fn constraint_decls() {
    // Argument just needs to be any expression, as far as the parser is concerned.
    check(
        &run_parser!(constraint_decl(expr()), "constraint blah;"),
        expect_test::expect!["constraint blah"],
    );
}

#[test]
fn solve_decls() {
    check(
        &run_parser!(solve_decl(), "solve satisfy;"),
        expect_test::expect!["solve satisfy"],
    );
    check(
        &run_parser!(solve_decl(), "solve minimize foo;"),
        expect_test::expect!["solve minimize foo"],
    );
    check(
        &run_parser!(solve_decl(), "solve maximize foo;"),
        expect_test::expect!["solve maximize foo"],
    );

    check(
        &run_parser!(solve_decl(), "solve maximize x + y;"),
        expect_test::expect!["solve maximize (x + y)"],
    );

    check(
        &run_parser!(solve_decl(), "solve world hunger;"),
        expect_test::expect![[r#"
            expected `maximize`, `minimize`, or `satisfy`, found `world`
            @6..11: expected `maximize`, `minimize`, or `satisfy`
        "#]],
    );
}

#[test]
fn basic_exprs() {
    check(&run_parser!(expr(), "123"), expect_test::expect!["123"]);
    check(&run_parser!(expr(), "foo"), expect_test::expect!["foo"]);
}

#[test]
fn unary_op_exprs() {
    check(&run_parser!(expr(), "!a"), expect_test::expect!["!a"]);
    check(&run_parser!(expr(), "+a"), expect_test::expect!["+a"]);
    check(&run_parser!(expr(), "-a"), expect_test::expect!["-a"]);
    check(&run_parser!(expr(), "+7"), expect_test::expect!["+7"]);
    check(&run_parser!(expr(), "+3.4"), expect_test::expect!["+3.4e0"]);
    check(
        &run_parser!(expr(), "+0x456"),
        expect_test::expect!["+1110"],
    );
    check(
        &run_parser!(expr(), "+0b01010101"),
        expect_test::expect!["+85"],
    );
    check(
        &run_parser!(
            expr(),
            "+0b1101000000001100101010101010101111111111101010101101010101010101"
        ),
        expect_test::expect!["+14991544915315053909"],
    );
    check(&run_parser!(expr(), "-1.0"), expect_test::expect!["-1e0"]);
    check(&run_parser!(expr(), "-1"), expect_test::expect!["-1"]);
    check(&run_parser!(expr(), "-0x133"), expect_test::expect!["-307"]);
    check(&run_parser!(expr(), "-0b1101"), expect_test::expect!["-13"]);
    check(
        &run_parser!(
            expr(),
            "-0b1101000000001100101010101010101010101010101010101101010101010101"
        ),
        expect_test::expect!["-14991544909594023253"],
    );
    check(
        &run_parser!(expr(), "! - - !  -+  -1"),
        expect_test::expect!["!--!-+-1"],
    );
    check(
        &run_parser!(expr(), "+ {- x} '  '  "),
        expect_test::expect!["+{ -x }''"],
    );
}

#[test]
fn binary_op_exprs() {
    check(
        &run_parser!(expr(), "a * 2.0"),
        expect_test::expect!["(a * 2e0)"],
    );
    check(
        &run_parser!(expr(), "a / 2.0"),
        expect_test::expect!["(a / 2e0)"],
    );
    check(
        &run_parser!(expr(), "a % 2.0"),
        expect_test::expect!["(a % 2e0)"],
    );
    check(
        &run_parser!(expr(), "a + 2.0"),
        expect_test::expect!["(a + 2e0)"],
    );
    check(
        &run_parser!(expr(), "a - 2.0"),
        expect_test::expect!["(a - 2e0)"],
    );
    check(
        &run_parser!(expr(), "a+2.0"),
        expect_test::expect!["(a + 2e0)"],
    );
    check(
        &run_parser!(expr(), "a-2.0"),
        expect_test::expect!["(a - 2e0)"],
    );
    check(
        &run_parser!(expr(), "a < 2.0"),
        expect_test::expect!["(a < 2e0)"],
    );
    check(
        &run_parser!(expr(), "a > 2.0"),
        expect_test::expect!["(a > 2e0)"],
    );
    check(
        &run_parser!(expr(), "a <= 2.0"),
        expect_test::expect!["(a <= 2e0)"],
    );
    check(
        &run_parser!(expr(), "a >= 2.0"),
        expect_test::expect!["(a >= 2e0)"],
    );
    check(
        &run_parser!(expr(), "a == 2.0"),
        expect_test::expect!["(a == 2e0)"],
    );
    check(
        &run_parser!(expr(), "a != 2.0"),
        expect_test::expect!["(a != 2e0)"],
    );
    check(
        &run_parser!(expr(), "a && b"),
        expect_test::expect!["(a && b)"],
    );

    check(
        &run_parser!(expr(), "a || b"),
        expect_test::expect!["(a || b)"],
    );

    check(
        &run_parser!(expr(), "a || b && c || d && !e"),
        expect_test::expect!["((a || (b && c)) || (d && !e))"],
    );
}

#[test]
fn complex_exprs() {
    check(
        &run_parser!(expr(), "2 * b * 3"),
        expect_test::expect!["((2 * b) * 3)"],
    );
    check(
        &run_parser!(expr(), "2 < b * 3"),
        expect_test::expect!["(2 < (b * 3))"],
    );
    check(
        &run_parser!(expr(), "2.0 > b * 3.0"),
        expect_test::expect!["(2e0 > (b * 3e0))"],
    );
    check(
        &run_parser!(expr(), "2.0 * b < 3.0"),
        expect_test::expect!["((2e0 * b) < 3e0)"],
    );
    check(
        &run_parser!(expr(), "2 > b < 3"),
        expect_test::expect!["((2 > b) < 3)"],
    );
    check(
        &run_parser!(expr(), "2 != b < 3"),
        expect_test::expect!["((2 != b) < 3)"],
    );
    check(
        &run_parser!(expr(), "2 < b != 3"),
        expect_test::expect!["((2 < b) != 3)"],
    );
    check(
        &run_parser!(expr(), "a > b * c < d"),
        expect_test::expect!["((a > (b * c)) < d)"],
    );
    check(
        &run_parser!(expr(), "2 + 3 * 4"),
        expect_test::expect!["(2 + (3 * 4))"],
    );
    check(
        &run_parser!(expr(), "10 - 8 / 4"),
        expect_test::expect!["(10 - (8 / 4))"],
    );
    check(
        &run_parser!(expr(), "10 + 8 % 4"),
        expect_test::expect!["(10 + (8 % 4))"],
    );
    check(
        &run_parser!(expr(), "2 + 3 * 4 < 5"),
        expect_test::expect!["((2 + (3 * 4)) < 5)"],
    );
    check(
        &run_parser!(expr(), "2 * 3 / 4 < 5"),
        expect_test::expect!["(((2 * 3) / 4) < 5)"],
    );
    check(
        &run_parser!(expr(), "10 - 5 + 3 > 7"),
        expect_test::expect!["(((10 - 5) + 3) > 7)"],
    );
    check(
        &run_parser!(expr(), "10 % 2 * 4 < 3"),
        expect_test::expect!["(((10 % 2) * 4) < 3)"],
    );
    check(
        &run_parser!(expr(), "2 + 3 * 4 - 5 / 2 > 1"),
        expect_test::expect!["(((2 + (3 * 4)) - (5 / 2)) > 1)"],
    );
}

#[test]
fn parens_exprs() {
    check(
        &run_parser!(expr(), "(1 + 2) * 3"),
        expect_test::expect!["((1 + 2) * 3)"],
    );
    check(
        &run_parser!(expr(), "1 * (2 + 3)"),
        expect_test::expect!["(1 * (2 + 3))"],
    );
    check(
        &run_parser!(expr(), "(1 + 2) * (3 + 4)"),
        expect_test::expect!["((1 + 2) * (3 + 4))"],
    );
    check(
        &run_parser!(expr(), "(1 + (2 * 3)) * 4"),
        expect_test::expect!["((1 + (2 * 3)) * 4)"],
    );
    check(
        &run_parser!(expr(), "(1 * (2 + 3)) * 4"),
        expect_test::expect!["((1 * (2 + 3)) * 4)"],
    );
    check(
        &run_parser!(expr(), "((1 + 2) * 3) * 4"),
        expect_test::expect!["(((1 + 2) * 3) * 4)"],
    );
    check(
        &run_parser!(expr(), "((1 + 2) * (3 + 4)) * 5"),
        expect_test::expect!["(((1 + 2) * (3 + 4)) * 5)"],
    );
    check(
        &run_parser!(expr(), "(1 + 2) * 3 / 4"),
        expect_test::expect!["(((1 + 2) * 3) / 4)"],
    );
    check(
        &run_parser!(expr(), "1 / (2 + 3) * 4"),
        expect_test::expect!["((1 / (2 + 3)) * 4)"],
    );
    check(
        &run_parser!(expr(), "(1 < 2) && (3 > 4)"),
        expect_test::expect!["((1 < 2) && (3 > 4))"],
    );
    check(
        &run_parser!(expr(), "(1 == 2) || (3 != 4)"),
        expect_test::expect!["((1 == 2) || (3 != 4))"],
    );
    check(
        &run_parser!(expr(), "1 < (2 && 3) > 4"),
        expect_test::expect!["((1 < (2 && 3)) > 4)"],
    );
    check(
        &run_parser!(expr(), "1 && (2 || 3)"),
        expect_test::expect!["(1 && (2 || 3))"],
    );
    check(
        &run_parser!(expr(), "1 == (2 || 3) != 4"),
        expect_test::expect!["((1 == (2 || 3)) != 4)"],
    );
    check(
        &run_parser!(expr(), "-(1 + 2)"),
        expect_test::expect!["-(1 + 2)"],
    );
    check(
        &run_parser!(expr(), "!(a < b)"),
        expect_test::expect!["!(a < b)"],
    );
    check(&run_parser!(expr(), "(1)"), expect_test::expect!["1"]);
    check(&run_parser!(expr(), "(a)"), expect_test::expect!["a"]);
    check(
        &run_parser!(expr(), "()"),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `::`, `[`, `cond`, `if`, `{`, or `{`, found `)`
            @1..2: expected `!`, `(`, `+`, `-`, `::`, `::`, `[`, `cond`, `if`, `{`, or `{`
        "#]],
    );
    check(
        &run_parser!(expr(), "(if a < b { 1 } else { 2 })"),
        expect_test::expect!["if (a < b) { 1 } else { 2 }"],
    );
    check(
        &run_parser!(expr(), "(foo(a, b, c))"),
        expect_test::expect!["foo(a, b, c)"],
    );
}

#[test]
fn enums() {
    check(
        &run_parser!(enum_decl(), "enum MyEnum = Variant1 | Variant2;"),
        expect_test::expect!["enum MyEnum = Variant1 | Variant2"],
    );
    check(
        &run_parser!(enum_decl(), "enum MyEnum = Variant1;"),
        expect_test::expect!["enum MyEnum = Variant1"],
    );
    check(
        &run_parser!(expr(), "MyEnum::Variant1"),
        expect_test::expect!["MyEnum::Variant1"],
    );
    check(
        &run_parser!(
            let_decl(expr()),
            r#"
            let x = MyEnum::Variant3;
            "#
        ),
        expect_test::expect!["let x = MyEnum::Variant3"],
    );
    check(
        &run_parser!(
            let_decl(expr()),
            r#"
            let e: ::path::to::MyEnum;
            "#
        ),
        expect_test::expect!["let e: ::path::to::MyEnum"],
    );
}

#[test]
fn custom_types() {
    check(
        &run_parser!(type_(expr()), "custom_type"),
        expect_test::expect!["custom_type"],
    );
    check(
        &run_parser!(type_decl(), "type MyInt = int;"),
        expect_test::expect!["type MyInt = int"],
    );
    check(
        &run_parser!(type_decl(), "type MyReal = real;"),
        expect_test::expect!["type MyReal = real"],
    );
    check(
        &run_parser!(type_decl(), "type MyBool = bool;"),
        expect_test::expect!["type MyBool = bool"],
    );
    check(
        &run_parser!(type_decl(), "type MyString = string;"),
        expect_test::expect!["type MyString = string"],
    );
    check(
        &run_parser!(type_decl(), "type IntArray = int[5];"),
        expect_test::expect!["type IntArray = int[5]"],
    );
    check(
        &run_parser!(type_decl(), "type MyTuple = { int, real, z: string };"),
        expect_test::expect!["type MyTuple = {int, real, z: string}"],
    );
    check(
        &run_parser!(type_decl(), "type MyAliasInt = MyInt;"),
        expect_test::expect!["type MyAliasInt = MyInt"],
    );
}

#[test]
fn idents() {
    check(
        &run_parser!(ident(), "foobar"),
        expect_test::expect!["foobar"],
    );
    check(
        &run_parser!(ident(), "foo_bar"),
        expect_test::expect!["foo_bar"],
    );
    check(
        &run_parser!(ident(), "FOO_bar"),
        expect_test::expect!["FOO_bar"],
    );
    check(
        &run_parser!(ident(), "__FOO"),
        expect_test::expect!["__FOO"],
    );
    check(
        &run_parser!(ident(), "_2_FOO1"),
        expect_test::expect!["_2_FOO1"],
    );
    check(&run_parser!(ident(), "_"), expect_test::expect!["_"]);
    check(
        &run_parser!(ident(), "12_ab"),
        expect_test::expect![[r#"
            expected something else, found `12`
            @0..2: expected something else
        "#]],
    );
    check(
        // Lexer will split this into 3 tokens, ident() will parse the first one.
        // This shows that we're not able to parser `ab*cd` as a single identifier
        &run_parser!(ident(), "ab*cd"),
        expect_test::expect![[r#"
            expected "end of input", found `*`
            @2..3: expected "end of input"
        "#]],
    );
}

#[test]
fn paths() {
    check(
        &run_parser!(path(), "foo::bar"),
        expect_test::expect!["foo::bar"],
    );
    check(
        &run_parser!(path(), "_foo_::_bar"),
        expect_test::expect!["_foo_::_bar"],
    );
    check(&run_parser!(path(), "_::_"), expect_test::expect!["_::_"]);
    check(
        &run_parser!(path(), "t2::_3t::t4_::t"),
        expect_test::expect!["t2::_3t::t4_::t"],
    );
    check(
        &run_parser!(path(), "::foo::bar"),
        expect_test::expect!["::foo::bar"],
    );

    // As long as these two produce an error... it should be expecting 'ident'.
    check(
        &run_parser!(path().then_ignore(end()), "foo::"),
        expect_test::expect![[r#"
            expected something else, found "end of input"
            @5..5: expected something else
        "#]],
    );
    check(
        &run_parser!(path().then_ignore(end()), "::foo::"),
        expect_test::expect![[r#"
            expected something else, found "end of input"
            @7..7: expected something else
        "#]],
    );
}

#[test]
fn fn_decl_test() {
    let src = r#"
fn foo(x: real, y: real) -> real {
    let z = 5.0;
    z
}
"#;

    check(
        &run_parser!(yurt_program(), src),
        expect_test::expect!["fn foo(x: real, y: real) -> real { let z = 5e0; z };"],
    );
}

#[test]
fn fn_call() {
    let src = r#"
let x = foo(a*3, c);
"#;

    check(
        &run_parser!(yurt_program(), src),
        expect_test::expect!["let x = foo((a * 3), c);"],
    );

    check(
        &run_parser!(expr(), "A::B::foo(-a, b+c)"),
        expect_test::expect!["A::B::foo(-a, (b + c))"],
    );
}

#[test]
fn code_blocks() {
    check(
        &run_parser!(let_decl(expr()), "let x = { 0 };"),
        expect_test::expect!["let x = { 0 }"],
    );

    check(
        &run_parser!(let_decl(expr()), "let x = { constraint x > 0.0; 0.0 };"),
        expect_test::expect!["let x = { constraint (x > 0e0); 0e0 }"],
    );

    check(
        &run_parser!(
            constraint_decl(expr()),
            "constraint { constraint { true }; x > 0 };"
        ),
        expect_test::expect!["constraint { constraint { true }; (x > 0) }"],
    );

    check(
        &run_parser!(let_decl(expr()), "let x = { 1.0 } * { 2.0 };"),
        expect_test::expect!["let x = ({ 1e0 } * { 2e0 })"],
    );

    check(
        &run_parser!(let_decl(expr()), "let x = {};"),
        expect_test::expect![[r#"
            empty tuple expressions are not allowed
            @8..10: empty tuple expression found
        "#]],
    );
}

#[test]
fn if_exprs() {
    check(
        &run_parser!(if_expr(expr()), "if c { 1 }"),
        expect_test::expect![[r#"
            expected `else`, found "end of input"
            @10..10: expected `else`
        "#]],
    );

    check(
        &run_parser!(if_expr(expr()), "if c { 1 } else { 0 }"),
        expect_test::expect!["if c { 1 } else { 0 }"],
    );

    check(
        &run_parser!(if_expr(expr()), "if c { if c { 1 } else { 0 } } else { 2 }"),
        expect_test::expect!["if c { if c { 1 } else { 0 } } else { 2 }"],
    );
}

#[test]
fn array_type() {
    check(
        &run_parser!(type_(expr()), r#"int[5]"#),
        expect_test::expect!["int[5]"],
    );

    check(
        &run_parser!(type_(expr()), r#"int[MyEnum]"#),
        expect_test::expect!["int[MyEnum]"],
    );

    check(
        &run_parser!(type_(expr()), r#"int[N]"#),
        expect_test::expect!["int[N]"],
    );

    check(
        &run_parser!(
            type_(expr()),
            r#"string[foo()][{ 7 }][if true { 1 } else { 2 }]"#
        ),
        expect_test::expect!["string[if true { 1 } else { 2 }][{ 7 }][foo()]"],
    );

    check(
        &run_parser!(type_(expr()), r#"real[N][9][M][3]"#),
        expect_test::expect!["real[3][M][9][N]"],
    );

    check(
        &run_parser!(type_(expr()), r#"{int, { real, string }}[N][9]"#),
        expect_test::expect!["{int, {real, string}}[9][N]"],
    );

    check(
        &run_parser!(let_decl(expr()), r#"let a: int[];"#),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `::`, `[`, `cond`, `if`, `{`, or `{`, found `]`
            @11..12: expected `!`, `(`, `+`, `-`, `::`, `::`, `[`, `cond`, `if`, `{`, or `{`
        "#]],
    );
}
#[test]
fn array_expressions() {
    check(&run_parser!(expr(), r#"[5]"#), expect_test::expect!["[5]"]);

    check(&run_parser!(expr(), r#"[5,]"#), expect_test::expect!["[5]"]);

    check(
        &run_parser!(expr(), r#"[5, 4]"#),
        expect_test::expect!["[5, 4]"],
    );

    check(
        &run_parser!(expr(), r#"[[ 1 ],]"#),
        expect_test::expect!["[[1]]"],
    );

    check(
        &run_parser!(expr(), r#"[[1, 2], 3]"#), // This should fail in semantic analysis
        expect_test::expect!["[[1, 2], 3]"],
    );

    check(
        &run_parser!(expr(), r#"[[1, 2], [3, 4]]"#),
        expect_test::expect!["[[1, 2], [3, 4]]"],
    );

    check(
        &run_parser!(
            expr(),
            r#"[[foo(), { 2 }], [if true { 1 } else { 2 }, t.0]]"#
        ),
        expect_test::expect!["[[foo(), { 2 }], [if true { 1 } else { 2 }, t.0]]"],
    );
}

#[test]
fn array_field_accesss() {
    check(
        &run_parser!(expr(), r#"a[5]"#),
        expect_test::expect!["a[5]"],
    );

    check(
        &run_parser!(expr(), r#"{ a }[N][foo()][M][4]"#),
        expect_test::expect!["{ a }[N][foo()][M][4]"],
    );

    check(
        &run_parser!(expr(), r#"foo()[{ M }][if true { 1 } else { 3 }]"#),
        expect_test::expect!["foo()[{ M }][if true { 1 } else { 3 }]"],
    );

    check(
        &run_parser!(let_decl(expr()), r#"let x = a[];"#),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `::`, `[`, `cond`, `if`, `{`, or `{`, found `]`
            @10..11: expected `!`, `(`, `+`, `-`, `::`, `::`, `[`, `cond`, `if`, `{`, or `{`
        "#]],
    );

    check(
        &run_parser!(expr(), r#"a[MyEnum::Variant1]"#),
        expect_test::expect!["a[MyEnum::Variant1]"],
    );
}

#[test]
fn tuple_expressions() {
    check(
        &run_parser!(expr(), r#"{0}"#), // This is not a tuple. It is a code block expr.
        expect_test::expect!["{ 0 }"],
    );

    check(
        &run_parser!(expr(), r#"{x: 0}"#), // This is a tuple because the field is named so there is no ambiguity
        expect_test::expect!["{x: 0}"],
    );

    check(
        &run_parser!(expr(), r#"{0,}"#), // This is a tuple
        expect_test::expect!["{0}"],
    );

    check(
        &run_parser!(expr(), r#"{x: 0,}"#), // This is a tuple
        expect_test::expect!["{x: 0}"],
    );

    check(
        &run_parser!(expr(), r#"{0, 1.0, "foo"}"#),
        expect_test::expect![[r#"{0, 1e0, "foo"}"#]],
    );

    check(
        &run_parser!(expr(), r#"{x: 0, y: 1.0, z: "foo"}"#),
        expect_test::expect![[r#"{x: 0, y: 1e0, z: "foo"}"#]],
    );

    check(
        &run_parser!(expr(), r#"{0, {1.0, "bar"}, "foo"}"#),
        expect_test::expect![[r#"{0, {1e0, "bar"}, "foo"}"#]],
    );

    check(
        &run_parser!(expr(), r#"{x: 0, {y: 1.0, "bar"}, z: "foo"}"#),
        expect_test::expect![[r#"{x: 0, {y: 1e0, "bar"}, z: "foo"}"#]],
    );

    check(
        &run_parser!(expr(), r#"{ { 42 }, if c { 2 } else { 3 }, foo() }"#),
        expect_test::expect!["{{ 42 }, if c { 2 } else { 3 }, foo()}"],
    );

    check(
        &run_parser!(
            expr(),
            r#"{ x: { 42 }, y: if c { 2 } else { 3 }, z: foo() }"#
        ),
        expect_test::expect!["{x: { 42 }, y: if c { 2 } else { 3 }, z: foo()}"],
    );
}

#[test]
fn tuple_field_accesses() {
    check(
        &run_parser!(expr(), r#"t.0 + t.9999999 + t.x"#),
        expect_test::expect!["((t.0 + t.9999999) + t.x)"],
    );

    check(
        &run_parser!(expr(), r#"{0, 1}.0"#),
        expect_test::expect!["{0, 1}.0"],
    );

    check(
        &run_parser!(expr(), r#"{0, 1}.x"#),
        expect_test::expect!["{0, 1}.x"],
    );

    check(
        &run_parser!(expr(), r#"t.0 .0"#),
        expect_test::expect!["t.0.0"],
    );

    check(
        &run_parser!(expr(), r#"t.x .y"#),
        expect_test::expect!["t.x.y"],
    );

    check(
        &run_parser!(expr(), "t \r .1 .2.2. \n 3 . \t 13 . 1.1"),
        expect_test::expect!["t.1.2.2.3.13.1.1"],
    );

    check(
        &run_parser!(expr(), "t \r .x .1.2. \n w . \t t. 3.4"),
        expect_test::expect!["t.x.1.2.w.t.3.4"],
    );

    check(
        &run_parser!(expr(), r#"foo().0.1"#),
        expect_test::expect!["foo().0.1"],
    );

    check(
        &run_parser!(expr(), r#"foo().a.b.0.1"#),
        expect_test::expect!["foo().a.b.0.1"],
    );

    check(
        &run_parser!(expr(), r#"{ {0, 0} }.0"#),
        expect_test::expect!["{ {0, 0} }.0"],
    );

    check(
        &run_parser!(expr(), r#"{ {0, 0} }.a"#),
        expect_test::expect!["{ {0, 0} }.a"],
    );

    check(
        &run_parser!(expr(), r#"if true { {0, 0} } else { {0, 0} }.0"#),
        expect_test::expect!["if true { {0, 0} } else { {0, 0} }.0"],
    );

    check(
        &run_parser!(expr(), r#"if true { {0, 0} } else { {0, 0} }.x"#),
        expect_test::expect!["if true { {0, 0} } else { {0, 0} }.x"],
    );

    // This parses because `1 + 2` is an expression, but it should fail in semantic analysis.
    check(
        &run_parser!(expr(), "1 + 2 .3"),
        expect_test::expect!["(1 + 2.3)"],
    );

    // This parses because `1 + 2` is an expression, but it should fail in semantic analysis.
    check(
        &run_parser!(expr(), "1 + 2 .a"),
        expect_test::expect!["(1 + 2.a)"],
    );

    check(
        &run_parser!(let_decl(expr()), "let x = t.0xa;"),
        expect_test::expect![[r#"
            invalid integer `0xa` as tuple index
            @10..13: invalid integer as tuple index
        "#]],
    );

    check(
        &run_parser!(let_decl(expr()), "let x = t.111111111111111111111111111;"),
        expect_test::expect![[r#"
            invalid integer `111111111111111111111111111` as tuple index
            @10..37: invalid integer as tuple index
        "#]],
    );

    check(
        &run_parser!(let_decl(expr()), "let x = t.111111111111111111111111111.2;"),
        expect_test::expect![[r#"
            invalid integer `111111111111111111111111111` as tuple index
            @10..37: invalid integer as tuple index
        "#]],
    );

    check(
        &run_parser!(let_decl(expr()), "let x = t.2.111111111111111111111111111;"),
        expect_test::expect![[r#"
            invalid integer `111111111111111111111111111` as tuple index
            @12..39: invalid integer as tuple index
        "#]],
    );

    check(
        &run_parser!(
            let_decl(expr()),
            "let x = t.222222222222222222222.111111111111111111111111111;"
        ),
        expect_test::expect![[r#"
            invalid integer `222222222222222222222` as tuple index
            @10..31: invalid integer as tuple index
        "#]],
    );

    check(
        &run_parser!(let_decl(expr()), "let x = t.1e5;"),
        expect_test::expect![[r#"
            invalid value `1e5` as tuple index
            @10..13: invalid value as tuple index
        "#]],
    );

    check(
        &run_parser!(let_decl(expr()), "let bad_tuple:{} = {};"),
        expect_test::expect![[r#"
            empty tuple types are not allowed
            @14..16: empty tuple type found
            empty tuple expressions are not allowed
            @19..21: empty tuple expression found
        "#]],
    );
}

#[test]
fn cond_exprs() {
    check(
        &run_parser!(cond_expr(expr()), r#"cond { else => a }"#),
        expect_test::expect!["cond { else => a }"],
    );

    check(
        &run_parser!(cond_expr(expr()), r#"cond { else => { a } }"#),
        expect_test::expect!["cond { else => { a } }"],
    );

    check(
        &run_parser!(cond_expr(expr()), r#"cond { a => b, else => c }"#),
        expect_test::expect!["cond { a => b, else => c }"],
    );

    check(
        &run_parser!(cond_expr(expr()), r#"cond { a => { b }, else => c, }"#),
        expect_test::expect!["cond { a => { b }, else => c }"],
    );

    check(
        &run_parser!(
            cond_expr(expr()),
            r#"cond { a => b, { true } => d, else => f, }"#
        ),
        expect_test::expect!["cond { a => b, { true } => d, else => f }"],
    );

    check(
        &run_parser!(cond_expr(expr()), r#"cond { a => b, }"#),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `::`, `[`, `cond`, `else`, `if`, `{`, or `{`, found `}`
            @15..16: expected `!`, `(`, `+`, `-`, `::`, `::`, `[`, `cond`, `else`, `if`, `{`, or `{`
        "#]],
    );

    check(
        &run_parser!(cond_expr(expr()), r#"cond { else => a, b => c }"#),
        expect_test::expect![[r#"
            expected `}`, found `b`
            @18..19: expected `}`
        "#]],
    );
}

#[test]
fn casting() {
    check(
        &run_parser!(expr(), r#"(5 as int) as real as int"#),
        expect_test::expect!["5 as int as real as int"],
    );

    check(
        &run_parser!(expr(), r#"t.0.1 as real * a[5][3] as int"#),
        expect_test::expect!["(t.0.1 as real * a[5][3] as int)"],
    );

    check(
        &run_parser!(
            let_decl(expr()),
            r#"let x = foo() as real as { int, real };"#
        ),
        expect_test::expect!["let x = foo() as real as {int, real}"],
    );

    check(
        &run_parser!(let_decl(expr()), r#"let x = 5 as;"#),
        expect_test::expect![[r#"
            expected `::`, `bool`, `int`, `real`, `string`, or `{`, found `;`
            @12..13: expected `::`, `bool`, `int`, `real`, `string`, or `{`
        "#]],
    );
}

#[test]
fn in_expr() {
    check(
        &run_parser!(expr(), r#"x in { 1, 2 }"#),
        expect_test::expect!["x in {1, 2}"],
    );

    check(
        &run_parser!(expr(), r#"x in [ 1, 2 ] in { true, false }"#),
        expect_test::expect!["x in [1, 2] in {true, false}"],
    );

    check(
        &run_parser!(expr(), r#"x as int in { 1, 2 }"#),
        expect_test::expect!["x as int in {1, 2}"],
    );

    check(
        &run_parser!(expr(), r#"[1] in foo() in [[1]]"#),
        expect_test::expect!["[1] in foo() in [[1]]"],
    );

    check(
        &run_parser!(let_decl(expr()), r#"let x = 5 in;"#),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `::`, `[`, `cond`, `if`, `{`, or `{`, found `;`
            @12..13: expected `!`, `(`, `+`, `-`, `::`, `::`, `[`, `cond`, `if`, `{`, or `{`
        "#]],
    );
}

#[test]
fn basic_program() {
    let src = r#"
let low_val: real = 1.23;
let high_val = 4.56;        // Implicit type.

// Here's the constraints.
constraint mid > low_val * 2.0;
constraint mid < high_val;

solve minimize mid;
"#;

    check(
        &run_parser!(yurt_program(), src),
        expect_test::expect!["let low_val: real = 1.23e0; let high_val = 4.56e0; constraint (mid > (low_val * 2e0)); constraint (mid < high_val); solve minimize mid;"],
    );
}

#[test]
fn fn_errors() {
    check(
        &run_parser!(yurt_program(), "fn foo() {5}"),
        expect_test::expect![[r#"
            expected `->`, found `{`
            @9..10: expected `->`
        "#]],
    );

    check(
        &run_parser!(yurt_program(), "fn foo() -> real {}"),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `::`, `[`, `cond`, `constraint`, `if`, `let`, `state`, `{`, or `{`, found `}`
            @18..19: expected `!`, `(`, `+`, `-`, `::`, `::`, `[`, `cond`, `constraint`, `if`, `let`, `state`, `{`, or `{`
        "#]],
    );
}

#[test]
fn out_of_order_decls() {
    let src = r#"
solve maximize low;
constraint low < high;
let high = 2.0;
solve satisfy;
let low = 1.0;
"#;

    check(
        &run_parser!(yurt_program(), src),
        expect_test::expect![
            "solve maximize low; constraint (low < high); let high = 2e0; solve satisfy; let low = 1e0;"
        ],
    );
}

#[test]
fn keywords_as_identifiers_errors() {
    for keyword in KEYWORDS {
        let src = format!("let {keyword} = 5;");
        assert_eq!(
            &run_parser!(yurt_program(), &src),
            &format!("expected identifier, found keyword `{keyword}`\n@4..{}: expected identifier, found keyword\n", 4 + format!("{keyword}").len()), // End of the error span),
            "Check \"identifier as keyword\" error for  keyword \"{}\"",
            keyword
        );
    }
}

#[test]
fn test_parse_str_to_ast() {
    let filepath: Rc<Path> = Rc::from(Path::new("test"));
    check(
        &parse_str_to_ast("let x = 5;", filepath.clone())
            .expect("Can't parse string.")
            .to_string(),
        expect_test::expect![["let x = 5;"]],
    );
    check(
        &format!("{:?}", parse_str_to_ast("let x = 5", filepath.clone())),
        expect_test::expect![[
            r#"Err([Parse { error: ExpectedFound { span: "test":9..9, expected: [Some(";"), Some("||"), Some("&&"), Some("!="), Some("=="), Some(">="), Some("<="), Some(">"), Some("<"), Some("+"), Some("-"), Some("*"), Some("/"), Some("%"), Some("in"), Some("as"), Some("."), Some("["), Some("'")], found: None } }])"#
        ]],
    );
    check(
        &format!("{:?}", parse_str_to_ast("@ @", filepath.clone())),
        expect_test::expect![[
            r#"Err([Lex { span: "test":0..1, error: InvalidToken }, Lex { span: "test":2..3, error: InvalidToken }])"#
        ]],
    );
}

#[test]
fn big_ints() {
    check(
        &run_parser!(
            let_decl(expr()),
            "let blah = 1234567890123456789012345678901234567890;"
        ),
        expect_test::expect!["let blah = 1234567890123456789012345678901234567890"],
    );
    check(
        &run_parser!(
            let_decl(expr()),
            "let blah = 0xfeedbadf00d2adeadcafed00dbabeface;"
        ),
        // Confirmed by using the Python REPL to convert from hex to dec...
        expect_test::expect!["let blah = 5421732407698601623698172315373246806734"],
    );
    check(
        &run_parser!(
            expr(),
            "0b110100101001010101010101010101010011010011010101010101010101010101010101010 + \
            0b01001001010110101010101001010101010010100100101001010010100100100001010"
        ),
        // Again confirmed using the Python REPL.  Handy.
        expect_test::expect!["(31076614848392666458794 + 676572722683907229962)"],
    );
}

#[test]
fn interface_test() {
    let src = r#"
interface Foo {
    fn foo(x: real, y: int[5]) -> real;
    fn bar(x: bool,) -> real;
    fn baz() -> { int, real };
}
"#;

    check(
        &run_parser!(interface_decl(), src),
        expect_test::expect!["interface Foo { fn foo(x: real, y: int[5]) -> real; fn bar(x: bool) -> real; fn baz() -> {int, real}; }"],
    );

    check(
        &run_parser!(interface_decl(), "interface Foo {}"),
        expect_test::expect!["interface Foo { }"],
    );
}

#[test]
fn contract_test() {
    check(
        &run_parser!(contract_decl(), "contract Foo(0) {}"),
        expect_test::expect!["contract Foo(0) { }"],
    );

    check(
        &run_parser!(contract_decl(), "contract Foo(if true {0} else {1}) {}"),
        expect_test::expect!["contract Foo(if true { 0 } else { 1 }) { }"],
    );

    check(
        &run_parser!(
            contract_decl(),
            "contract Foo(0) implements X::Bar, ::Y::Baz {}"
        ),
        expect_test::expect!["contract Foo(0) implements X::Bar, ::Y::Baz { }"],
    );

    check(
        &run_parser!(
            contract_decl(),
            "contract Foo(0) implements Bar { fn baz(x: real) -> int; }"
        ),
        expect_test::expect!["contract Foo(0) implements Bar { fn baz(x: real) -> int; }"],
    );

    check(
        &run_parser!(contract_decl(), "contract Foo { }"),
        expect_test::expect![[r#"
            expected `(`, found `{`
            @13..14: expected `(`
        "#]],
    );

    check(
        &run_parser!(contract_decl(), "contract Foo(0) implements { }"),
        expect_test::expect![[r#"
            expected `::`, found `{`
            @27..28: expected `::`
        "#]],
    );
}

#[test]
fn extern_test() {
    check(
        &run_parser!(extern_decl(), "extern {}"),
        expect_test::expect!["extern { }"],
    );
    check(
        &run_parser!(extern_decl(), "extern { fn foo() -> string; }"),
        expect_test::expect!["extern { fn foo() -> string; }"],
    );
    check(
        &run_parser!(extern_decl(), "extern { fn foo(x: int, y: real) -> int; }"),
        expect_test::expect!["extern { fn foo(x: int, y: real) -> int; }"],
    );
    check(
        &run_parser!(
            extern_decl(),
            "extern { fn foo() -> int; fn bar() -> real; }"
        ),
        expect_test::expect!["extern { fn foo() -> int; fn bar() -> real; }"],
    );
    check(
        &run_parser!(extern_decl(), "extern { fn foo(); }"),
        expect_test::expect![[r#"
            expected `->`, found `;`
            @17..18: expected `->`
        "#]],
    );
}
