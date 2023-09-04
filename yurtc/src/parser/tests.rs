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
            Ok(ast) => format!("{:?}", ast),
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
        expect_test::expect![[r#"Primitive { kind: Int, span: "test":0..3 }"#]],
    );
    check(
        &run_parser!(type_(expr()), "real"),
        expect_test::expect![[r#"Primitive { kind: Real, span: "test":0..4 }"#]],
    );
    check(
        &run_parser!(type_(expr()), "bool"),
        expect_test::expect![[r#"Primitive { kind: Bool, span: "test":0..4 }"#]],
    );
    check(
        &run_parser!(type_(expr()), "string"),
        expect_test::expect![[r#"Primitive { kind: String, span: "test":0..6 }"#]],
    );
    check(
        &run_parser!(type_(expr()), "{int, real, string}"),
        expect_test::expect![[
            r#"Tuple { fields: [(None, Primitive { kind: Int, span: "test":1..4 }), (None, Primitive { kind: Real, span: "test":6..10 }), (None, Primitive { kind: String, span: "test":12..18 })], span: "test":0..19 }"#
        ]],
    );
    check(
        &run_parser!(type_(expr()), "{int, {real, int}, string}"),
        expect_test::expect![[
            r#"Tuple { fields: [(None, Primitive { kind: Int, span: "test":1..4 }), (None, Tuple { fields: [(None, Primitive { kind: Real, span: "test":7..11 }), (None, Primitive { kind: Int, span: "test":13..16 })], span: "test":6..17 }), (None, Primitive { kind: String, span: "test":19..25 })], span: "test":0..26 }"#
        ]],
    );
}

#[test]
fn immediates() {
    check(
        &run_parser!(immediate(), "0x88"),
        expect_test::expect![[r#"Int(136)"#]],
    );
    check(
        &run_parser!(immediate(), "0b111"),
        expect_test::expect![[r#"Int(7)"#]],
    );
    check(
        &run_parser!(immediate(), "1"),
        expect_test::expect![[r#"Int(1)"#]],
    );
    check(
        &run_parser!(immediate(), "0x4f3f4f3f4f3f4f3f4f3f4f3f4f"),
        expect_test::expect![[r#"BigInt(6278618198356320102092284837711)"#]],
    );
    check(
        &run_parser!(immediate(), "0b1000000000000000000000000000000000000000000000000000000000000000000000000000000000000"),
        expect_test::expect![[r#"BigInt(19342813113834066795298816)"#]],
    );
    check(
        &run_parser!(immediate(), "9223372036854775808"),
        expect_test::expect![[r#"BigInt(9223372036854775808)"#]],
    );
    check(
        &run_parser!(immediate(), "1.3"),
        expect_test::expect![[r#"Real(1.3)"#]],
    );
}

#[test]
fn use_statements() {
    check(
        &run_parser!(yurt_program(), "use *; use ::*;"),
        expect_test::expect![[
            r#"[Use { is_absolute: false, use_tree: Glob("test":4..5), span: "test":0..6 }, Use { is_absolute: true, use_tree: Glob("test":13..14), span: "test":7..15 }]"#
        ]],
    );

    check(
        &run_parser!(yurt_program(), "use {}; use ::{};"),
        expect_test::expect![[
            r#"[Use { is_absolute: false, use_tree: Group { imports: [], span: "test":4..6 }, span: "test":0..7 }, Use { is_absolute: true, use_tree: Group { imports: [], span: "test":14..16 }, span: "test":8..17 }]"#
        ]],
    );

    check(
        &run_parser!(yurt_program(), "use a; use ::a; use ::a as b;"),
        expect_test::expect![[
            r#"[Use { is_absolute: false, use_tree: Name { name: Ident { name: "a", span: "test":4..5 }, span: "test":4..5 }, span: "test":0..6 }, Use { is_absolute: true, use_tree: Name { name: Ident { name: "a", span: "test":13..14 }, span: "test":13..14 }, span: "test":7..15 }, Use { is_absolute: true, use_tree: Alias { name: Ident { name: "a", span: "test":22..23 }, alias: Ident { name: "b", span: "test":27..28 }, span: "test":22..28 }, span: "test":16..29 }]"#
        ]],
    );

    check(
        &run_parser!(yurt_program(), "use a::b; use ::a::b; use ::a::b as c;"),
        expect_test::expect![[
            r#"[Use { is_absolute: false, use_tree: Path { prefix: Ident { name: "a", span: "test":4..5 }, suffix: Name { name: Ident { name: "b", span: "test":7..8 }, span: "test":7..8 }, span: "test":4..8 }, span: "test":0..9 }, Use { is_absolute: true, use_tree: Path { prefix: Ident { name: "a", span: "test":16..17 }, suffix: Name { name: Ident { name: "b", span: "test":19..20 }, span: "test":19..20 }, span: "test":16..20 }, span: "test":10..21 }, Use { is_absolute: true, use_tree: Path { prefix: Ident { name: "a", span: "test":28..29 }, suffix: Alias { name: Ident { name: "b", span: "test":31..32 }, alias: Ident { name: "c", span: "test":36..37 }, span: "test":31..37 }, span: "test":28..37 }, span: "test":22..38 }]"#
        ]],
    );

    check(
        &run_parser!(yurt_program(), "use a::{b, c as d};"),
        expect_test::expect![[
            r#"[Use { is_absolute: false, use_tree: Path { prefix: Ident { name: "a", span: "test":4..5 }, suffix: Group { imports: [Name { name: Ident { name: "b", span: "test":8..9 }, span: "test":8..9 }, Alias { name: Ident { name: "c", span: "test":11..12 }, alias: Ident { name: "d", span: "test":16..17 }, span: "test":11..17 }], span: "test":7..18 }, span: "test":4..18 }, span: "test":0..19 }]"#
        ]],
    );

    check(
        &run_parser!(yurt_program(), "use ::a::{*, c as d};"),
        expect_test::expect![[
            r#"[Use { is_absolute: true, use_tree: Path { prefix: Ident { name: "a", span: "test":6..7 }, suffix: Group { imports: [Glob("test":10..11), Alias { name: Ident { name: "c", span: "test":13..14 }, alias: Ident { name: "d", span: "test":18..19 }, span: "test":13..19 }], span: "test":9..20 }, span: "test":6..20 }, span: "test":0..21 }]"#
        ]],
    );

    check(
        &run_parser!(yurt_program(), "use ::a::{*, c as d};"),
        expect_test::expect![[
            r#"[Use { is_absolute: true, use_tree: Path { prefix: Ident { name: "a", span: "test":6..7 }, suffix: Group { imports: [Glob("test":10..11), Alias { name: Ident { name: "c", span: "test":13..14 }, alias: Ident { name: "d", span: "test":18..19 }, span: "test":13..19 }], span: "test":9..20 }, span: "test":6..20 }, span: "test":0..21 }]"#
        ]],
    );

    check(
        &run_parser!(yurt_program(), "use a::{{*}, {c as d, { e as f, * }}};"),
        expect_test::expect![[
            r#"[Use { is_absolute: false, use_tree: Path { prefix: Ident { name: "a", span: "test":4..5 }, suffix: Group { imports: [Group { imports: [Glob("test":9..10)], span: "test":8..11 }, Group { imports: [Alias { name: Ident { name: "c", span: "test":14..15 }, alias: Ident { name: "d", span: "test":19..20 }, span: "test":14..20 }, Group { imports: [Alias { name: Ident { name: "e", span: "test":24..25 }, alias: Ident { name: "f", span: "test":29..30 }, span: "test":24..30 }, Glob("test":32..33)], span: "test":22..35 }], span: "test":13..36 }], span: "test":7..37 }, span: "test":4..37 }, span: "test":0..38 }]"#
        ]],
    );

    // Errors - TODO: imporve these
    check(
        &run_parser!(use_statement(), "use ;"),
        expect_test::expect![[r#"
            expected `::`, `*`, or `{`, found `;`
            @4..5: expected `::`, `*`, or `{`
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
        expect_test::expect![[
            r#"Let { name: Ident { name: "blah", span: "test":4..8 }, ty: None, init: Some(Immediate { value: Real(1.0), span: "test":11..14 }), span: "test":0..15 }"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: real = 1.0;"),
        expect_test::expect![[
            r#"Let { name: Ident { name: "blah", span: "test":4..8 }, ty: Some(Primitive { kind: Real, span: "test":10..14 }), init: Some(Immediate { value: Real(1.0), span: "test":17..20 }), span: "test":0..21 }"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: real;"),
        expect_test::expect![[
            r#"Let { name: Ident { name: "blah", span: "test":4..8 }, ty: Some(Primitive { kind: Real, span: "test":10..14 }), init: None, span: "test":0..15 }"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah = 1;"),
        expect_test::expect![[
            r#"Let { name: Ident { name: "blah", span: "test":4..8 }, ty: None, init: Some(Immediate { value: Int(1), span: "test":11..12 }), span: "test":0..13 }"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: int = 1;"),
        expect_test::expect![[
            r#"Let { name: Ident { name: "blah", span: "test":4..8 }, ty: Some(Primitive { kind: Int, span: "test":10..13 }), init: Some(Immediate { value: Int(1), span: "test":16..17 }), span: "test":0..18 }"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: int;"),
        expect_test::expect![[
            r#"Let { name: Ident { name: "blah", span: "test":4..8 }, ty: Some(Primitive { kind: Int, span: "test":10..13 }), init: None, span: "test":0..14 }"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah = true;"),
        expect_test::expect![[
            r#"Let { name: Ident { name: "blah", span: "test":4..8 }, ty: None, init: Some(Immediate { value: Bool(true), span: "test":11..15 }), span: "test":0..16 }"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: bool = false;"),
        expect_test::expect![[
            r#"Let { name: Ident { name: "blah", span: "test":4..8 }, ty: Some(Primitive { kind: Bool, span: "test":10..14 }), init: Some(Immediate { value: Bool(false), span: "test":17..22 }), span: "test":0..23 }"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: bool;"),
        expect_test::expect![[
            r#"Let { name: Ident { name: "blah", span: "test":4..8 }, ty: Some(Primitive { kind: Bool, span: "test":10..14 }), init: None, span: "test":0..15 }"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), r#"let blah = "hello";"#),
        expect_test::expect![[
            r#"Let { name: Ident { name: "blah", span: "test":4..8 }, ty: None, init: Some(Immediate { value: String("hello"), span: "test":11..18 }), span: "test":0..19 }"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), r#"let blah: string = "hello";"#),
        expect_test::expect![[
            r#"Let { name: Ident { name: "blah", span: "test":4..8 }, ty: Some(Primitive { kind: String, span: "test":10..16 }), init: Some(Immediate { value: String("hello"), span: "test":19..26 }), span: "test":0..27 }"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), r#"let blah: string;"#),
        expect_test::expect![[
            r#"Let { name: Ident { name: "blah", span: "test":4..8 }, ty: Some(Primitive { kind: String, span: "test":10..16 }), init: None, span: "test":0..17 }"#
        ]],
    );
}

#[test]
fn constraint_decls() {
    // Argument just needs to be any expression, as far as the parser is concerned.
    check(
        &run_parser!(constraint_decl(expr()), "constraint blah;"),
        expect_test::expect![[
            r#"Constraint { expr: Path(Path { path: [Ident { name: "blah", span: "test":11..15 }], is_absolute: false, span: "test":11..15 }), span: "test":0..16 }"#
        ]],
    );
}

#[test]
fn solve_decls() {
    check(
        &run_parser!(solve_decl(), "solve satisfy;"),
        expect_test::expect![[r#"Solve { directive: Satisfy, span: "test":0..14 }"#]],
    );
    check(
        &run_parser!(solve_decl(), "solve minimize foo;"),
        expect_test::expect![[
            r#"Solve { directive: Minimize(Path(Path { path: [Ident { name: "foo", span: "test":15..18 }], is_absolute: false, span: "test":15..18 })), span: "test":0..19 }"#
        ]],
    );
    check(
        &run_parser!(solve_decl(), "solve maximize foo;"),
        expect_test::expect![[
            r#"Solve { directive: Maximize(Path(Path { path: [Ident { name: "foo", span: "test":15..18 }], is_absolute: false, span: "test":15..18 })), span: "test":0..19 }"#
        ]],
    );

    check(
        &run_parser!(solve_decl(), "solve maximize x + y;"),
        expect_test::expect![[
            r#"Solve { directive: Maximize(BinaryOp { op: Add, lhs: Path(Path { path: [Ident { name: "x", span: "test":15..16 }], is_absolute: false, span: "test":15..16 }), rhs: Path(Path { path: [Ident { name: "y", span: "test":19..20 }], is_absolute: false, span: "test":19..20 }), span: "test":15..20 }), span: "test":0..21 }"#
        ]],
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
    check(
        &run_parser!(expr(), "123"),
        expect_test::expect![[r#"Immediate { value: Int(123), span: "test":0..3 }"#]],
    );
    check(
        &run_parser!(expr(), "foo"),
        expect_test::expect![[
            r#"Path(Path { path: [Ident { name: "foo", span: "test":0..3 }], is_absolute: false, span: "test":0..3 })"#
        ]],
    );
}

#[test]
fn unary_op_exprs() {
    check(
        &run_parser!(expr(), "!a"),
        expect_test::expect![[
            r#"UnaryOp { op: Not, expr: Path(Path { path: [Ident { name: "a", span: "test":1..2 }], is_absolute: false, span: "test":1..2 }), span: "test":0..2 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "+a"),
        expect_test::expect![[
            r#"UnaryOp { op: Pos, expr: Path(Path { path: [Ident { name: "a", span: "test":1..2 }], is_absolute: false, span: "test":1..2 }), span: "test":0..2 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "-a"),
        expect_test::expect![[
            r#"UnaryOp { op: Neg, expr: Path(Path { path: [Ident { name: "a", span: "test":1..2 }], is_absolute: false, span: "test":1..2 }), span: "test":0..2 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "+7"),
        expect_test::expect![[
            r#"UnaryOp { op: Pos, expr: Immediate { value: Int(7), span: "test":1..2 }, span: "test":0..2 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "+3.4"),
        expect_test::expect![[
            r#"UnaryOp { op: Pos, expr: Immediate { value: Real(3.4), span: "test":1..4 }, span: "test":0..4 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "+0x456"),
        expect_test::expect![[
            r#"UnaryOp { op: Pos, expr: Immediate { value: Int(1110), span: "test":1..6 }, span: "test":0..6 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "+0b01010101"),
        expect_test::expect![[
            r#"UnaryOp { op: Pos, expr: Immediate { value: Int(85), span: "test":1..11 }, span: "test":0..11 }"#
        ]],
    );
    check(
        &run_parser!(
            expr(),
            "+0b1101000000001100101010101010101111111111101010101101010101010101"
        ),
        expect_test::expect![[
            r#"UnaryOp { op: Pos, expr: Immediate { value: BigInt(14991544915315053909), span: "test":1..67 }, span: "test":0..67 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "-1.0"),
        expect_test::expect![[
            r#"UnaryOp { op: Neg, expr: Immediate { value: Real(1.0), span: "test":1..4 }, span: "test":0..4 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "-1"),
        expect_test::expect![[
            r#"UnaryOp { op: Neg, expr: Immediate { value: Int(1), span: "test":1..2 }, span: "test":0..2 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "-0x133"),
        expect_test::expect![[
            r#"UnaryOp { op: Neg, expr: Immediate { value: Int(307), span: "test":1..6 }, span: "test":0..6 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "-0b1101"),
        expect_test::expect![[
            r#"UnaryOp { op: Neg, expr: Immediate { value: Int(13), span: "test":1..7 }, span: "test":0..7 }"#
        ]],
    );
    check(
        &run_parser!(
            expr(),
            "-0b1101000000001100101010101010101010101010101010101101010101010101"
        ),
        expect_test::expect![[
            r#"UnaryOp { op: Neg, expr: Immediate { value: BigInt(14991544909594023253), span: "test":1..67 }, span: "test":0..67 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "! - - !  -+  -1"),
        expect_test::expect![[
            r#"UnaryOp { op: Not, expr: UnaryOp { op: Neg, expr: UnaryOp { op: Neg, expr: UnaryOp { op: Not, expr: UnaryOp { op: Neg, expr: UnaryOp { op: Pos, expr: UnaryOp { op: Neg, expr: Immediate { value: Int(1), span: "test":14..15 }, span: "test":13..15 }, span: "test":10..15 }, span: "test":9..15 }, span: "test":6..15 }, span: "test":4..15 }, span: "test":2..15 }, span: "test":0..15 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "+ {- x} '  '  "),
        expect_test::expect![[
            r#"UnaryOp { op: Pos, expr: UnaryOp { op: NextState, expr: UnaryOp { op: NextState, expr: Block(Block { statements: [], final_expr: UnaryOp { op: Neg, expr: Path(Path { path: [Ident { name: "x", span: "test":5..6 }], is_absolute: false, span: "test":5..6 }), span: "test":3..6 }, span: "test":2..7 }), span: "test":2..9 }, span: "test":2..12 }, span: "test":0..12 }"#
        ]],
    );
}

#[test]
fn binary_op_exprs() {
    check(
        &run_parser!(expr(), "a * 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: Mul, lhs: Path(Path { path: [Ident { name: "a", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), rhs: Immediate { value: Real(2.0), span: "test":4..7 }, span: "test":0..7 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a / 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: Div, lhs: Path(Path { path: [Ident { name: "a", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), rhs: Immediate { value: Real(2.0), span: "test":4..7 }, span: "test":0..7 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a % 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: Mod, lhs: Path(Path { path: [Ident { name: "a", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), rhs: Immediate { value: Real(2.0), span: "test":4..7 }, span: "test":0..7 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a + 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: Add, lhs: Path(Path { path: [Ident { name: "a", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), rhs: Immediate { value: Real(2.0), span: "test":4..7 }, span: "test":0..7 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a - 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: Sub, lhs: Path(Path { path: [Ident { name: "a", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), rhs: Immediate { value: Real(2.0), span: "test":4..7 }, span: "test":0..7 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a+2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: Add, lhs: Path(Path { path: [Ident { name: "a", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), rhs: Immediate { value: Real(2.0), span: "test":2..5 }, span: "test":0..5 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a-2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: Sub, lhs: Path(Path { path: [Ident { name: "a", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), rhs: Immediate { value: Real(2.0), span: "test":2..5 }, span: "test":0..5 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a < 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThan, lhs: Path(Path { path: [Ident { name: "a", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), rhs: Immediate { value: Real(2.0), span: "test":4..7 }, span: "test":0..7 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a > 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: GreaterThan, lhs: Path(Path { path: [Ident { name: "a", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), rhs: Immediate { value: Real(2.0), span: "test":4..7 }, span: "test":0..7 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a <= 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThanOrEqual, lhs: Path(Path { path: [Ident { name: "a", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), rhs: Immediate { value: Real(2.0), span: "test":5..8 }, span: "test":0..8 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a >= 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: GreaterThanOrEqual, lhs: Path(Path { path: [Ident { name: "a", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), rhs: Immediate { value: Real(2.0), span: "test":5..8 }, span: "test":0..8 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a == 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: Equal, lhs: Path(Path { path: [Ident { name: "a", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), rhs: Immediate { value: Real(2.0), span: "test":5..8 }, span: "test":0..8 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a != 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: NotEqual, lhs: Path(Path { path: [Ident { name: "a", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), rhs: Immediate { value: Real(2.0), span: "test":5..8 }, span: "test":0..8 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a && b"),
        expect_test::expect![[
            r#"BinaryOp { op: LogicalAnd, lhs: Path(Path { path: [Ident { name: "a", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), rhs: Path(Path { path: [Ident { name: "b", span: "test":5..6 }], is_absolute: false, span: "test":5..6 }), span: "test":0..6 }"#
        ]],
    );

    check(
        &run_parser!(expr(), "a || b"),
        expect_test::expect![[
            r#"BinaryOp { op: LogicalOr, lhs: Path(Path { path: [Ident { name: "a", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), rhs: Path(Path { path: [Ident { name: "b", span: "test":5..6 }], is_absolute: false, span: "test":5..6 }), span: "test":0..6 }"#
        ]],
    );

    check(
        &run_parser!(expr(), "a || b && c || d && !e"),
        expect_test::expect![[
            r#"BinaryOp { op: LogicalOr, lhs: BinaryOp { op: LogicalOr, lhs: Path(Path { path: [Ident { name: "a", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), rhs: BinaryOp { op: LogicalAnd, lhs: Path(Path { path: [Ident { name: "b", span: "test":5..6 }], is_absolute: false, span: "test":5..6 }), rhs: Path(Path { path: [Ident { name: "c", span: "test":10..11 }], is_absolute: false, span: "test":10..11 }), span: "test":5..11 }, span: "test":0..11 }, rhs: BinaryOp { op: LogicalAnd, lhs: Path(Path { path: [Ident { name: "d", span: "test":15..16 }], is_absolute: false, span: "test":15..16 }), rhs: UnaryOp { op: Not, expr: Path(Path { path: [Ident { name: "e", span: "test":21..22 }], is_absolute: false, span: "test":21..22 }), span: "test":20..22 }, span: "test":15..22 }, span: "test":0..22 }"#
        ]],
    );
}

#[test]
fn complex_exprs() {
    check(
        &run_parser!(expr(), "2 * b * 3"),
        expect_test::expect![[
            r#"BinaryOp { op: Mul, lhs: BinaryOp { op: Mul, lhs: Immediate { value: Int(2), span: "test":0..1 }, rhs: Path(Path { path: [Ident { name: "b", span: "test":4..5 }], is_absolute: false, span: "test":4..5 }), span: "test":0..5 }, rhs: Immediate { value: Int(3), span: "test":8..9 }, span: "test":0..9 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2 < b * 3"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThan, lhs: Immediate { value: Int(2), span: "test":0..1 }, rhs: BinaryOp { op: Mul, lhs: Path(Path { path: [Ident { name: "b", span: "test":4..5 }], is_absolute: false, span: "test":4..5 }), rhs: Immediate { value: Int(3), span: "test":8..9 }, span: "test":4..9 }, span: "test":0..9 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2.0 > b * 3.0"),
        expect_test::expect![[
            r#"BinaryOp { op: GreaterThan, lhs: Immediate { value: Real(2.0), span: "test":0..3 }, rhs: BinaryOp { op: Mul, lhs: Path(Path { path: [Ident { name: "b", span: "test":6..7 }], is_absolute: false, span: "test":6..7 }), rhs: Immediate { value: Real(3.0), span: "test":10..13 }, span: "test":6..13 }, span: "test":0..13 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2.0 * b < 3.0"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThan, lhs: BinaryOp { op: Mul, lhs: Immediate { value: Real(2.0), span: "test":0..3 }, rhs: Path(Path { path: [Ident { name: "b", span: "test":6..7 }], is_absolute: false, span: "test":6..7 }), span: "test":0..7 }, rhs: Immediate { value: Real(3.0), span: "test":10..13 }, span: "test":0..13 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2 > b < 3"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThan, lhs: BinaryOp { op: GreaterThan, lhs: Immediate { value: Int(2), span: "test":0..1 }, rhs: Path(Path { path: [Ident { name: "b", span: "test":4..5 }], is_absolute: false, span: "test":4..5 }), span: "test":0..5 }, rhs: Immediate { value: Int(3), span: "test":8..9 }, span: "test":0..9 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2 != b < 3"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThan, lhs: BinaryOp { op: NotEqual, lhs: Immediate { value: Int(2), span: "test":0..1 }, rhs: Path(Path { path: [Ident { name: "b", span: "test":5..6 }], is_absolute: false, span: "test":5..6 }), span: "test":0..6 }, rhs: Immediate { value: Int(3), span: "test":9..10 }, span: "test":0..10 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2 < b != 3"),
        expect_test::expect![[
            r#"BinaryOp { op: NotEqual, lhs: BinaryOp { op: LessThan, lhs: Immediate { value: Int(2), span: "test":0..1 }, rhs: Path(Path { path: [Ident { name: "b", span: "test":4..5 }], is_absolute: false, span: "test":4..5 }), span: "test":0..5 }, rhs: Immediate { value: Int(3), span: "test":9..10 }, span: "test":0..10 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a > b * c < d"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThan, lhs: BinaryOp { op: GreaterThan, lhs: Path(Path { path: [Ident { name: "a", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), rhs: BinaryOp { op: Mul, lhs: Path(Path { path: [Ident { name: "b", span: "test":4..5 }], is_absolute: false, span: "test":4..5 }), rhs: Path(Path { path: [Ident { name: "c", span: "test":8..9 }], is_absolute: false, span: "test":8..9 }), span: "test":4..9 }, span: "test":0..9 }, rhs: Path(Path { path: [Ident { name: "d", span: "test":12..13 }], is_absolute: false, span: "test":12..13 }), span: "test":0..13 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2 + 3 * 4"),
        expect_test::expect![[
            r#"BinaryOp { op: Add, lhs: Immediate { value: Int(2), span: "test":0..1 }, rhs: BinaryOp { op: Mul, lhs: Immediate { value: Int(3), span: "test":4..5 }, rhs: Immediate { value: Int(4), span: "test":8..9 }, span: "test":4..9 }, span: "test":0..9 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "10 - 8 / 4"),
        expect_test::expect![[
            r#"BinaryOp { op: Sub, lhs: Immediate { value: Int(10), span: "test":0..2 }, rhs: BinaryOp { op: Div, lhs: Immediate { value: Int(8), span: "test":5..6 }, rhs: Immediate { value: Int(4), span: "test":9..10 }, span: "test":5..10 }, span: "test":0..10 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "10 + 8 % 4"),
        expect_test::expect![[
            r#"BinaryOp { op: Add, lhs: Immediate { value: Int(10), span: "test":0..2 }, rhs: BinaryOp { op: Mod, lhs: Immediate { value: Int(8), span: "test":5..6 }, rhs: Immediate { value: Int(4), span: "test":9..10 }, span: "test":5..10 }, span: "test":0..10 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2 + 3 * 4 < 5"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThan, lhs: BinaryOp { op: Add, lhs: Immediate { value: Int(2), span: "test":0..1 }, rhs: BinaryOp { op: Mul, lhs: Immediate { value: Int(3), span: "test":4..5 }, rhs: Immediate { value: Int(4), span: "test":8..9 }, span: "test":4..9 }, span: "test":0..9 }, rhs: Immediate { value: Int(5), span: "test":12..13 }, span: "test":0..13 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2 * 3 / 4 < 5"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThan, lhs: BinaryOp { op: Div, lhs: BinaryOp { op: Mul, lhs: Immediate { value: Int(2), span: "test":0..1 }, rhs: Immediate { value: Int(3), span: "test":4..5 }, span: "test":0..5 }, rhs: Immediate { value: Int(4), span: "test":8..9 }, span: "test":0..9 }, rhs: Immediate { value: Int(5), span: "test":12..13 }, span: "test":0..13 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "10 - 5 + 3 > 7"),
        expect_test::expect![[
            r#"BinaryOp { op: GreaterThan, lhs: BinaryOp { op: Add, lhs: BinaryOp { op: Sub, lhs: Immediate { value: Int(10), span: "test":0..2 }, rhs: Immediate { value: Int(5), span: "test":5..6 }, span: "test":0..6 }, rhs: Immediate { value: Int(3), span: "test":9..10 }, span: "test":0..10 }, rhs: Immediate { value: Int(7), span: "test":13..14 }, span: "test":0..14 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "10 % 2 * 4 < 3"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThan, lhs: BinaryOp { op: Mul, lhs: BinaryOp { op: Mod, lhs: Immediate { value: Int(10), span: "test":0..2 }, rhs: Immediate { value: Int(2), span: "test":5..6 }, span: "test":0..6 }, rhs: Immediate { value: Int(4), span: "test":9..10 }, span: "test":0..10 }, rhs: Immediate { value: Int(3), span: "test":13..14 }, span: "test":0..14 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2 + 3 * 4 - 5 / 2 > 1"),
        expect_test::expect![[
            r#"BinaryOp { op: GreaterThan, lhs: BinaryOp { op: Sub, lhs: BinaryOp { op: Add, lhs: Immediate { value: Int(2), span: "test":0..1 }, rhs: BinaryOp { op: Mul, lhs: Immediate { value: Int(3), span: "test":4..5 }, rhs: Immediate { value: Int(4), span: "test":8..9 }, span: "test":4..9 }, span: "test":0..9 }, rhs: BinaryOp { op: Div, lhs: Immediate { value: Int(5), span: "test":12..13 }, rhs: Immediate { value: Int(2), span: "test":16..17 }, span: "test":12..17 }, span: "test":0..17 }, rhs: Immediate { value: Int(1), span: "test":20..21 }, span: "test":0..21 }"#
        ]],
    );
}

#[test]
fn parens_exprs() {
    check(
        &run_parser!(expr(), "(1 + 2) * 3"),
        expect_test::expect![[
            r#"BinaryOp { op: Mul, lhs: BinaryOp { op: Add, lhs: Immediate { value: Int(1), span: "test":1..2 }, rhs: Immediate { value: Int(2), span: "test":5..6 }, span: "test":1..6 }, rhs: Immediate { value: Int(3), span: "test":10..11 }, span: "test":1..11 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "1 * (2 + 3)"),
        expect_test::expect![[
            r#"BinaryOp { op: Mul, lhs: Immediate { value: Int(1), span: "test":0..1 }, rhs: BinaryOp { op: Add, lhs: Immediate { value: Int(2), span: "test":5..6 }, rhs: Immediate { value: Int(3), span: "test":9..10 }, span: "test":5..10 }, span: "test":0..11 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "(1 + 2) * (3 + 4)"),
        expect_test::expect![[
            r#"BinaryOp { op: Mul, lhs: BinaryOp { op: Add, lhs: Immediate { value: Int(1), span: "test":1..2 }, rhs: Immediate { value: Int(2), span: "test":5..6 }, span: "test":1..6 }, rhs: BinaryOp { op: Add, lhs: Immediate { value: Int(3), span: "test":11..12 }, rhs: Immediate { value: Int(4), span: "test":15..16 }, span: "test":11..16 }, span: "test":1..17 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "(1 + (2 * 3)) * 4"),
        expect_test::expect![[
            r#"BinaryOp { op: Mul, lhs: BinaryOp { op: Add, lhs: Immediate { value: Int(1), span: "test":1..2 }, rhs: BinaryOp { op: Mul, lhs: Immediate { value: Int(2), span: "test":6..7 }, rhs: Immediate { value: Int(3), span: "test":10..11 }, span: "test":6..11 }, span: "test":1..12 }, rhs: Immediate { value: Int(4), span: "test":16..17 }, span: "test":1..17 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "(1 * (2 + 3)) * 4"),
        expect_test::expect![[
            r#"BinaryOp { op: Mul, lhs: BinaryOp { op: Mul, lhs: Immediate { value: Int(1), span: "test":1..2 }, rhs: BinaryOp { op: Add, lhs: Immediate { value: Int(2), span: "test":6..7 }, rhs: Immediate { value: Int(3), span: "test":10..11 }, span: "test":6..11 }, span: "test":1..12 }, rhs: Immediate { value: Int(4), span: "test":16..17 }, span: "test":1..17 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "((1 + 2) * 3) * 4"),
        expect_test::expect![[
            r#"BinaryOp { op: Mul, lhs: BinaryOp { op: Mul, lhs: BinaryOp { op: Add, lhs: Immediate { value: Int(1), span: "test":2..3 }, rhs: Immediate { value: Int(2), span: "test":6..7 }, span: "test":2..7 }, rhs: Immediate { value: Int(3), span: "test":11..12 }, span: "test":2..12 }, rhs: Immediate { value: Int(4), span: "test":16..17 }, span: "test":2..17 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "((1 + 2) * (3 + 4)) * 5"),
        expect_test::expect![[
            r#"BinaryOp { op: Mul, lhs: BinaryOp { op: Mul, lhs: BinaryOp { op: Add, lhs: Immediate { value: Int(1), span: "test":2..3 }, rhs: Immediate { value: Int(2), span: "test":6..7 }, span: "test":2..7 }, rhs: BinaryOp { op: Add, lhs: Immediate { value: Int(3), span: "test":12..13 }, rhs: Immediate { value: Int(4), span: "test":16..17 }, span: "test":12..17 }, span: "test":2..18 }, rhs: Immediate { value: Int(5), span: "test":22..23 }, span: "test":2..23 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "(1 + 2) * 3 / 4"),
        expect_test::expect![[
            r#"BinaryOp { op: Div, lhs: BinaryOp { op: Mul, lhs: BinaryOp { op: Add, lhs: Immediate { value: Int(1), span: "test":1..2 }, rhs: Immediate { value: Int(2), span: "test":5..6 }, span: "test":1..6 }, rhs: Immediate { value: Int(3), span: "test":10..11 }, span: "test":1..11 }, rhs: Immediate { value: Int(4), span: "test":14..15 }, span: "test":1..15 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "1 / (2 + 3) * 4"),
        expect_test::expect![[
            r#"BinaryOp { op: Mul, lhs: BinaryOp { op: Div, lhs: Immediate { value: Int(1), span: "test":0..1 }, rhs: BinaryOp { op: Add, lhs: Immediate { value: Int(2), span: "test":5..6 }, rhs: Immediate { value: Int(3), span: "test":9..10 }, span: "test":5..10 }, span: "test":0..11 }, rhs: Immediate { value: Int(4), span: "test":14..15 }, span: "test":0..15 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "(1 < 2) && (3 > 4)"),
        expect_test::expect![[
            r#"BinaryOp { op: LogicalAnd, lhs: BinaryOp { op: LessThan, lhs: Immediate { value: Int(1), span: "test":1..2 }, rhs: Immediate { value: Int(2), span: "test":5..6 }, span: "test":1..6 }, rhs: BinaryOp { op: GreaterThan, lhs: Immediate { value: Int(3), span: "test":12..13 }, rhs: Immediate { value: Int(4), span: "test":16..17 }, span: "test":12..17 }, span: "test":1..18 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "(1 == 2) || (3 != 4)"),
        expect_test::expect![[
            r#"BinaryOp { op: LogicalOr, lhs: BinaryOp { op: Equal, lhs: Immediate { value: Int(1), span: "test":1..2 }, rhs: Immediate { value: Int(2), span: "test":6..7 }, span: "test":1..7 }, rhs: BinaryOp { op: NotEqual, lhs: Immediate { value: Int(3), span: "test":13..14 }, rhs: Immediate { value: Int(4), span: "test":18..19 }, span: "test":13..19 }, span: "test":1..20 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "1 < (2 && 3) > 4"),
        expect_test::expect![[
            r#"BinaryOp { op: GreaterThan, lhs: BinaryOp { op: LessThan, lhs: Immediate { value: Int(1), span: "test":0..1 }, rhs: BinaryOp { op: LogicalAnd, lhs: Immediate { value: Int(2), span: "test":5..6 }, rhs: Immediate { value: Int(3), span: "test":10..11 }, span: "test":5..11 }, span: "test":0..12 }, rhs: Immediate { value: Int(4), span: "test":15..16 }, span: "test":0..16 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "1 && (2 || 3)"),
        expect_test::expect![[
            r#"BinaryOp { op: LogicalAnd, lhs: Immediate { value: Int(1), span: "test":0..1 }, rhs: BinaryOp { op: LogicalOr, lhs: Immediate { value: Int(2), span: "test":6..7 }, rhs: Immediate { value: Int(3), span: "test":11..12 }, span: "test":6..12 }, span: "test":0..13 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "1 == (2 || 3) != 4"),
        expect_test::expect![[
            r#"BinaryOp { op: NotEqual, lhs: BinaryOp { op: Equal, lhs: Immediate { value: Int(1), span: "test":0..1 }, rhs: BinaryOp { op: LogicalOr, lhs: Immediate { value: Int(2), span: "test":6..7 }, rhs: Immediate { value: Int(3), span: "test":11..12 }, span: "test":6..12 }, span: "test":0..13 }, rhs: Immediate { value: Int(4), span: "test":17..18 }, span: "test":0..18 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "-(1 + 2)"),
        expect_test::expect![[
            r#"UnaryOp { op: Neg, expr: BinaryOp { op: Add, lhs: Immediate { value: Int(1), span: "test":2..3 }, rhs: Immediate { value: Int(2), span: "test":6..7 }, span: "test":2..7 }, span: "test":0..8 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "!(a < b)"),
        expect_test::expect![[
            r#"UnaryOp { op: Not, expr: BinaryOp { op: LessThan, lhs: Path(Path { path: [Ident { name: "a", span: "test":2..3 }], is_absolute: false, span: "test":2..3 }), rhs: Path(Path { path: [Ident { name: "b", span: "test":6..7 }], is_absolute: false, span: "test":6..7 }), span: "test":2..7 }, span: "test":0..8 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "(1)"),
        expect_test::expect![[r#"Immediate { value: Int(1), span: "test":1..2 }"#]],
    );
    check(
        &run_parser!(expr(), "(a)"),
        expect_test::expect![[
            r#"Path(Path { path: [Ident { name: "a", span: "test":1..2 }], is_absolute: false, span: "test":1..2 })"#
        ]],
    );
    check(
        &run_parser!(expr(), "()"),
        expect_test::expect![[r#"
            expected `::`, `::`, `!`, `+`, `-`, `{`, `{`, `(`, `[`, `if`, or `cond`, found `)`
            @1..2: expected `::`, `::`, `!`, `+`, `-`, `{`, `{`, `(`, `[`, `if`, or `cond`
        "#]],
    );
    check(
        &run_parser!(expr(), "(if a < b { 1 } else { 2 })"),
        expect_test::expect![[
            r#"If { condition: BinaryOp { op: LessThan, lhs: Path(Path { path: [Ident { name: "a", span: "test":4..5 }], is_absolute: false, span: "test":4..5 }), rhs: Path(Path { path: [Ident { name: "b", span: "test":8..9 }], is_absolute: false, span: "test":8..9 }), span: "test":4..9 }, then_block: Block { statements: [], final_expr: Immediate { value: Int(1), span: "test":12..13 }, span: "test":10..15 }, else_block: Block { statements: [], final_expr: Immediate { value: Int(2), span: "test":23..24 }, span: "test":21..26 }, span: "test":1..26 }"#
        ]],
    );
    check(
        &run_parser!(expr(), "(foo(a, b, c))"),
        expect_test::expect![[
            r#"Call { name: Path { path: [Ident { name: "foo", span: "test":1..4 }], is_absolute: false, span: "test":1..4 }, args: [Path(Path { path: [Ident { name: "a", span: "test":5..6 }], is_absolute: false, span: "test":5..6 }), Path(Path { path: [Ident { name: "b", span: "test":8..9 }], is_absolute: false, span: "test":8..9 }), Path(Path { path: [Ident { name: "c", span: "test":11..12 }], is_absolute: false, span: "test":11..12 })], span: "test":1..13 }"#
        ]],
    );
}

#[test]
fn enums() {
    check(
        &run_parser!(enum_decl(), "enum MyEnum = Variant1 | Variant2;"),
        expect_test::expect![[
            r#"Enum(EnumDecl { name: Ident { name: "MyEnum", span: "test":5..11 }, variants: [Ident { name: "Variant1", span: "test":14..22 }, Ident { name: "Variant2", span: "test":25..33 }], span: "test":0..34 })"#
        ]],
    );
    check(
        &run_parser!(enum_decl(), "enum MyEnum = Variant1;"),
        expect_test::expect![[
            r#"Enum(EnumDecl { name: Ident { name: "MyEnum", span: "test":5..11 }, variants: [Ident { name: "Variant1", span: "test":14..22 }], span: "test":0..23 })"#
        ]],
    );
    check(
        &run_parser!(expr(), "MyEnum::Variant1"),
        expect_test::expect![[
            r#"Path(Path { path: [Ident { name: "MyEnum", span: "test":0..6 }, Ident { name: "Variant1", span: "test":8..16 }], is_absolute: false, span: "test":0..16 })"#
        ]],
    );
    check(
        &run_parser!(
            let_decl(expr()),
            r#"
            let x = MyEnum::Variant3;
            "#
        ),
        expect_test::expect![[
            r#"Let { name: Ident { name: "x", span: "test":17..18 }, ty: None, init: Some(Path(Path { path: [Ident { name: "MyEnum", span: "test":21..27 }, Ident { name: "Variant3", span: "test":29..37 }], is_absolute: false, span: "test":21..37 })), span: "test":13..38 }"#
        ]],
    );
    check(
        &run_parser!(
            let_decl(expr()),
            r#"
            let e: ::path::to::MyEnum;
            "#
        ),
        expect_test::expect![[
            r#"Let { name: Ident { name: "e", span: "test":17..18 }, ty: Some(CustomType { path: Path { path: [Ident { name: "path", span: "test":22..26 }, Ident { name: "to", span: "test":28..30 }, Ident { name: "MyEnum", span: "test":32..38 }], is_absolute: true, span: "test":20..38 }, span: "test":20..38 }), init: None, span: "test":13..39 }"#
        ]],
    );
}

#[test]
fn custom_types() {
    check(
        &run_parser!(type_(expr()), "custom_type"),
        expect_test::expect![[
            r#"CustomType { path: Path { path: [Ident { name: "custom_type", span: "test":0..11 }], is_absolute: false, span: "test":0..11 }, span: "test":0..11 }"#
        ]],
    );
    check(
        &run_parser!(type_decl(), "type MyInt = int;"),
        expect_test::expect![[
            r#"NewType { name: Ident { name: "MyInt", span: "test":5..10 }, ty: Primitive { kind: Int, span: "test":13..16 }, span: "test":0..17 }"#
        ]],
    );
    check(
        &run_parser!(type_decl(), "type MyReal = real;"),
        expect_test::expect![[
            r#"NewType { name: Ident { name: "MyReal", span: "test":5..11 }, ty: Primitive { kind: Real, span: "test":14..18 }, span: "test":0..19 }"#
        ]],
    );
    check(
        &run_parser!(type_decl(), "type MyBool = bool;"),
        expect_test::expect![[
            r#"NewType { name: Ident { name: "MyBool", span: "test":5..11 }, ty: Primitive { kind: Bool, span: "test":14..18 }, span: "test":0..19 }"#
        ]],
    );
    check(
        &run_parser!(type_decl(), "type MyString = string;"),
        expect_test::expect![[
            r#"NewType { name: Ident { name: "MyString", span: "test":5..13 }, ty: Primitive { kind: String, span: "test":16..22 }, span: "test":0..23 }"#
        ]],
    );
    check(
        &run_parser!(type_decl(), "type IntArray = int[5];"),
        expect_test::expect![[
            r#"NewType { name: Ident { name: "IntArray", span: "test":5..13 }, ty: Array { ty: Primitive { kind: Int, span: "test":16..19 }, range: Immediate { value: Int(5), span: "test":20..21 }, span: "test":16..22 }, span: "test":0..23 }"#
        ]],
    );
    check(
        &run_parser!(type_decl(), "type MyTuple = { int, real, z: string };"),
        expect_test::expect![[
            r#"NewType { name: Ident { name: "MyTuple", span: "test":5..12 }, ty: Tuple { fields: [(None, Primitive { kind: Int, span: "test":17..20 }), (None, Primitive { kind: Real, span: "test":22..26 }), (Some(Ident { name: "z", span: "test":28..29 }), Primitive { kind: String, span: "test":31..37 })], span: "test":15..39 }, span: "test":0..40 }"#
        ]],
    );
    check(
        &run_parser!(type_decl(), "type MyAliasInt = MyInt;"),
        expect_test::expect![[
            r#"NewType { name: Ident { name: "MyAliasInt", span: "test":5..15 }, ty: CustomType { path: Path { path: [Ident { name: "MyInt", span: "test":18..23 }], is_absolute: false, span: "test":18..23 }, span: "test":18..23 }, span: "test":0..24 }"#
        ]],
    );
}

#[test]
fn idents() {
    check(
        &run_parser!(ident(), "foobar"),
        expect_test::expect![[r#"Ident { name: "foobar", span: "test":0..6 }"#]],
    );
    check(
        &run_parser!(ident(), "foo_bar"),
        expect_test::expect![[r#"Ident { name: "foo_bar", span: "test":0..7 }"#]],
    );
    check(
        &run_parser!(ident(), "FOO_bar"),
        expect_test::expect![[r#"Ident { name: "FOO_bar", span: "test":0..7 }"#]],
    );
    check(
        &run_parser!(ident(), "__FOO"),
        expect_test::expect![[r#"Ident { name: "__FOO", span: "test":0..5 }"#]],
    );
    check(
        &run_parser!(ident(), "_2_FOO1"),
        expect_test::expect![[r#"Ident { name: "_2_FOO1", span: "test":0..7 }"#]],
    );
    check(
        &run_parser!(ident(), "_"),
        expect_test::expect![[r#"Ident { name: "_", span: "test":0..1 }"#]],
    );
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
        expect_test::expect![[
            r#"Path { path: [Ident { name: "foo", span: "test":0..3 }, Ident { name: "bar", span: "test":5..8 }], is_absolute: false, span: "test":0..8 }"#
        ]],
    );
    check(
        &run_parser!(path(), "_foo_::_bar"),
        expect_test::expect![[
            r#"Path { path: [Ident { name: "_foo_", span: "test":0..5 }, Ident { name: "_bar", span: "test":7..11 }], is_absolute: false, span: "test":0..11 }"#
        ]],
    );
    check(
        &run_parser!(path(), "_::_"),
        expect_test::expect![[
            r#"Path { path: [Ident { name: "_", span: "test":0..1 }, Ident { name: "_", span: "test":3..4 }], is_absolute: false, span: "test":0..4 }"#
        ]],
    );
    check(
        &run_parser!(path(), "t2::_3t::t4_::t"),
        expect_test::expect![[
            r#"Path { path: [Ident { name: "t2", span: "test":0..2 }, Ident { name: "_3t", span: "test":4..7 }, Ident { name: "t4_", span: "test":9..12 }, Ident { name: "t", span: "test":14..15 }], is_absolute: false, span: "test":0..15 }"#
        ]],
    );
    check(
        &run_parser!(path(), "::foo::bar"),
        expect_test::expect![[
            r#"Path { path: [Ident { name: "foo", span: "test":2..5 }, Ident { name: "bar", span: "test":7..10 }], is_absolute: true, span: "test":0..10 }"#
        ]],
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
        expect_test::expect![[
            r#"[Fn { fn_sig: FnSig { name: Ident { name: "foo", span: "test":4..7 }, params: [(Ident { name: "x", span: "test":8..9 }, Primitive { kind: Real, span: "test":11..15 }), (Ident { name: "y", span: "test":17..18 }, Primitive { kind: Real, span: "test":20..24 })], return_type: Primitive { kind: Real, span: "test":29..33 }, span: "test":1..33 }, body: Block { statements: [Let { name: Ident { name: "z", span: "test":44..45 }, ty: None, init: Some(Immediate { value: Real(5.0), span: "test":48..51 }), span: "test":40..52 }], final_expr: Path(Path { path: [Ident { name: "z", span: "test":57..58 }], is_absolute: false, span: "test":57..58 }), span: "test":34..60 }, span: "test":1..60 }]"#
        ]],
    );
}

#[test]
fn fn_call() {
    let src = r#"
let x = foo(a*3, c);
"#;

    check(
        &run_parser!(yurt_program(), src),
        expect_test::expect![[
            r#"[Let { name: Ident { name: "x", span: "test":5..6 }, ty: None, init: Some(Call { name: Path { path: [Ident { name: "foo", span: "test":9..12 }], is_absolute: false, span: "test":9..12 }, args: [BinaryOp { op: Mul, lhs: Path(Path { path: [Ident { name: "a", span: "test":13..14 }], is_absolute: false, span: "test":13..14 }), rhs: Immediate { value: Int(3), span: "test":15..16 }, span: "test":13..16 }, Path(Path { path: [Ident { name: "c", span: "test":18..19 }], is_absolute: false, span: "test":18..19 })], span: "test":9..20 }), span: "test":1..21 }]"#
        ]],
    );

    check(
        &run_parser!(expr(), "A::B::foo(-a, b+c)"),
        expect_test::expect![[
            r#"Call { name: Path { path: [Ident { name: "A", span: "test":0..1 }, Ident { name: "B", span: "test":3..4 }, Ident { name: "foo", span: "test":6..9 }], is_absolute: false, span: "test":0..9 }, args: [UnaryOp { op: Neg, expr: Path(Path { path: [Ident { name: "a", span: "test":11..12 }], is_absolute: false, span: "test":11..12 }), span: "test":10..12 }, BinaryOp { op: Add, lhs: Path(Path { path: [Ident { name: "b", span: "test":14..15 }], is_absolute: false, span: "test":14..15 }), rhs: Path(Path { path: [Ident { name: "c", span: "test":16..17 }], is_absolute: false, span: "test":16..17 }), span: "test":14..17 }], span: "test":0..18 }"#
        ]],
    );
}

#[test]
fn code_blocks() {
    check(
        &run_parser!(let_decl(expr()), "let x = { 0 };"),
        expect_test::expect![[
            r#"Let { name: Ident { name: "x", span: "test":4..5 }, ty: None, init: Some(Block(Block { statements: [], final_expr: Immediate { value: Int(0), span: "test":10..11 }, span: "test":8..13 })), span: "test":0..14 }"#
        ]],
    );

    check(
        &run_parser!(let_decl(expr()), "let x = { constraint x > 0.0; 0.0 };"),
        expect_test::expect![[
            r#"Let { name: Ident { name: "x", span: "test":4..5 }, ty: None, init: Some(Block(Block { statements: [Constraint { expr: BinaryOp { op: GreaterThan, lhs: Path(Path { path: [Ident { name: "x", span: "test":21..22 }], is_absolute: false, span: "test":21..22 }), rhs: Immediate { value: Real(0.0), span: "test":25..28 }, span: "test":21..28 }, span: "test":10..29 }], final_expr: Immediate { value: Real(0.0), span: "test":30..33 }, span: "test":8..35 })), span: "test":0..36 }"#
        ]],
    );

    check(
        &run_parser!(
            constraint_decl(expr()),
            "constraint { constraint { true }; x > 0 };"
        ),
        expect_test::expect![[
            r#"Constraint { expr: Block(Block { statements: [Constraint { expr: Block(Block { statements: [], final_expr: Immediate { value: Bool(true), span: "test":26..30 }, span: "test":24..32 }), span: "test":13..33 }], final_expr: BinaryOp { op: GreaterThan, lhs: Path(Path { path: [Ident { name: "x", span: "test":34..35 }], is_absolute: false, span: "test":34..35 }), rhs: Immediate { value: Int(0), span: "test":38..39 }, span: "test":34..39 }, span: "test":11..41 }), span: "test":0..42 }"#
        ]],
    );

    check(
        &run_parser!(let_decl(expr()), "let x = { 1.0 } * { 2.0 };"),
        expect_test::expect![[
            r#"Let { name: Ident { name: "x", span: "test":4..5 }, ty: None, init: Some(BinaryOp { op: Mul, lhs: Block(Block { statements: [], final_expr: Immediate { value: Real(1.0), span: "test":10..13 }, span: "test":8..15 }), rhs: Block(Block { statements: [], final_expr: Immediate { value: Real(2.0), span: "test":20..23 }, span: "test":18..25 }), span: "test":8..25 }), span: "test":0..26 }"#
        ]],
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
        expect_test::expect![[
            r#"If { condition: Path(Path { path: [Ident { name: "c", span: "test":3..4 }], is_absolute: false, span: "test":3..4 }), then_block: Block { statements: [], final_expr: Immediate { value: Int(1), span: "test":7..8 }, span: "test":5..10 }, else_block: Block { statements: [], final_expr: Immediate { value: Int(0), span: "test":18..19 }, span: "test":16..21 }, span: "test":0..21 }"#
        ]],
    );

    check(
        &run_parser!(if_expr(expr()), "if c { if c { 1 } else { 0 } } else { 2 }"),
        expect_test::expect![[
            r#"If { condition: Path(Path { path: [Ident { name: "c", span: "test":3..4 }], is_absolute: false, span: "test":3..4 }), then_block: Block { statements: [], final_expr: If { condition: Path(Path { path: [Ident { name: "c", span: "test":10..11 }], is_absolute: false, span: "test":10..11 }), then_block: Block { statements: [], final_expr: Immediate { value: Int(1), span: "test":14..15 }, span: "test":12..17 }, else_block: Block { statements: [], final_expr: Immediate { value: Int(0), span: "test":25..26 }, span: "test":23..28 }, span: "test":7..28 }, span: "test":5..30 }, else_block: Block { statements: [], final_expr: Immediate { value: Int(2), span: "test":38..39 }, span: "test":36..41 }, span: "test":0..41 }"#
        ]],
    );

    check(
        &run_parser!(if_expr(expr()), "if c { if c { 1 } else { 0 } } else { 2 }"),
        expect_test::expect![[
            r#"If { condition: Path(Path { path: [Ident { name: "c", span: "test":3..4 }], is_absolute: false, span: "test":3..4 }), then_block: Block { statements: [], final_expr: If { condition: Path(Path { path: [Ident { name: "c", span: "test":10..11 }], is_absolute: false, span: "test":10..11 }), then_block: Block { statements: [], final_expr: Immediate { value: Int(1), span: "test":14..15 }, span: "test":12..17 }, else_block: Block { statements: [], final_expr: Immediate { value: Int(0), span: "test":25..26 }, span: "test":23..28 }, span: "test":7..28 }, span: "test":5..30 }, else_block: Block { statements: [], final_expr: Immediate { value: Int(2), span: "test":38..39 }, span: "test":36..41 }, span: "test":0..41 }"#
        ]],
    );
}

#[test]
fn array_type() {
    check(
        &run_parser!(type_(expr()), r#"int[5]"#),
        expect_test::expect![[
            r#"Array { ty: Primitive { kind: Int, span: "test":0..3 }, range: Immediate { value: Int(5), span: "test":4..5 }, span: "test":0..6 }"#
        ]],
    );

    check(
        &run_parser!(type_(expr()), r#"int[MyEnum]"#),
        expect_test::expect![[
            r#"Array { ty: Primitive { kind: Int, span: "test":0..3 }, range: Path(Path { path: [Ident { name: "MyEnum", span: "test":4..10 }], is_absolute: false, span: "test":4..10 }), span: "test":0..11 }"#
        ]],
    );

    check(
        &run_parser!(type_(expr()), r#"int[N]"#),
        expect_test::expect![[
            r#"Array { ty: Primitive { kind: Int, span: "test":0..3 }, range: Path(Path { path: [Ident { name: "N", span: "test":4..5 }], is_absolute: false, span: "test":4..5 }), span: "test":0..6 }"#
        ]],
    );

    check(
        &run_parser!(
            type_(expr()),
            r#"string[foo()][{ 7 }][if true { 1 } else { 2 }]"#
        ),
        expect_test::expect![[
            r#"Array { ty: Array { ty: Array { ty: Primitive { kind: String, span: "test":0..6 }, range: If { condition: Immediate { value: Bool(true), span: "test":24..28 }, then_block: Block { statements: [], final_expr: Immediate { value: Int(1), span: "test":31..32 }, span: "test":29..34 }, else_block: Block { statements: [], final_expr: Immediate { value: Int(2), span: "test":42..43 }, span: "test":40..45 }, span: "test":21..45 }, span: "test":0..46 }, range: Block(Block { statements: [], final_expr: Immediate { value: Int(7), span: "test":16..17 }, span: "test":14..19 }), span: "test":0..20 }, range: Call { name: Path { path: [Ident { name: "foo", span: "test":7..10 }], is_absolute: false, span: "test":7..10 }, args: [], span: "test":7..12 }, span: "test":0..13 }"#
        ]],
    );

    check(
        &run_parser!(type_(expr()), r#"real[N][9][M][3]"#),
        expect_test::expect![[
            r#"Array { ty: Array { ty: Array { ty: Array { ty: Primitive { kind: Real, span: "test":0..4 }, range: Immediate { value: Int(3), span: "test":14..15 }, span: "test":0..16 }, range: Path(Path { path: [Ident { name: "M", span: "test":11..12 }], is_absolute: false, span: "test":11..12 }), span: "test":0..13 }, range: Immediate { value: Int(9), span: "test":8..9 }, span: "test":0..10 }, range: Path(Path { path: [Ident { name: "N", span: "test":5..6 }], is_absolute: false, span: "test":5..6 }), span: "test":0..7 }"#
        ]],
    );

    check(
        &run_parser!(type_(expr()), r#"{int, { real, string }}[N][9]"#),
        expect_test::expect![[
            r#"Array { ty: Array { ty: Tuple { fields: [(None, Primitive { kind: Int, span: "test":1..4 }), (None, Tuple { fields: [(None, Primitive { kind: Real, span: "test":8..12 }), (None, Primitive { kind: String, span: "test":14..20 })], span: "test":6..22 })], span: "test":0..23 }, range: Immediate { value: Int(9), span: "test":27..28 }, span: "test":0..29 }, range: Path(Path { path: [Ident { name: "N", span: "test":24..25 }], is_absolute: false, span: "test":24..25 }), span: "test":0..26 }"#
        ]],
    );

    check(
        &run_parser!(let_decl(expr()), r#"let a: int[];"#),
        expect_test::expect![[r#"
            expected `::`, `::`, `!`, `+`, `-`, `{`, `{`, `(`, `[`, `if`, or `cond`, found `]`
            @11..12: expected `::`, `::`, `!`, `+`, `-`, `{`, `{`, `(`, `[`, `if`, or `cond`
        "#]],
    );
}
#[test]
fn array_expressions() {
    check(
        &run_parser!(expr(), r#"[5]"#),
        expect_test::expect![[
            r#"Array { elements: [Immediate { value: Int(5), span: "test":1..2 }], span: "test":0..3 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"[5,]"#),
        expect_test::expect![[
            r#"Array { elements: [Immediate { value: Int(5), span: "test":1..2 }], span: "test":0..4 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"[5, 4]"#),
        expect_test::expect![[
            r#"Array { elements: [Immediate { value: Int(5), span: "test":1..2 }, Immediate { value: Int(4), span: "test":4..5 }], span: "test":0..6 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"[[ 1 ],]"#),
        expect_test::expect![[
            r#"Array { elements: [Array { elements: [Immediate { value: Int(1), span: "test":3..4 }], span: "test":1..6 }], span: "test":0..8 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"[[1, 2], 3]"#), // This should fail in semantic analysis
        expect_test::expect![[
            r#"Array { elements: [Array { elements: [Immediate { value: Int(1), span: "test":2..3 }, Immediate { value: Int(2), span: "test":5..6 }], span: "test":1..7 }, Immediate { value: Int(3), span: "test":9..10 }], span: "test":0..11 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"[[1, 2], [3, 4]]"#),
        expect_test::expect![[
            r#"Array { elements: [Array { elements: [Immediate { value: Int(1), span: "test":2..3 }, Immediate { value: Int(2), span: "test":5..6 }], span: "test":1..7 }, Array { elements: [Immediate { value: Int(3), span: "test":10..11 }, Immediate { value: Int(4), span: "test":13..14 }], span: "test":9..15 }], span: "test":0..16 }"#
        ]],
    );

    check(
        &run_parser!(
            expr(),
            r#"[[foo(), { 2 }], [if true { 1 } else { 2 }, t.0]]"#
        ),
        expect_test::expect![[
            r#"Array { elements: [Array { elements: [Call { name: Path { path: [Ident { name: "foo", span: "test":2..5 }], is_absolute: false, span: "test":2..5 }, args: [], span: "test":2..7 }, Block(Block { statements: [], final_expr: Immediate { value: Int(2), span: "test":11..12 }, span: "test":9..14 })], span: "test":1..15 }, Array { elements: [If { condition: Immediate { value: Bool(true), span: "test":21..25 }, then_block: Block { statements: [], final_expr: Immediate { value: Int(1), span: "test":28..29 }, span: "test":26..31 }, else_block: Block { statements: [], final_expr: Immediate { value: Int(2), span: "test":39..40 }, span: "test":37..42 }, span: "test":18..42 }, TupleFieldAccess { tuple: Path(Path { path: [Ident { name: "t", span: "test":44..45 }], is_absolute: false, span: "test":44..45 }), field: Left(0), span: "test":44..47 }], span: "test":17..48 }], span: "test":0..49 }"#
        ]],
    );
}

#[test]
fn array_field_accesss() {
    check(
        &run_parser!(expr(), r#"a[5]"#),
        expect_test::expect![[
            r#"ArrayElementAccess { array: Path(Path { path: [Ident { name: "a", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), index: Immediate { value: Int(5), span: "test":2..3 }, span: "test":0..4 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"{ a }[N][foo()][M][4]"#),
        expect_test::expect![[
            r#"ArrayElementAccess { array: ArrayElementAccess { array: ArrayElementAccess { array: ArrayElementAccess { array: Block(Block { statements: [], final_expr: Path(Path { path: [Ident { name: "a", span: "test":2..3 }], is_absolute: false, span: "test":2..3 }), span: "test":0..5 }), index: Immediate { value: Int(4), span: "test":19..20 }, span: "test":0..21 }, index: Path(Path { path: [Ident { name: "M", span: "test":16..17 }], is_absolute: false, span: "test":16..17 }), span: "test":0..18 }, index: Call { name: Path { path: [Ident { name: "foo", span: "test":9..12 }], is_absolute: false, span: "test":9..12 }, args: [], span: "test":9..14 }, span: "test":0..15 }, index: Path(Path { path: [Ident { name: "N", span: "test":6..7 }], is_absolute: false, span: "test":6..7 }), span: "test":0..8 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"foo()[{ M }][if true { 1 } else { 3 }]"#),
        expect_test::expect![[
            r#"ArrayElementAccess { array: ArrayElementAccess { array: Call { name: Path { path: [Ident { name: "foo", span: "test":0..3 }], is_absolute: false, span: "test":0..3 }, args: [], span: "test":0..5 }, index: If { condition: Immediate { value: Bool(true), span: "test":16..20 }, then_block: Block { statements: [], final_expr: Immediate { value: Int(1), span: "test":23..24 }, span: "test":21..26 }, else_block: Block { statements: [], final_expr: Immediate { value: Int(3), span: "test":34..35 }, span: "test":32..37 }, span: "test":13..37 }, span: "test":0..38 }, index: Block(Block { statements: [], final_expr: Path(Path { path: [Ident { name: "M", span: "test":8..9 }], is_absolute: false, span: "test":8..9 }), span: "test":6..11 }), span: "test":0..12 }"#
        ]],
    );

    check(
        &run_parser!(let_decl(expr()), r#"let x = a[];"#),
        expect_test::expect![[r#"
            expected `::`, `::`, `!`, `+`, `-`, `{`, `{`, `(`, `[`, `if`, or `cond`, found `]`
            @10..11: expected `::`, `::`, `!`, `+`, `-`, `{`, `{`, `(`, `[`, `if`, or `cond`
        "#]],
    );

    check(
        &run_parser!(expr(), r#"a[MyEnum::Variant1]"#),
        expect_test::expect![[
            r#"ArrayElementAccess { array: Path(Path { path: [Ident { name: "a", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), index: Path(Path { path: [Ident { name: "MyEnum", span: "test":2..8 }, Ident { name: "Variant1", span: "test":10..18 }], is_absolute: false, span: "test":2..18 }), span: "test":0..19 }"#
        ]],
    );
}

#[test]
fn tuple_expressions() {
    check(
        &run_parser!(expr(), r#"{0}"#), // This is not a tuple. It is a code block expr.
        expect_test::expect![[
            r#"Block(Block { statements: [], final_expr: Immediate { value: Int(0), span: "test":1..2 }, span: "test":0..3 })"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"{x: 0}"#), // This is a tuple because the field is named so there is no ambiguity
        expect_test::expect![[
            r#"Tuple { fields: [(Some(Ident { name: "x", span: "test":1..2 }), Immediate { value: Int(0), span: "test":4..5 })], span: "test":0..6 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"{0,}"#), // This is a tuple
        expect_test::expect![[
            r#"Tuple { fields: [(None, Immediate { value: Int(0), span: "test":1..2 })], span: "test":0..4 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"{x: 0,}"#), // This is a tuple
        expect_test::expect![[
            r#"Tuple { fields: [(Some(Ident { name: "x", span: "test":1..2 }), Immediate { value: Int(0), span: "test":4..5 })], span: "test":0..7 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"{0, 1.0, "foo"}"#),
        expect_test::expect![[
            r#"Tuple { fields: [(None, Immediate { value: Int(0), span: "test":1..2 }), (None, Immediate { value: Real(1.0), span: "test":4..7 }), (None, Immediate { value: String("foo"), span: "test":9..14 })], span: "test":0..15 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"{x: 0, y: 1.0, z: "foo"}"#),
        expect_test::expect![[
            r#"Tuple { fields: [(Some(Ident { name: "x", span: "test":1..2 }), Immediate { value: Int(0), span: "test":4..5 }), (Some(Ident { name: "y", span: "test":7..8 }), Immediate { value: Real(1.0), span: "test":10..13 }), (Some(Ident { name: "z", span: "test":15..16 }), Immediate { value: String("foo"), span: "test":18..23 })], span: "test":0..24 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"{0, {1.0, "bar"}, "foo"}"#),
        expect_test::expect![[
            r#"Tuple { fields: [(None, Immediate { value: Int(0), span: "test":1..2 }), (None, Tuple { fields: [(None, Immediate { value: Real(1.0), span: "test":5..8 }), (None, Immediate { value: String("bar"), span: "test":10..15 })], span: "test":4..16 }), (None, Immediate { value: String("foo"), span: "test":18..23 })], span: "test":0..24 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"{x: 0, {y: 1.0, "bar"}, z: "foo"}"#),
        expect_test::expect![[
            r#"Tuple { fields: [(Some(Ident { name: "x", span: "test":1..2 }), Immediate { value: Int(0), span: "test":4..5 }), (None, Tuple { fields: [(Some(Ident { name: "y", span: "test":8..9 }), Immediate { value: Real(1.0), span: "test":11..14 }), (None, Immediate { value: String("bar"), span: "test":16..21 })], span: "test":7..22 }), (Some(Ident { name: "z", span: "test":24..25 }), Immediate { value: String("foo"), span: "test":27..32 })], span: "test":0..33 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"{ { 42 }, if c { 2 } else { 3 }, foo() }"#),
        expect_test::expect![[
            r#"Tuple { fields: [(None, Block(Block { statements: [], final_expr: Immediate { value: Int(42), span: "test":4..6 }, span: "test":2..8 })), (None, If { condition: Path(Path { path: [Ident { name: "c", span: "test":13..14 }], is_absolute: false, span: "test":13..14 }), then_block: Block { statements: [], final_expr: Immediate { value: Int(2), span: "test":17..18 }, span: "test":15..20 }, else_block: Block { statements: [], final_expr: Immediate { value: Int(3), span: "test":28..29 }, span: "test":26..31 }, span: "test":10..31 }), (None, Call { name: Path { path: [Ident { name: "foo", span: "test":33..36 }], is_absolute: false, span: "test":33..36 }, args: [], span: "test":33..38 })], span: "test":0..40 }"#
        ]],
    );

    check(
        &run_parser!(
            expr(),
            r#"{ x: { 42 }, y: if c { 2 } else { 3 }, z: foo() }"#
        ),
        expect_test::expect![[
            r#"Tuple { fields: [(Some(Ident { name: "x", span: "test":2..3 }), Block(Block { statements: [], final_expr: Immediate { value: Int(42), span: "test":7..9 }, span: "test":5..11 })), (Some(Ident { name: "y", span: "test":13..14 }), If { condition: Path(Path { path: [Ident { name: "c", span: "test":19..20 }], is_absolute: false, span: "test":19..20 }), then_block: Block { statements: [], final_expr: Immediate { value: Int(2), span: "test":23..24 }, span: "test":21..26 }, else_block: Block { statements: [], final_expr: Immediate { value: Int(3), span: "test":34..35 }, span: "test":32..37 }, span: "test":16..37 }), (Some(Ident { name: "z", span: "test":39..40 }), Call { name: Path { path: [Ident { name: "foo", span: "test":42..45 }], is_absolute: false, span: "test":42..45 }, args: [], span: "test":42..47 })], span: "test":0..49 }"#
        ]],
    );
}

#[test]
fn tuple_field_accesses() {
    check(
        &run_parser!(expr(), r#"t.0 + t.9999999 + t.x"#),
        expect_test::expect![[
            r#"BinaryOp { op: Add, lhs: BinaryOp { op: Add, lhs: TupleFieldAccess { tuple: Path(Path { path: [Ident { name: "t", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), field: Left(0), span: "test":0..3 }, rhs: TupleFieldAccess { tuple: Path(Path { path: [Ident { name: "t", span: "test":6..7 }], is_absolute: false, span: "test":6..7 }), field: Left(9999999), span: "test":6..15 }, span: "test":0..15 }, rhs: TupleFieldAccess { tuple: Path(Path { path: [Ident { name: "t", span: "test":18..19 }], is_absolute: false, span: "test":18..19 }), field: Right(Ident { name: "x", span: "test":20..21 }), span: "test":18..21 }, span: "test":0..21 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"{0, 1}.0"#),
        expect_test::expect![[
            r#"TupleFieldAccess { tuple: Tuple { fields: [(None, Immediate { value: Int(0), span: "test":1..2 }), (None, Immediate { value: Int(1), span: "test":4..5 })], span: "test":0..6 }, field: Left(0), span: "test":0..8 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"{0, 1}.x"#),
        expect_test::expect![[
            r#"TupleFieldAccess { tuple: Tuple { fields: [(None, Immediate { value: Int(0), span: "test":1..2 }), (None, Immediate { value: Int(1), span: "test":4..5 })], span: "test":0..6 }, field: Right(Ident { name: "x", span: "test":7..8 }), span: "test":0..8 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"t.0 .0"#),
        expect_test::expect![[
            r#"TupleFieldAccess { tuple: TupleFieldAccess { tuple: Path(Path { path: [Ident { name: "t", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), field: Left(0), span: "test":0..3 }, field: Left(0), span: "test":0..6 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"t.x .y"#),
        expect_test::expect![[
            r#"TupleFieldAccess { tuple: TupleFieldAccess { tuple: Path(Path { path: [Ident { name: "t", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), field: Right(Ident { name: "x", span: "test":2..3 }), span: "test":0..3 }, field: Right(Ident { name: "y", span: "test":5..6 }), span: "test":0..6 }"#
        ]],
    );

    check(
        &run_parser!(expr(), "t \r .1 .2.2. \n 3 . \t 13 . 1.1"),
        expect_test::expect![[
            r#"TupleFieldAccess { tuple: TupleFieldAccess { tuple: TupleFieldAccess { tuple: TupleFieldAccess { tuple: TupleFieldAccess { tuple: TupleFieldAccess { tuple: TupleFieldAccess { tuple: Path(Path { path: [Ident { name: "t", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), field: Left(1), span: "test":0..6 }, field: Left(2), span: "test":0..11 }, field: Left(2), span: "test":0..11 }, field: Left(3), span: "test":0..16 }, field: Left(13), span: "test":0..23 }, field: Left(1), span: "test":0..29 }, field: Left(1), span: "test":0..29 }"#
        ]],
    );

    check(
        &run_parser!(expr(), "t \r .x .1.2. \n w . \t t. 3.4"),
        expect_test::expect![[
            r#"TupleFieldAccess { tuple: TupleFieldAccess { tuple: TupleFieldAccess { tuple: TupleFieldAccess { tuple: TupleFieldAccess { tuple: TupleFieldAccess { tuple: TupleFieldAccess { tuple: Path(Path { path: [Ident { name: "t", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), field: Right(Ident { name: "x", span: "test":5..6 }), span: "test":0..6 }, field: Left(1), span: "test":0..11 }, field: Left(2), span: "test":0..11 }, field: Right(Ident { name: "w", span: "test":15..16 }), span: "test":0..16 }, field: Right(Ident { name: "t", span: "test":21..22 }), span: "test":0..22 }, field: Left(3), span: "test":0..27 }, field: Left(4), span: "test":0..27 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"foo().0.1"#),
        expect_test::expect![[
            r#"TupleFieldAccess { tuple: TupleFieldAccess { tuple: Call { name: Path { path: [Ident { name: "foo", span: "test":0..3 }], is_absolute: false, span: "test":0..3 }, args: [], span: "test":0..5 }, field: Left(0), span: "test":0..9 }, field: Left(1), span: "test":0..9 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"foo().a.b.0.1"#),
        expect_test::expect![[
            r#"TupleFieldAccess { tuple: TupleFieldAccess { tuple: TupleFieldAccess { tuple: TupleFieldAccess { tuple: Call { name: Path { path: [Ident { name: "foo", span: "test":0..3 }], is_absolute: false, span: "test":0..3 }, args: [], span: "test":0..5 }, field: Right(Ident { name: "a", span: "test":6..7 }), span: "test":0..7 }, field: Right(Ident { name: "b", span: "test":8..9 }), span: "test":0..9 }, field: Left(0), span: "test":0..13 }, field: Left(1), span: "test":0..13 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"{ {0, 0} }.0"#),
        expect_test::expect![[
            r#"TupleFieldAccess { tuple: Block(Block { statements: [], final_expr: Tuple { fields: [(None, Immediate { value: Int(0), span: "test":3..4 }), (None, Immediate { value: Int(0), span: "test":6..7 })], span: "test":2..8 }, span: "test":0..10 }), field: Left(0), span: "test":0..12 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"{ {0, 0} }.a"#),
        expect_test::expect![[
            r#"TupleFieldAccess { tuple: Block(Block { statements: [], final_expr: Tuple { fields: [(None, Immediate { value: Int(0), span: "test":3..4 }), (None, Immediate { value: Int(0), span: "test":6..7 })], span: "test":2..8 }, span: "test":0..10 }), field: Right(Ident { name: "a", span: "test":11..12 }), span: "test":0..12 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"if true { {0, 0} } else { {0, 0} }.0"#),
        expect_test::expect![[
            r#"TupleFieldAccess { tuple: If { condition: Immediate { value: Bool(true), span: "test":3..7 }, then_block: Block { statements: [], final_expr: Tuple { fields: [(None, Immediate { value: Int(0), span: "test":11..12 }), (None, Immediate { value: Int(0), span: "test":14..15 })], span: "test":10..16 }, span: "test":8..18 }, else_block: Block { statements: [], final_expr: Tuple { fields: [(None, Immediate { value: Int(0), span: "test":27..28 }), (None, Immediate { value: Int(0), span: "test":30..31 })], span: "test":26..32 }, span: "test":24..34 }, span: "test":0..34 }, field: Left(0), span: "test":0..36 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"if true { {0, 0} } else { {0, 0} }.x"#),
        expect_test::expect![[
            r#"TupleFieldAccess { tuple: If { condition: Immediate { value: Bool(true), span: "test":3..7 }, then_block: Block { statements: [], final_expr: Tuple { fields: [(None, Immediate { value: Int(0), span: "test":11..12 }), (None, Immediate { value: Int(0), span: "test":14..15 })], span: "test":10..16 }, span: "test":8..18 }, else_block: Block { statements: [], final_expr: Tuple { fields: [(None, Immediate { value: Int(0), span: "test":27..28 }), (None, Immediate { value: Int(0), span: "test":30..31 })], span: "test":26..32 }, span: "test":24..34 }, span: "test":0..34 }, field: Right(Ident { name: "x", span: "test":35..36 }), span: "test":0..36 }"#
        ]],
    );

    // This parses because `1 + 2` is an expression, but it should fail in semantic analysis.
    check(
        &run_parser!(expr(), "1 + 2 .3"),
        expect_test::expect![[
            r#"BinaryOp { op: Add, lhs: Immediate { value: Int(1), span: "test":0..1 }, rhs: TupleFieldAccess { tuple: Immediate { value: Int(2), span: "test":4..5 }, field: Left(3), span: "test":4..8 }, span: "test":0..8 }"#
        ]],
    );

    // This parses because `1 + 2` is an expression, but it should fail in semantic analysis.
    check(
        &run_parser!(expr(), "1 + 2 .a"),
        expect_test::expect![[
            r#"BinaryOp { op: Add, lhs: Immediate { value: Int(1), span: "test":0..1 }, rhs: TupleFieldAccess { tuple: Immediate { value: Int(2), span: "test":4..5 }, field: Right(Ident { name: "a", span: "test":7..8 }), span: "test":4..8 }, span: "test":0..8 }"#
        ]],
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
        expect_test::expect![[
            r#"Cond { branches: [], else_result: Path(Path { path: [Ident { name: "a", span: "test":15..16 }], is_absolute: false, span: "test":15..16 }), span: "test":0..18 }"#
        ]],
    );

    check(
        &run_parser!(cond_expr(expr()), r#"cond { else => { a } }"#),
        expect_test::expect![[
            r#"Cond { branches: [], else_result: Block(Block { statements: [], final_expr: Path(Path { path: [Ident { name: "a", span: "test":17..18 }], is_absolute: false, span: "test":17..18 }), span: "test":15..20 }), span: "test":0..22 }"#
        ]],
    );

    check(
        &run_parser!(cond_expr(expr()), r#"cond { a => b, else => c }"#),
        expect_test::expect![[
            r#"Cond { branches: [CondBranch { condition: Path(Path { path: [Ident { name: "a", span: "test":7..8 }], is_absolute: false, span: "test":7..8 }), result: Path(Path { path: [Ident { name: "b", span: "test":12..13 }], is_absolute: false, span: "test":12..13 }), span: "test":7..14 }], else_result: Path(Path { path: [Ident { name: "c", span: "test":23..24 }], is_absolute: false, span: "test":23..24 }), span: "test":0..26 }"#
        ]],
    );

    check(
        &run_parser!(cond_expr(expr()), r#"cond { a => { b }, else => c, }"#),
        expect_test::expect![[
            r#"Cond { branches: [CondBranch { condition: Path(Path { path: [Ident { name: "a", span: "test":7..8 }], is_absolute: false, span: "test":7..8 }), result: Block(Block { statements: [], final_expr: Path(Path { path: [Ident { name: "b", span: "test":14..15 }], is_absolute: false, span: "test":14..15 }), span: "test":12..17 }), span: "test":7..18 }], else_result: Path(Path { path: [Ident { name: "c", span: "test":27..28 }], is_absolute: false, span: "test":27..28 }), span: "test":0..31 }"#
        ]],
    );

    check(
        &run_parser!(
            cond_expr(expr()),
            r#"cond { a => b, { true } => d, else => f, }"#
        ),
        expect_test::expect![[
            r#"Cond { branches: [CondBranch { condition: Path(Path { path: [Ident { name: "a", span: "test":7..8 }], is_absolute: false, span: "test":7..8 }), result: Path(Path { path: [Ident { name: "b", span: "test":12..13 }], is_absolute: false, span: "test":12..13 }), span: "test":7..14 }, CondBranch { condition: Block(Block { statements: [], final_expr: Immediate { value: Bool(true), span: "test":17..21 }, span: "test":15..23 }), result: Path(Path { path: [Ident { name: "d", span: "test":27..28 }], is_absolute: false, span: "test":27..28 }), span: "test":15..29 }], else_result: Path(Path { path: [Ident { name: "f", span: "test":38..39 }], is_absolute: false, span: "test":38..39 }), span: "test":0..42 }"#
        ]],
    );

    check(
        &run_parser!(cond_expr(expr()), r#"cond { a => b, }"#),
        expect_test::expect![[r#"
            expected `::`, `::`, `!`, `+`, `-`, `{`, `{`, `(`, `[`, `if`, `else`, or `cond`, found `}`
            @15..16: expected `::`, `::`, `!`, `+`, `-`, `{`, `{`, `(`, `[`, `if`, `else`, or `cond`
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
        expect_test::expect![[
            r#"Cast { value: Cast { value: Cast { value: Immediate { value: Int(5), span: "test":1..2 }, ty: Primitive { kind: Int, span: "test":6..9 }, span: "test":1..9 }, ty: Primitive { kind: Real, span: "test":14..18 }, span: "test":1..18 }, ty: Primitive { kind: Int, span: "test":22..25 }, span: "test":1..25 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"t.0.1 as real * a[5][3] as int"#),
        expect_test::expect![[
            r#"BinaryOp { op: Mul, lhs: Cast { value: TupleFieldAccess { tuple: TupleFieldAccess { tuple: Path(Path { path: [Ident { name: "t", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), field: Left(0), span: "test":0..5 }, field: Left(1), span: "test":0..5 }, ty: Primitive { kind: Real, span: "test":9..13 }, span: "test":0..13 }, rhs: Cast { value: ArrayElementAccess { array: ArrayElementAccess { array: Path(Path { path: [Ident { name: "a", span: "test":16..17 }], is_absolute: false, span: "test":16..17 }), index: Immediate { value: Int(3), span: "test":21..22 }, span: "test":16..23 }, index: Immediate { value: Int(5), span: "test":18..19 }, span: "test":16..20 }, ty: Primitive { kind: Int, span: "test":27..30 }, span: "test":16..30 }, span: "test":0..30 }"#
        ]],
    );

    check(
        &run_parser!(
            let_decl(expr()),
            r#"let x = foo() as real as { int, real };"#
        ),
        expect_test::expect![[
            r#"Let { name: Ident { name: "x", span: "test":4..5 }, ty: None, init: Some(Cast { value: Cast { value: Call { name: Path { path: [Ident { name: "foo", span: "test":8..11 }], is_absolute: false, span: "test":8..11 }, args: [], span: "test":8..13 }, ty: Primitive { kind: Real, span: "test":17..21 }, span: "test":8..21 }, ty: Tuple { fields: [(None, Primitive { kind: Int, span: "test":27..30 }), (None, Primitive { kind: Real, span: "test":32..36 })], span: "test":25..38 }, span: "test":8..38 }), span: "test":0..39 }"#
        ]],
    );

    check(
        &run_parser!(let_decl(expr()), r#"let x = 5 as;"#),
        expect_test::expect![[r#"
            expected `::`, `{`, `real`, `int`, `bool`, or `string`, found `;`
            @12..13: expected `::`, `{`, `real`, `int`, `bool`, or `string`
        "#]],
    );
}

#[test]
fn in_expr() {
    check(
        &run_parser!(expr(), r#"x in { 1, 2 }"#),
        expect_test::expect![[
            r#"In { value: Path(Path { path: [Ident { name: "x", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), collection: Tuple { fields: [(None, Immediate { value: Int(1), span: "test":7..8 }), (None, Immediate { value: Int(2), span: "test":10..11 })], span: "test":5..13 }, span: "test":0..13 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"x in [ 1, 2 ] in { true, false }"#),
        expect_test::expect![[
            r#"In { value: Path(Path { path: [Ident { name: "x", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), collection: In { value: Array { elements: [Immediate { value: Int(1), span: "test":7..8 }, Immediate { value: Int(2), span: "test":10..11 }], span: "test":5..13 }, collection: Tuple { fields: [(None, Immediate { value: Bool(true), span: "test":19..23 }), (None, Immediate { value: Bool(false), span: "test":25..30 })], span: "test":17..32 }, span: "test":5..32 }, span: "test":0..32 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"x as int in { 1, 2 }"#),
        expect_test::expect![[
            r#"In { value: Cast { value: Path(Path { path: [Ident { name: "x", span: "test":0..1 }], is_absolute: false, span: "test":0..1 }), ty: Primitive { kind: Int, span: "test":5..8 }, span: "test":0..8 }, collection: Tuple { fields: [(None, Immediate { value: Int(1), span: "test":14..15 }), (None, Immediate { value: Int(2), span: "test":17..18 })], span: "test":12..20 }, span: "test":0..20 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"[1] in foo() in [[1]]"#),
        expect_test::expect![[
            r#"In { value: Array { elements: [Immediate { value: Int(1), span: "test":1..2 }], span: "test":0..3 }, collection: In { value: Call { name: Path { path: [Ident { name: "foo", span: "test":7..10 }], is_absolute: false, span: "test":7..10 }, args: [], span: "test":7..12 }, collection: Array { elements: [Array { elements: [Immediate { value: Int(1), span: "test":18..19 }], span: "test":17..20 }], span: "test":16..21 }, span: "test":7..21 }, span: "test":0..21 }"#
        ]],
    );

    check(
        &run_parser!(let_decl(expr()), r#"let x = 5 in;"#),
        expect_test::expect![[r#"
            expected `::`, `::`, `!`, `+`, `-`, `{`, `{`, `(`, `[`, `if`, or `cond`, found `;`
            @12..13: expected `::`, `::`, `!`, `+`, `-`, `{`, `{`, `(`, `[`, `if`, or `cond`
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
        expect_test::expect![[
            r#"[Let { name: Ident { name: "low_val", span: "test":5..12 }, ty: Some(Primitive { kind: Real, span: "test":14..18 }), init: Some(Immediate { value: Real(1.23), span: "test":21..25 }), span: "test":1..26 }, Let { name: Ident { name: "high_val", span: "test":31..39 }, ty: None, init: Some(Immediate { value: Real(4.56), span: "test":42..46 }), span: "test":27..47 }, Constraint { expr: BinaryOp { op: GreaterThan, lhs: Path(Path { path: [Ident { name: "mid", span: "test":112..115 }], is_absolute: false, span: "test":112..115 }), rhs: BinaryOp { op: Mul, lhs: Path(Path { path: [Ident { name: "low_val", span: "test":118..125 }], is_absolute: false, span: "test":118..125 }), rhs: Immediate { value: Real(2.0), span: "test":128..131 }, span: "test":118..131 }, span: "test":112..131 }, span: "test":101..132 }, Constraint { expr: BinaryOp { op: LessThan, lhs: Path(Path { path: [Ident { name: "mid", span: "test":144..147 }], is_absolute: false, span: "test":144..147 }), rhs: Path(Path { path: [Ident { name: "high_val", span: "test":150..158 }], is_absolute: false, span: "test":150..158 }), span: "test":144..158 }, span: "test":133..159 }, Solve { directive: Minimize(Path(Path { path: [Ident { name: "mid", span: "test":176..179 }], is_absolute: false, span: "test":176..179 })), span: "test":161..180 }]"#
        ]],
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
            expected `::`, `::`, `!`, `+`, `-`, `{`, `{`, `(`, `[`, `if`, `cond`, `let`, `state`, or `constraint`, found `}`
            @18..19: expected `::`, `::`, `!`, `+`, `-`, `{`, `{`, `(`, `[`, `if`, `cond`, `let`, `state`, or `constraint`
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
        expect_test::expect![[
            r#"[Solve { directive: Maximize(Path(Path { path: [Ident { name: "low", span: "test":16..19 }], is_absolute: false, span: "test":16..19 })), span: "test":1..20 }, Constraint { expr: BinaryOp { op: LessThan, lhs: Path(Path { path: [Ident { name: "low", span: "test":32..35 }], is_absolute: false, span: "test":32..35 }), rhs: Path(Path { path: [Ident { name: "high", span: "test":38..42 }], is_absolute: false, span: "test":38..42 }), span: "test":32..42 }, span: "test":21..43 }, Let { name: Ident { name: "high", span: "test":48..52 }, ty: None, init: Some(Immediate { value: Real(2.0), span: "test":55..58 }), span: "test":44..59 }, Solve { directive: Satisfy, span: "test":60..74 }, Let { name: Ident { name: "low", span: "test":79..82 }, ty: None, init: Some(Immediate { value: Real(1.0), span: "test":85..88 }), span: "test":75..89 }]"#
        ]],
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
        &format!("{:?}", parse_str_to_ast("let x = 5;", filepath.clone())),
        expect_test::expect![[
            r#"Ok([Let { name: Ident { name: "x", span: "test":4..5 }, ty: None, init: Some(Immediate { value: Int(5), span: "test":8..9 }), span: "test":0..10 }])"#
        ]],
    );
    check(
        &format!("{:?}", parse_str_to_ast("let x = 5", filepath.clone())),
        expect_test::expect![[
            r#"Err([Parse { error: ExpectedFound { span: "test":9..9, expected: [Some(Semi), Some(DoublePipe), Some(DoubleAmpersand), Some(NotEq), Some(EqEq), Some(GtEq), Some(LtEq), Some(Gt), Some(Lt), Some(Plus), Some(Minus), Some(Star), Some(Div), Some(Mod), Some(In), Some(As), Some(Dot), Some(BracketOpen), Some(SingleQuote)], found: None } }])"#
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
        expect_test::expect![[
            r#"Let { name: Ident { name: "blah", span: "test":4..8 }, ty: None, init: Some(Immediate { value: BigInt(1234567890123456789012345678901234567890), span: "test":11..51 }), span: "test":0..52 }"#
        ]],
    );
    check(
        &run_parser!(
            let_decl(expr()),
            "let blah = 0xfeedbadf00d2adeadcafed00dbabeface;"
        ),
        // Confirmed by using the Python REPL to convert from hex to dec...
        expect_test::expect![[
            r#"Let { name: Ident { name: "blah", span: "test":4..8 }, ty: None, init: Some(Immediate { value: BigInt(5421732407698601623698172315373246806734), span: "test":11..46 }), span: "test":0..47 }"#
        ]],
    );
    check(
        &run_parser!(
            expr(),
            "0b110100101001010101010101010101010011010011010101010101010101010101010101010 + \
            0b01001001010110101010101001010101010010100100101001010010100100100001010"
        ),
        // Again confirmed using the Python REPL.  Handy.
        expect_test::expect![[
            r#"BinaryOp { op: Add, lhs: Immediate { value: BigInt(31076614848392666458794), span: "test":0..77 }, rhs: Immediate { value: BigInt(676572722683907229962), span: "test":80..153 }, span: "test":0..153 }"#
        ]],
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
        expect_test::expect![[
            r#"Interface(InterfaceDecl { name: Ident { name: "Foo", span: "test":11..14 }, functions: [FnSig { name: Ident { name: "foo", span: "test":24..27 }, params: [(Ident { name: "x", span: "test":28..29 }, Primitive { kind: Real, span: "test":31..35 }), (Ident { name: "y", span: "test":37..38 }, Array { ty: Primitive { kind: Int, span: "test":40..43 }, range: Immediate { value: Int(5), span: "test":44..45 }, span: "test":40..46 })], return_type: Primitive { kind: Real, span: "test":51..55 }, span: "test":21..55 }, FnSig { name: Ident { name: "bar", span: "test":64..67 }, params: [(Ident { name: "x", span: "test":68..69 }, Primitive { kind: Bool, span: "test":71..75 })], return_type: Primitive { kind: Real, span: "test":81..85 }, span: "test":61..85 }, FnSig { name: Ident { name: "baz", span: "test":94..97 }, params: [], return_type: Tuple { fields: [(None, Primitive { kind: Int, span: "test":105..108 }), (None, Primitive { kind: Real, span: "test":110..114 })], span: "test":103..116 }, span: "test":91..116 }], span: "test":1..119 })"#
        ]],
    );

    check(
        &run_parser!(interface_decl(), "interface Foo {}"),
        expect_test::expect![[
            r#"Interface(InterfaceDecl { name: Ident { name: "Foo", span: "test":10..13 }, functions: [], span: "test":0..16 })"#
        ]],
    );
}

#[test]
fn contract_test() {
    check(
        &run_parser!(contract_decl(), "contract Foo(0) {}"),
        expect_test::expect![[
            r#"Contract(ContractDecl { name: Ident { name: "Foo", span: "test":9..12 }, id: Immediate { value: Int(0), span: "test":13..14 }, interfaces: [], functions: [], span: "test":0..18 })"#
        ]],
    );

    check(
        &run_parser!(contract_decl(), "contract Foo(if true {0} else {1}) {}"),
        expect_test::expect![[
            r#"Contract(ContractDecl { name: Ident { name: "Foo", span: "test":9..12 }, id: If { condition: Immediate { value: Bool(true), span: "test":16..20 }, then_block: Block { statements: [], final_expr: Immediate { value: Int(0), span: "test":22..23 }, span: "test":21..24 }, else_block: Block { statements: [], final_expr: Immediate { value: Int(1), span: "test":31..32 }, span: "test":30..33 }, span: "test":13..33 }, interfaces: [], functions: [], span: "test":0..37 })"#
        ]],
    );

    check(
        &run_parser!(
            contract_decl(),
            "contract Foo(0) implements X::Bar, ::Y::Baz {}"
        ),
        expect_test::expect![[
            r#"Contract(ContractDecl { name: Ident { name: "Foo", span: "test":9..12 }, id: Immediate { value: Int(0), span: "test":13..14 }, interfaces: [Path { path: [Ident { name: "X", span: "test":27..28 }, Ident { name: "Bar", span: "test":30..33 }], is_absolute: false, span: "test":27..33 }, Path { path: [Ident { name: "Y", span: "test":37..38 }, Ident { name: "Baz", span: "test":40..43 }], is_absolute: true, span: "test":35..43 }], functions: [], span: "test":0..46 })"#
        ]],
    );

    check(
        &run_parser!(
            contract_decl(),
            "contract Foo(0) implements Bar { fn baz(x: real) -> int; }"
        ),
        expect_test::expect![[
            r#"Contract(ContractDecl { name: Ident { name: "Foo", span: "test":9..12 }, id: Immediate { value: Int(0), span: "test":13..14 }, interfaces: [Path { path: [Ident { name: "Bar", span: "test":27..30 }], is_absolute: false, span: "test":27..30 }], functions: [FnSig { name: Ident { name: "baz", span: "test":36..39 }, params: [(Ident { name: "x", span: "test":40..41 }, Primitive { kind: Real, span: "test":43..47 })], return_type: Primitive { kind: Int, span: "test":52..55 }, span: "test":33..55 }], span: "test":0..58 })"#
        ]],
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
        expect_test::expect![[r#"Extern { functions: [], span: "test":0..9 }"#]],
    );
    check(
        &run_parser!(extern_decl(), "extern { fn foo() -> string; }"),
        expect_test::expect![[
            r#"Extern { functions: [FnSig { name: Ident { name: "foo", span: "test":12..15 }, params: [], return_type: Primitive { kind: String, span: "test":21..27 }, span: "test":9..27 }], span: "test":0..30 }"#
        ]],
    );
    check(
        &run_parser!(extern_decl(), "extern { fn foo(x: int, y: real) -> int; }"),
        expect_test::expect![[
            r#"Extern { functions: [FnSig { name: Ident { name: "foo", span: "test":12..15 }, params: [(Ident { name: "x", span: "test":16..17 }, Primitive { kind: Int, span: "test":19..22 }), (Ident { name: "y", span: "test":24..25 }, Primitive { kind: Real, span: "test":27..31 })], return_type: Primitive { kind: Int, span: "test":36..39 }, span: "test":9..39 }], span: "test":0..42 }"#
        ]],
    );
    check(
        &run_parser!(
            extern_decl(),
            "extern { fn foo() -> int; fn bar() -> real; }"
        ),
        expect_test::expect![[
            r#"Extern { functions: [FnSig { name: Ident { name: "foo", span: "test":12..15 }, params: [], return_type: Primitive { kind: Int, span: "test":21..24 }, span: "test":9..24 }, FnSig { name: Ident { name: "bar", span: "test":29..32 }, params: [], return_type: Primitive { kind: Real, span: "test":38..42 }, span: "test":26..42 }], span: "test":0..45 }"#
        ]],
    );
    check(
        &run_parser!(extern_decl(), "extern { fn foo(); }"),
        expect_test::expect![[r#"
            expected `->`, found `;`
            @17..18: expected `->`
        "#]],
    );
}
