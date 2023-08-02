use crate::{
    error::CompileError,
    lexer::{self, KEYWORDS},
    parser::*,
};
use chumsky::Stream;

#[cfg(test)]
macro_rules! run_parser {
    ($parser: expr, $source: expr) => {{
        let (toks, errs) = lexer::lex($source);
        assert!(errs.is_empty());
        let token_stream = Stream::from_iter($source.len()..$source.len(), toks.into_iter());
        match $parser.parse(token_stream) {
            Ok(ast) => format!("{:?}", ast),
            Err(errors) => format!(
                "{}",
                // Print each error on one line. For each error, start with the span.
                errors.iter().fold(String::new(), |acc, error| {
                    let span = CompileError::Parse {
                        error: error.clone(),
                    }
                    .span();
                    format!("{}@{}..{}: {}\n", acc, span.start, span.end(), error)
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
    check(&run_parser!(type_(), "int"), expect_test::expect!["Int"]);
    check(&run_parser!(type_(), "real"), expect_test::expect!["Real"]);
    check(&run_parser!(type_(), "bool"), expect_test::expect!["Bool"]);
    check(
        &run_parser!(type_(), "string"),
        expect_test::expect!["String"],
    );
    check(
        &run_parser!(type_(), "{int, real, string}"),
        expect_test::expect!["Tuple([(None, Int), (None, Real), (None, String)])"],
    );
    check(
        &run_parser!(type_(), "{int, {real, int}, string}"),
        expect_test::expect![
            "Tuple([(None, Int), (None, Tuple([(None, Real), (None, Int)])), (None, String)])"
        ],
    );
}

#[test]
fn immediates() {
    check(
        &run_parser!(immediate(), "0x88;"),
        expect_test::expect![[r#"Int(136)"#]],
    );
    check(
        &run_parser!(immediate(), "0b111;"),
        expect_test::expect![[r#"Int(7)"#]],
    );
    check(
        &run_parser!(immediate(), "1;"),
        expect_test::expect![[r#"Int(1)"#]],
    );
    check(
        &run_parser!(immediate(), "0x4f3f4f3f4f3f4f3f4f3f4f3f4f;"),
        expect_test::expect![[r#"BigInt(6278618198356320102092284837711)"#]],
    );
    check(
        &run_parser!(immediate(), "0b1000000000000000000000000000000000000000000000000000000000000000000000000000000000000;"),
        expect_test::expect![[r#"BigInt(19342813113834066795298816)"#]],
    );
    check(
        &run_parser!(immediate(), "9223372036854775808;"),
        expect_test::expect![[r#"BigInt(9223372036854775808)"#]],
    );
    check(
        &run_parser!(immediate(), "1.3;"),
        expect_test::expect![[r#"Real(1.3)"#]],
    );
}

#[test]
fn use_statements() {
    check(
        &run_parser!(yurt_program(), "use *; use ::*;"),
        expect_test::expect!["[Use { is_absolute: false, use_tree: Glob }, Use { is_absolute: true, use_tree: Glob }]"],
    );

    check(
        &run_parser!(yurt_program(), "use {}; use ::{};"),
        expect_test::expect!["[Use { is_absolute: false, use_tree: Group { imports: [] } }, Use { is_absolute: true, use_tree: Group { imports: [] } }]"],
    );

    check(
        &run_parser!(yurt_program(), "use a; use ::a; use ::a as b;"),
        expect_test::expect![[
            r#"[Use { is_absolute: false, use_tree: Name { name: "a" } }, Use { is_absolute: true, use_tree: Name { name: "a" } }, Use { is_absolute: true, use_tree: Alias { name: "a", alias: "b" } }]"#
        ]],
    );

    check(
        &run_parser!(yurt_program(), "use a::b; use ::a::b; use ::a::b as c;"),
        expect_test::expect![[
            r#"[Use { is_absolute: false, use_tree: Path { prefix: "a", suffix: Name { name: "b" } } }, Use { is_absolute: true, use_tree: Path { prefix: "a", suffix: Name { name: "b" } } }, Use { is_absolute: true, use_tree: Path { prefix: "a", suffix: Alias { name: "b", alias: "c" } } }]"#
        ]],
    );

    check(
        &run_parser!(yurt_program(), "use a::{b, c as d};"),
        expect_test::expect![[
            r#"[Use { is_absolute: false, use_tree: Path { prefix: "a", suffix: Group { imports: [Name { name: "b" }, Alias { name: "c", alias: "d" }] } } }]"#
        ]],
    );

    check(
        &run_parser!(yurt_program(), "use ::a::{*, c as d};"),
        expect_test::expect![[
            r#"[Use { is_absolute: true, use_tree: Path { prefix: "a", suffix: Group { imports: [Glob, Alias { name: "c", alias: "d" }] } } }]"#
        ]],
    );

    check(
        &run_parser!(yurt_program(), "use ::a::{*, c as d};"),
        expect_test::expect![[
            r#"[Use { is_absolute: true, use_tree: Path { prefix: "a", suffix: Group { imports: [Glob, Alias { name: "c", alias: "d" }] } } }]"#
        ]],
    );

    check(
        &run_parser!(yurt_program(), "use a::{{*}, {c as d, { e as f, * }}};"),
        expect_test::expect![[
            r#"[Use { is_absolute: false, use_tree: Path { prefix: "a", suffix: Group { imports: [Group { imports: [Glob] }, Group { imports: [Alias { name: "c", alias: "d" }, Group { imports: [Alias { name: "e", alias: "f" }, Glob] }] }] } } }]"#
        ]],
    );

    // Errors - TODO: imporve these
    check(
        &run_parser!(use_statement(), "use ;"),
        expect_test::expect![[r#"
            @4..5: found ";" but expected "::", "*",  or "{"
        "#]],
    );

    check(
        &run_parser!(use_statement(), "use ::;"),
        expect_test::expect![[r#"
            @6..7: found ";" but expected "*",  or "{"
        "#]],
    );

    check(
        &run_parser!(use_statement(), "use a::;"),
        expect_test::expect![[r#"
            @5..7: found "::" but expected ";"
        "#]],
    );

    check(
        &run_parser!(use_statement(), "use * as b;"),
        expect_test::expect![[r#"
            @6..8: found "as" but expected ";"
        "#]],
    );

    check(
        &run_parser!(use_statement(), "use a::{* as d};"),
        expect_test::expect![[r#"
            @5..7: found "::" but expected ";"
        "#]],
    );
}

#[test]
fn let_decls() {
    check(
        &run_parser!(let_decl(expr()), "let blah;"),
        expect_test::expect![[r#"
            @0..9: type annotation or initializer needed for variable "blah"
        "#]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah = 1.0;"),
        expect_test::expect![[
            r#"Let(LetDecl { name: "blah", ty: None, init: Some(Immediate(Real(1.0))) })"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: real = 1.0;"),
        expect_test::expect![[
            r#"Let(LetDecl { name: "blah", ty: Some(Real), init: Some(Immediate(Real(1.0))) })"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: real;"),
        expect_test::expect![[r#"Let(LetDecl { name: "blah", ty: Some(Real), init: None })"#]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah = 1;"),
        expect_test::expect![[
            r#"Let(LetDecl { name: "blah", ty: None, init: Some(Immediate(Int(1))) })"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: int = 1;"),
        expect_test::expect![[
            r#"Let(LetDecl { name: "blah", ty: Some(Int), init: Some(Immediate(Int(1))) })"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: int;"),
        expect_test::expect![[r#"Let(LetDecl { name: "blah", ty: Some(Int), init: None })"#]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah = true;"),
        expect_test::expect![[
            r#"Let(LetDecl { name: "blah", ty: None, init: Some(Immediate(Bool(true))) })"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: bool = false;"),
        expect_test::expect![[
            r#"Let(LetDecl { name: "blah", ty: Some(Bool), init: Some(Immediate(Bool(false))) })"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: bool;"),
        expect_test::expect![[r#"Let(LetDecl { name: "blah", ty: Some(Bool), init: None })"#]],
    );
    check(
        &run_parser!(let_decl(expr()), r#"let blah = "hello";"#),
        expect_test::expect![[
            r#"Let(LetDecl { name: "blah", ty: None, init: Some(Immediate(String("hello"))) })"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), r#"let blah: string = "hello";"#),
        expect_test::expect![[
            r#"Let(LetDecl { name: "blah", ty: Some(String), init: Some(Immediate(String("hello"))) })"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), r#"let blah: string;"#),
        expect_test::expect![[r#"Let(LetDecl { name: "blah", ty: Some(String), init: None })"#]],
    );
}

#[test]
fn constraint_decls() {
    // Argument just needs to be any expression, as far as the parser is concerned.
    check(
        &run_parser!(constraint_decl(expr()), "constraint blah;"),
        expect_test::expect![[
            r#"Constraint(Ident(Ident { path: ["blah"], is_absolute: false }))"#
        ]],
    );
}

#[test]
fn solve_decls() {
    check(
        &run_parser!(solve_decl(), "solve satisfy;"),
        expect_test::expect!["Solve(Satisfy)"],
    );
    check(
        &run_parser!(solve_decl(), "solve minimize foo;"),
        expect_test::expect![[r#"Solve(Minimize(Ident { path: ["foo"], is_absolute: false }))"#]],
    );
    check(
        &run_parser!(solve_decl(), "solve maximize foo;"),
        expect_test::expect![[r#"Solve(Maximize(Ident { path: ["foo"], is_absolute: false }))"#]],
    );

    check(
        &run_parser!(solve_decl(), "solve world hunger;"),
        expect_test::expect![[r#"
            @6..11: found "world" but expected "maximize", "minimize",  or "satisfy"
        "#]],
    );
}

#[test]
fn basic_exprs() {
    check(
        &run_parser!(expr(), "123"),
        expect_test::expect!["Immediate(Int(123))"],
    );
    check(
        &run_parser!(expr(), "foo"),
        expect_test::expect![[r#"Ident(Ident { path: ["foo"], is_absolute: false })"#]],
    );
}

#[test]
fn unary_op_exprs() {
    check(
        &run_parser!(expr(), "!a"),
        expect_test::expect![[
            r#"UnaryOp { op: Not, expr: Ident(Ident { path: ["a"], is_absolute: false }) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "+a"),
        expect_test::expect![[
            r#"UnaryOp { op: Pos, expr: Ident(Ident { path: ["a"], is_absolute: false }) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "-a"),
        expect_test::expect![[
            r#"UnaryOp { op: Neg, expr: Ident(Ident { path: ["a"], is_absolute: false }) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "+7"),
        expect_test::expect![[r#"UnaryOp { op: Pos, expr: Immediate(Int(7)) }"#]],
    );
    check(
        &run_parser!(expr(), "+3.4"),
        expect_test::expect![[r#"UnaryOp { op: Pos, expr: Immediate(Real(3.4)) }"#]],
    );
    check(
        &run_parser!(expr(), "+0x456"),
        expect_test::expect![[r#"UnaryOp { op: Pos, expr: Immediate(Int(1110)) }"#]],
    );
    check(
        &run_parser!(expr(), "+0b01010101"),
        expect_test::expect![[r#"UnaryOp { op: Pos, expr: Immediate(Int(85)) }"#]],
    );
    check(
        &run_parser!(
            expr(),
            "+0b1101000000001100101010101010101111111111101010101101010101010101"
        ),
        expect_test::expect![[
            r#"UnaryOp { op: Pos, expr: Immediate(BigInt(14991544915315053909)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "-1.0"),
        expect_test::expect![[r#"UnaryOp { op: Neg, expr: Immediate(Real(1.0)) }"#]],
    );
    check(
        &run_parser!(expr(), "-1"),
        expect_test::expect![[r#"UnaryOp { op: Neg, expr: Immediate(Int(1)) }"#]],
    );
    check(
        &run_parser!(expr(), "-0x133"),
        expect_test::expect![[r#"UnaryOp { op: Neg, expr: Immediate(Int(307)) }"#]],
    );
    check(
        &run_parser!(expr(), "-0b1101"),
        expect_test::expect![[r#"UnaryOp { op: Neg, expr: Immediate(Int(13)) }"#]],
    );
    check(
        &run_parser!(
            expr(),
            "-0b1101000000001100101010101010101010101010101010101101010101010101"
        ),
        expect_test::expect![[
            r#"UnaryOp { op: Neg, expr: Immediate(BigInt(14991544909594023253)) }"#
        ]],
    );
}

#[test]
fn binary_op_exprs() {
    check(
        &run_parser!(expr(), "a * 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: Mul, lhs: Ident(Ident { path: ["a"], is_absolute: false }), rhs: Immediate(Real(2.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a / 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: Div, lhs: Ident(Ident { path: ["a"], is_absolute: false }), rhs: Immediate(Real(2.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a % 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: Mod, lhs: Ident(Ident { path: ["a"], is_absolute: false }), rhs: Immediate(Real(2.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a + 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: Add, lhs: Ident(Ident { path: ["a"], is_absolute: false }), rhs: Immediate(Real(2.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a - 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: Sub, lhs: Ident(Ident { path: ["a"], is_absolute: false }), rhs: Immediate(Real(2.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a+2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: Add, lhs: Ident(Ident { path: ["a"], is_absolute: false }), rhs: Immediate(Real(2.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a-2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: Sub, lhs: Ident(Ident { path: ["a"], is_absolute: false }), rhs: Immediate(Real(2.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a < 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThan, lhs: Ident(Ident { path: ["a"], is_absolute: false }), rhs: Immediate(Real(2.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a > 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: GreaterThan, lhs: Ident(Ident { path: ["a"], is_absolute: false }), rhs: Immediate(Real(2.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a <= 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThanOrEqual, lhs: Ident(Ident { path: ["a"], is_absolute: false }), rhs: Immediate(Real(2.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a >= 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: GreaterThanOrEqual, lhs: Ident(Ident { path: ["a"], is_absolute: false }), rhs: Immediate(Real(2.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a == 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: Equal, lhs: Ident(Ident { path: ["a"], is_absolute: false }), rhs: Immediate(Real(2.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a != 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: NotEqual, lhs: Ident(Ident { path: ["a"], is_absolute: false }), rhs: Immediate(Real(2.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a && b"),
        expect_test::expect![[
            r#"BinaryOp { op: LogicalAnd, lhs: Ident(Ident { path: ["a"], is_absolute: false }), rhs: Ident(Ident { path: ["b"], is_absolute: false }) }"#
        ]],
    );

    check(
        &run_parser!(expr(), "a || b"),
        expect_test::expect![[
            r#"BinaryOp { op: LogicalOr, lhs: Ident(Ident { path: ["a"], is_absolute: false }), rhs: Ident(Ident { path: ["b"], is_absolute: false }) }"#
        ]],
    );

    check(
        &run_parser!(expr(), "a || b && c || d && !e"),
        expect_test::expect![[
            r#"BinaryOp { op: LogicalOr, lhs: BinaryOp { op: LogicalOr, lhs: Ident(Ident { path: ["a"], is_absolute: false }), rhs: BinaryOp { op: LogicalAnd, lhs: Ident(Ident { path: ["b"], is_absolute: false }), rhs: Ident(Ident { path: ["c"], is_absolute: false }) } }, rhs: BinaryOp { op: LogicalAnd, lhs: Ident(Ident { path: ["d"], is_absolute: false }), rhs: UnaryOp { op: Not, expr: Ident(Ident { path: ["e"], is_absolute: false }) } } }"#
        ]],
    );
}

#[test]
fn complex_exprs() {
    check(
        &run_parser!(expr(), "2 * b * 3"),
        expect_test::expect![[
            r#"BinaryOp { op: Mul, lhs: BinaryOp { op: Mul, lhs: Immediate(Int(2)), rhs: Ident(Ident { path: ["b"], is_absolute: false }) }, rhs: Immediate(Int(3)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2 < b * 3"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThan, lhs: Immediate(Int(2)), rhs: BinaryOp { op: Mul, lhs: Ident(Ident { path: ["b"], is_absolute: false }), rhs: Immediate(Int(3)) } }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2.0 > b * 3.0"),
        expect_test::expect![[
            r#"BinaryOp { op: GreaterThan, lhs: Immediate(Real(2.0)), rhs: BinaryOp { op: Mul, lhs: Ident(Ident { path: ["b"], is_absolute: false }), rhs: Immediate(Real(3.0)) } }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2.0 * b < 3.0"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThan, lhs: BinaryOp { op: Mul, lhs: Immediate(Real(2.0)), rhs: Ident(Ident { path: ["b"], is_absolute: false }) }, rhs: Immediate(Real(3.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2 > b < 3"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThan, lhs: BinaryOp { op: GreaterThan, lhs: Immediate(Int(2)), rhs: Ident(Ident { path: ["b"], is_absolute: false }) }, rhs: Immediate(Int(3)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2 != b < 3"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThan, lhs: BinaryOp { op: NotEqual, lhs: Immediate(Int(2)), rhs: Ident(Ident { path: ["b"], is_absolute: false }) }, rhs: Immediate(Int(3)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2 < b != 3"),
        expect_test::expect![[
            r#"BinaryOp { op: NotEqual, lhs: BinaryOp { op: LessThan, lhs: Immediate(Int(2)), rhs: Ident(Ident { path: ["b"], is_absolute: false }) }, rhs: Immediate(Int(3)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a > b * c < d"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThan, lhs: BinaryOp { op: GreaterThan, lhs: Ident(Ident { path: ["a"], is_absolute: false }), rhs: BinaryOp { op: Mul, lhs: Ident(Ident { path: ["b"], is_absolute: false }), rhs: Ident(Ident { path: ["c"], is_absolute: false }) } }, rhs: Ident(Ident { path: ["d"], is_absolute: false }) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2 + 3 * 4"),
        expect_test::expect!["BinaryOp { op: Add, lhs: Immediate(Int(2)), rhs: BinaryOp { op: Mul, lhs: Immediate(Int(3)), rhs: Immediate(Int(4)) } }"],
    );
    check(
        &run_parser!(expr(), "10 - 8 / 4"),
        expect_test::expect!["BinaryOp { op: Sub, lhs: Immediate(Int(10)), rhs: BinaryOp { op: Div, lhs: Immediate(Int(8)), rhs: Immediate(Int(4)) } }"],
    );
    check(
        &run_parser!(expr(), "10 + 8 % 4"),
        expect_test::expect!["BinaryOp { op: Add, lhs: Immediate(Int(10)), rhs: BinaryOp { op: Mod, lhs: Immediate(Int(8)), rhs: Immediate(Int(4)) } }"],
    );
    check(
        &run_parser!(expr(), "2 + 3 * 4 < 5"),
        expect_test::expect!["BinaryOp { op: LessThan, lhs: BinaryOp { op: Add, lhs: Immediate(Int(2)), rhs: BinaryOp { op: Mul, lhs: Immediate(Int(3)), rhs: Immediate(Int(4)) } }, rhs: Immediate(Int(5)) }"],
    );
    check(
        &run_parser!(expr(), "2 * 3 / 4 < 5"),
        expect_test::expect!["BinaryOp { op: LessThan, lhs: BinaryOp { op: Div, lhs: BinaryOp { op: Mul, lhs: Immediate(Int(2)), rhs: Immediate(Int(3)) }, rhs: Immediate(Int(4)) }, rhs: Immediate(Int(5)) }"],
    );
    check(
        &run_parser!(expr(), "10 - 5 + 3 > 7"),
        expect_test::expect!["BinaryOp { op: GreaterThan, lhs: BinaryOp { op: Add, lhs: BinaryOp { op: Sub, lhs: Immediate(Int(10)), rhs: Immediate(Int(5)) }, rhs: Immediate(Int(3)) }, rhs: Immediate(Int(7)) }"],
    );
    check(
        &run_parser!(expr(), "10 % 2 * 4 < 3"),
        expect_test::expect!["BinaryOp { op: LessThan, lhs: BinaryOp { op: Mul, lhs: BinaryOp { op: Mod, lhs: Immediate(Int(10)), rhs: Immediate(Int(2)) }, rhs: Immediate(Int(4)) }, rhs: Immediate(Int(3)) }"],
    );
    check(
        &run_parser!(expr(), "2 + 3 * 4 - 5 / 2 > 1"),
        expect_test::expect!["BinaryOp { op: GreaterThan, lhs: BinaryOp { op: Sub, lhs: BinaryOp { op: Add, lhs: Immediate(Int(2)), rhs: BinaryOp { op: Mul, lhs: Immediate(Int(3)), rhs: Immediate(Int(4)) } }, rhs: BinaryOp { op: Div, lhs: Immediate(Int(5)), rhs: Immediate(Int(2)) } }, rhs: Immediate(Int(1)) }"],
    );
}

#[test]
fn parens_exprs() {
    check(
        &run_parser!(expr(), "(1 + 2) * 3"),
        expect_test::expect!["BinaryOp { op: Mul, lhs: BinaryOp { op: Add, lhs: Immediate(Int(1)), rhs: Immediate(Int(2)) }, rhs: Immediate(Int(3)) }"],
    );
    check(
        &run_parser!(expr(), "1 * (2 + 3)"),
        expect_test::expect!["BinaryOp { op: Mul, lhs: Immediate(Int(1)), rhs: BinaryOp { op: Add, lhs: Immediate(Int(2)), rhs: Immediate(Int(3)) } }"],
    );
    check(
        &run_parser!(expr(), "(1 + 2) * (3 + 4)"),
        expect_test::expect!["BinaryOp { op: Mul, lhs: BinaryOp { op: Add, lhs: Immediate(Int(1)), rhs: Immediate(Int(2)) }, rhs: BinaryOp { op: Add, lhs: Immediate(Int(3)), rhs: Immediate(Int(4)) } }"],
    );
    check(
        &run_parser!(expr(), "(1 + (2 * 3)) * 4"),
        expect_test::expect!["BinaryOp { op: Mul, lhs: BinaryOp { op: Add, lhs: Immediate(Int(1)), rhs: BinaryOp { op: Mul, lhs: Immediate(Int(2)), rhs: Immediate(Int(3)) } }, rhs: Immediate(Int(4)) }"],
    );
    check(
        &run_parser!(expr(), "(1 * (2 + 3)) * 4"),
        expect_test::expect!["BinaryOp { op: Mul, lhs: BinaryOp { op: Mul, lhs: Immediate(Int(1)), rhs: BinaryOp { op: Add, lhs: Immediate(Int(2)), rhs: Immediate(Int(3)) } }, rhs: Immediate(Int(4)) }"],
    );
    check(
        &run_parser!(expr(), "((1 + 2) * 3) * 4"),
        expect_test::expect!["BinaryOp { op: Mul, lhs: BinaryOp { op: Mul, lhs: BinaryOp { op: Add, lhs: Immediate(Int(1)), rhs: Immediate(Int(2)) }, rhs: Immediate(Int(3)) }, rhs: Immediate(Int(4)) }"],
    );
    check(
        &run_parser!(expr(), "((1 + 2) * (3 + 4)) * 5"),
        expect_test::expect!["BinaryOp { op: Mul, lhs: BinaryOp { op: Mul, lhs: BinaryOp { op: Add, lhs: Immediate(Int(1)), rhs: Immediate(Int(2)) }, rhs: BinaryOp { op: Add, lhs: Immediate(Int(3)), rhs: Immediate(Int(4)) } }, rhs: Immediate(Int(5)) }"],
    );
    check(
        &run_parser!(expr(), "(1 + 2) * 3 / 4"),
        expect_test::expect!["BinaryOp { op: Div, lhs: BinaryOp { op: Mul, lhs: BinaryOp { op: Add, lhs: Immediate(Int(1)), rhs: Immediate(Int(2)) }, rhs: Immediate(Int(3)) }, rhs: Immediate(Int(4)) }"],
    );
    check(
        &run_parser!(expr(), "1 / (2 + 3) * 4"),
        expect_test::expect!["BinaryOp { op: Mul, lhs: BinaryOp { op: Div, lhs: Immediate(Int(1)), rhs: BinaryOp { op: Add, lhs: Immediate(Int(2)), rhs: Immediate(Int(3)) } }, rhs: Immediate(Int(4)) }"],
    );
    check(
        &run_parser!(expr(), "(1 < 2) && (3 > 4)"),
        expect_test::expect!["BinaryOp { op: LogicalAnd, lhs: BinaryOp { op: LessThan, lhs: Immediate(Int(1)), rhs: Immediate(Int(2)) }, rhs: BinaryOp { op: GreaterThan, lhs: Immediate(Int(3)), rhs: Immediate(Int(4)) } }"],
    );
    check(
        &run_parser!(expr(), "(1 == 2) || (3 != 4)"),
        expect_test::expect!["BinaryOp { op: LogicalOr, lhs: BinaryOp { op: Equal, lhs: Immediate(Int(1)), rhs: Immediate(Int(2)) }, rhs: BinaryOp { op: NotEqual, lhs: Immediate(Int(3)), rhs: Immediate(Int(4)) } }"],
    );
    check(
        &run_parser!(expr(), "1 < (2 && 3) > 4"),
        expect_test::expect!["BinaryOp { op: GreaterThan, lhs: BinaryOp { op: LessThan, lhs: Immediate(Int(1)), rhs: BinaryOp { op: LogicalAnd, lhs: Immediate(Int(2)), rhs: Immediate(Int(3)) } }, rhs: Immediate(Int(4)) }"],
    );
    check(
        &run_parser!(expr(), "1 && (2 || 3)"),
        expect_test::expect!["BinaryOp { op: LogicalAnd, lhs: Immediate(Int(1)), rhs: BinaryOp { op: LogicalOr, lhs: Immediate(Int(2)), rhs: Immediate(Int(3)) } }"],
    );
    check(
        &run_parser!(expr(), "1 == (2 || 3) != 4"),
        expect_test::expect!["BinaryOp { op: NotEqual, lhs: BinaryOp { op: Equal, lhs: Immediate(Int(1)), rhs: BinaryOp { op: LogicalOr, lhs: Immediate(Int(2)), rhs: Immediate(Int(3)) } }, rhs: Immediate(Int(4)) }"],
    );
    check(
        &run_parser!(expr(), "-(1 + 2)"),
        expect_test::expect!["UnaryOp { op: Neg, expr: BinaryOp { op: Add, lhs: Immediate(Int(1)), rhs: Immediate(Int(2)) } }"],
    );
    check(
        &run_parser!(expr(), "!(a < b)"),
        expect_test::expect![[
            r#"UnaryOp { op: Not, expr: BinaryOp { op: LessThan, lhs: Ident(Ident { path: ["a"], is_absolute: false }), rhs: Ident(Ident { path: ["b"], is_absolute: false }) } }"#
        ]],
    );
    check(
        &run_parser!(expr(), "(1)"),
        expect_test::expect!["Immediate(Int(1))"],
    );
    check(
        &run_parser!(expr(), "(a)"),
        expect_test::expect![[r#"Ident(Ident { path: ["a"], is_absolute: false })"#]],
    );
    check(
        &run_parser!(expr(), "()"),
        expect_test::expect![[r#"
            @1..2: found ")" but expected "::", "::", "!", "+", "-", "{", "{", "(", "if",  or "cond"
        "#]],
    );
    check(
        &run_parser!(expr(), "(if a < b { 1 } else { 2 })"),
        expect_test::expect![[
            r#"If(IfExpr { condition: BinaryOp { op: LessThan, lhs: Ident(Ident { path: ["a"], is_absolute: false }), rhs: Ident(Ident { path: ["b"], is_absolute: false }) }, then_block: Block { statements: [], final_expr: Immediate(Int(1)) }, else_block: Block { statements: [], final_expr: Immediate(Int(2)) } })"#
        ]],
    );
    check(
        &run_parser!(expr(), "(foo(a, b, c))"),
        expect_test::expect![[
            r#"Call { name: Ident { path: ["foo"], is_absolute: false }, args: [Ident(Ident { path: ["a"], is_absolute: false }), Ident(Ident { path: ["b"], is_absolute: false }), Ident(Ident { path: ["c"], is_absolute: false })] }"#
        ]],
    );
}

#[test]
fn idents() {
    check(
        &run_parser!(ident(), "foobar"),
        expect_test::expect![[r#""foobar""#]],
    );
    check(
        &run_parser!(ident(), "foo_bar"),
        expect_test::expect![[r#""foo_bar""#]],
    );
    check(
        &run_parser!(ident(), "FOO_bar"),
        expect_test::expect![[r#""FOO_bar""#]],
    );
    check(
        &run_parser!(ident(), "__FOO"),
        expect_test::expect![[r#""__FOO""#]],
    );
    check(
        &run_parser!(ident(), "_2_FOO1"),
        expect_test::expect![[r#""_2_FOO1""#]],
    );
    check(&run_parser!(ident(), "_"), expect_test::expect![[r#""_""#]]);
    check(
        &run_parser!(ident(), "12_ab"),
        expect_test::expect![[r#"
            @0..2: found "12" but expected something else
        "#]],
    );
    check(
        // Lexer will split this into 3 tokens, ident() will parse the first one.
        &run_parser!(ident(), "ab*cd"),
        expect_test::expect![[r#""ab""#]],
    );
}

#[test]
fn ident_paths() {
    check(
        &run_parser!(ident_path(), "foo::bar"),
        expect_test::expect![[r#"Ident { path: ["foo", "bar"], is_absolute: false }"#]],
    );
    check(
        &run_parser!(ident_path(), "_foo_::_bar"),
        expect_test::expect![[r#"Ident { path: ["_foo_", "_bar"], is_absolute: false }"#]],
    );
    check(
        &run_parser!(ident_path(), "_::_"),
        expect_test::expect![[r#"Ident { path: ["_", "_"], is_absolute: false }"#]],
    );
    check(
        &run_parser!(ident_path(), "t2::_3t::t4_::t"),
        expect_test::expect![[r#"Ident { path: ["t2", "_3t", "t4_", "t"], is_absolute: false }"#]],
    );
    check(
        &run_parser!(ident_path(), "::foo::bar"),
        expect_test::expect![[r#"Ident { path: ["foo", "bar"], is_absolute: true }"#]],
    );

    // As long as these two produce an error... it should be expecting 'ident'.
    check(
        &run_parser!(ident_path().then_ignore(end()), "foo::"),
        expect_test::expect![[r#"
            @5..5: found end of input but expected something else
        "#]],
    );
    check(
        &run_parser!(ident_path().then_ignore(end()), "::foo::"),
        expect_test::expect![[r#"
            @7..7: found end of input but expected something else
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
            r#"[Fn { name: "foo", params: [("x", Real), ("y", Real)], return_type: Real, body: Block { statements: [Let(LetDecl { name: "z", ty: None, init: Some(Immediate(Real(5.0))) })], final_expr: Ident(Ident { path: ["z"], is_absolute: false }) } }]"#
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
            r#"[Let(LetDecl { name: "x", ty: None, init: Some(Call { name: Ident { path: ["foo"], is_absolute: false }, args: [BinaryOp { op: Mul, lhs: Ident(Ident { path: ["a"], is_absolute: false }), rhs: Immediate(Int(3)) }, Ident(Ident { path: ["c"], is_absolute: false })] }) })]"#
        ]],
    );
}

#[test]
fn code_blocks() {
    check(
        &run_parser!(let_decl(expr()), "let x = { 0 };"),
        expect_test::expect![[
            r#"Let(LetDecl { name: "x", ty: None, init: Some(Block(Block { statements: [], final_expr: Immediate(Int(0)) })) })"#
        ]],
    );

    check(
        &run_parser!(let_decl(expr()), "let x = { constraint x > 0.0; 0.0 };"),
        expect_test::expect![[
            r#"Let(LetDecl { name: "x", ty: None, init: Some(Block(Block { statements: [Constraint(BinaryOp { op: GreaterThan, lhs: Ident(Ident { path: ["x"], is_absolute: false }), rhs: Immediate(Real(0.0)) })], final_expr: Immediate(Real(0.0)) })) })"#
        ]],
    );

    check(
        &run_parser!(
            constraint_decl(expr()),
            "constraint { constraint { true }; x > 0 };"
        ),
        expect_test::expect![[
            r#"Constraint(Block(Block { statements: [Constraint(Block(Block { statements: [], final_expr: Immediate(Bool(true)) }))], final_expr: BinaryOp { op: GreaterThan, lhs: Ident(Ident { path: ["x"], is_absolute: false }), rhs: Immediate(Int(0)) } }))"#
        ]],
    );

    check(
        &run_parser!(let_decl(expr()), "let x = { 1.0 } * { 2.0 };"),
        expect_test::expect![[
            r#"Let(LetDecl { name: "x", ty: None, init: Some(BinaryOp { op: Mul, lhs: Block(Block { statements: [], final_expr: Immediate(Real(1.0)) }), rhs: Block(Block { statements: [], final_expr: Immediate(Real(2.0)) }) }) })"#
        ]],
    );

    check(
        &format!("{:?}", run_parser!(let_decl(expr()), "let x = {};")),
        expect_test::expect![[r#""@8..10: empty tuple expressions are not allowed\n""#]],
    );
}

#[test]
fn if_exprs() {
    check(
        &run_parser!(if_expr(expr()), "if c { 1 }"),
        expect_test::expect![[r#"
            @10..10: found end of input but expected "else"
        "#]],
    );

    check(
        &run_parser!(if_expr(expr()), "if c { 1 } else { 0 }"),
        expect_test::expect![[
            r#"If(IfExpr { condition: Ident(Ident { path: ["c"], is_absolute: false }), then_block: Block { statements: [], final_expr: Immediate(Int(1)) }, else_block: Block { statements: [], final_expr: Immediate(Int(0)) } })"#
        ]],
    );

    check(
        &run_parser!(if_expr(expr()), "if c { if c { 1 } else { 0 } } else { 2 }"),
        expect_test::expect![[
            r#"If(IfExpr { condition: Ident(Ident { path: ["c"], is_absolute: false }), then_block: Block { statements: [], final_expr: If(IfExpr { condition: Ident(Ident { path: ["c"], is_absolute: false }), then_block: Block { statements: [], final_expr: Immediate(Int(1)) }, else_block: Block { statements: [], final_expr: Immediate(Int(0)) } }) }, else_block: Block { statements: [], final_expr: Immediate(Int(2)) } })"#
        ]],
    );

    check(
        &run_parser!(if_expr(expr()), "if c { if c { 1 } else { 0 } } else { 2 }"),
        expect_test::expect![[
            r#"If(IfExpr { condition: Ident(Ident { path: ["c"], is_absolute: false }), then_block: Block { statements: [], final_expr: If(IfExpr { condition: Ident(Ident { path: ["c"], is_absolute: false }), then_block: Block { statements: [], final_expr: Immediate(Int(1)) }, else_block: Block { statements: [], final_expr: Immediate(Int(0)) } }) }, else_block: Block { statements: [], final_expr: Immediate(Int(2)) } })"#
        ]],
    );
}

#[test]
fn tuple_expressions() {
    check(
        &run_parser!(expr(), r#"{0}"#), // This is not a tuple. It is a code block expr.
        expect_test::expect!["Block(Block { statements: [], final_expr: Immediate(Int(0)) })"],
    );

    check(
        &run_parser!(expr(), r#"{x: 0}"#), // This is a tuple because the field is named so there is no ambiguity
        expect_test::expect![[r#"Tuple([(Some("x"), Immediate(Int(0)))])"#]],
    );

    check(
        &run_parser!(expr(), r#"{0,}"#), // This is a tuple
        expect_test::expect!["Tuple([(None, Immediate(Int(0)))])"],
    );

    check(
        &run_parser!(expr(), r#"{x: 0,}"#), // This is a tuple
        expect_test::expect![[r#"Tuple([(Some("x"), Immediate(Int(0)))])"#]],
    );

    check(
        &run_parser!(expr(), r#"{0, 1.0, "foo"}"#),
        expect_test::expect![[
            r#"Tuple([(None, Immediate(Int(0))), (None, Immediate(Real(1.0))), (None, Immediate(String("foo")))])"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"{x: 0, y: 1.0, z: "foo"}"#),
        expect_test::expect![[
            r#"Tuple([(Some("x"), Immediate(Int(0))), (Some("y"), Immediate(Real(1.0))), (Some("z"), Immediate(String("foo")))])"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"{0, {1.0, "bar"}, "foo"}"#),
        expect_test::expect![[
            r#"Tuple([(None, Immediate(Int(0))), (None, Tuple([(None, Immediate(Real(1.0))), (None, Immediate(String("bar")))])), (None, Immediate(String("foo")))])"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"{x: 0, {y: 1.0, "bar"}, z: "foo"}"#),
        expect_test::expect![[
            r#"Tuple([(Some("x"), Immediate(Int(0))), (None, Tuple([(Some("y"), Immediate(Real(1.0))), (None, Immediate(String("bar")))])), (Some("z"), Immediate(String("foo")))])"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"{ { 42 }, if c { 2 } else { 3 }, foo() }"#),
        expect_test::expect![[
            r#"Tuple([(None, Block(Block { statements: [], final_expr: Immediate(Int(42)) })), (None, If(IfExpr { condition: Ident(Ident { path: ["c"], is_absolute: false }), then_block: Block { statements: [], final_expr: Immediate(Int(2)) }, else_block: Block { statements: [], final_expr: Immediate(Int(3)) } })), (None, Call { name: Ident { path: ["foo"], is_absolute: false }, args: [] })])"#
        ]],
    );

    check(
        &run_parser!(
            expr(),
            r#"{ x: { 42 }, y: if c { 2 } else { 3 }, z: foo() }"#
        ),
        expect_test::expect![[
            r#"Tuple([(Some("x"), Block(Block { statements: [], final_expr: Immediate(Int(42)) })), (Some("y"), If(IfExpr { condition: Ident(Ident { path: ["c"], is_absolute: false }), then_block: Block { statements: [], final_expr: Immediate(Int(2)) }, else_block: Block { statements: [], final_expr: Immediate(Int(3)) } })), (Some("z"), Call { name: Ident { path: ["foo"], is_absolute: false }, args: [] })])"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"t.0 + t.9999999 + t.x"#),
        expect_test::expect![[
            r#"BinaryOp { op: Add, lhs: BinaryOp { op: Add, lhs: TupleFieldAccess { tuple: Ident(Ident { path: ["t"], is_absolute: false }), field: Left(0) }, rhs: TupleFieldAccess { tuple: Ident(Ident { path: ["t"], is_absolute: false }), field: Left(9999999) } }, rhs: TupleFieldAccess { tuple: Ident(Ident { path: ["t"], is_absolute: false }), field: Right("x") } }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"{0, 1}.0"#),
        expect_test::expect!["TupleFieldAccess { tuple: Tuple([(None, Immediate(Int(0))), (None, Immediate(Int(1)))]), field: Left(0) }"],
    );

    check(
        &run_parser!(expr(), r#"{0, 1}.x"#),
        expect_test::expect![[
            r#"TupleFieldAccess { tuple: Tuple([(None, Immediate(Int(0))), (None, Immediate(Int(1)))]), field: Right("x") }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"t.0 .0"#),
        expect_test::expect![[
            r#"TupleFieldAccess { tuple: TupleFieldAccess { tuple: Ident(Ident { path: ["t"], is_absolute: false }), field: Left(0) }, field: Left(0) }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"t.x .y"#),
        expect_test::expect![[
            r#"TupleFieldAccess { tuple: TupleFieldAccess { tuple: Ident(Ident { path: ["t"], is_absolute: false }), field: Right("x") }, field: Right("y") }"#
        ]],
    );

    check(
        &run_parser!(expr(), "t \r .1 .2.2. \n 3 . \t 13 . 1.1"),
        expect_test::expect![[
            r#"TupleFieldAccess { tuple: TupleFieldAccess { tuple: TupleFieldAccess { tuple: TupleFieldAccess { tuple: TupleFieldAccess { tuple: TupleFieldAccess { tuple: TupleFieldAccess { tuple: Ident(Ident { path: ["t"], is_absolute: false }), field: Left(1) }, field: Left(2) }, field: Left(2) }, field: Left(3) }, field: Left(13) }, field: Left(1) }, field: Left(1) }"#
        ]],
    );

    check(
        &run_parser!(expr(), "t \r .x .1.2. \n w . \t t. 3.4"),
        expect_test::expect![[
            r#"TupleFieldAccess { tuple: TupleFieldAccess { tuple: TupleFieldAccess { tuple: TupleFieldAccess { tuple: TupleFieldAccess { tuple: TupleFieldAccess { tuple: TupleFieldAccess { tuple: Ident(Ident { path: ["t"], is_absolute: false }), field: Right("x") }, field: Left(1) }, field: Left(2) }, field: Right("w") }, field: Right("t") }, field: Left(3) }, field: Left(4) }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"foo().0.1"#),
        expect_test::expect![[
            r#"TupleFieldAccess { tuple: TupleFieldAccess { tuple: Call { name: Ident { path: ["foo"], is_absolute: false }, args: [] }, field: Left(0) }, field: Left(1) }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"foo().a.b.0.1"#),
        expect_test::expect![[
            r#"TupleFieldAccess { tuple: TupleFieldAccess { tuple: TupleFieldAccess { tuple: TupleFieldAccess { tuple: Call { name: Ident { path: ["foo"], is_absolute: false }, args: [] }, field: Right("a") }, field: Right("b") }, field: Left(0) }, field: Left(1) }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"{ {0, 0} }.0"#),
        expect_test::expect!["TupleFieldAccess { tuple: Block(Block { statements: [], final_expr: Tuple([(None, Immediate(Int(0))), (None, Immediate(Int(0)))]) }), field: Left(0) }"],
    );

    check(
        &run_parser!(expr(), r#"{ {0, 0} }.a"#),
        expect_test::expect![[
            r#"TupleFieldAccess { tuple: Block(Block { statements: [], final_expr: Tuple([(None, Immediate(Int(0))), (None, Immediate(Int(0)))]) }), field: Right("a") }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"if true { {0, 0} } else { {0, 0} }.0"#),
        expect_test::expect!["TupleFieldAccess { tuple: If(IfExpr { condition: Immediate(Bool(true)), then_block: Block { statements: [], final_expr: Tuple([(None, Immediate(Int(0))), (None, Immediate(Int(0)))]) }, else_block: Block { statements: [], final_expr: Tuple([(None, Immediate(Int(0))), (None, Immediate(Int(0)))]) } }), field: Left(0) }"],
    );

    check(
        &run_parser!(expr(), r#"if true { {0, 0} } else { {0, 0} }.x"#),
        expect_test::expect![[
            r#"TupleFieldAccess { tuple: If(IfExpr { condition: Immediate(Bool(true)), then_block: Block { statements: [], final_expr: Tuple([(None, Immediate(Int(0))), (None, Immediate(Int(0)))]) }, else_block: Block { statements: [], final_expr: Tuple([(None, Immediate(Int(0))), (None, Immediate(Int(0)))]) } }), field: Right("x") }"#
        ]],
    );

    // This parses because `1 + 2` is an expression, but it should fail in semantic analysis.
    check(
        &run_parser!(expr(), "1 + 2 .3"),
        expect_test::expect!["BinaryOp { op: Add, lhs: Immediate(Int(1)), rhs: TupleFieldAccess { tuple: Immediate(Int(2)), field: Left(3) } }"],
    );

    // This parses because `1 + 2` is an expression, but it should fail in semantic analysis.
    check(
        &run_parser!(expr(), "1 + 2 .a"),
        expect_test::expect![[
            r#"BinaryOp { op: Add, lhs: Immediate(Int(1)), rhs: TupleFieldAccess { tuple: Immediate(Int(2)), field: Right("a") } }"#
        ]],
    );

    check(
        &run_parser!(let_decl(expr()), "let x = t.0xa;"),
        expect_test::expect![[r#"
            @10..13: invalid integer value "0xa" for tuple index
        "#]],
    );

    check(
        &run_parser!(let_decl(expr()), "let x = t.111111111111111111111111111;"),
        expect_test::expect![[r#"
            @10..37: invalid integer value "111111111111111111111111111" for tuple index
        "#]],
    );

    check(
        &run_parser!(let_decl(expr()), "let x = t.111111111111111111111111111.2;"),
        expect_test::expect![[r#"
            @10..37: invalid integer value "111111111111111111111111111" for tuple index
        "#]],
    );

    check(
        &run_parser!(let_decl(expr()), "let x = t.2.111111111111111111111111111;"),
        expect_test::expect![[r#"
            @12..39: invalid integer value "111111111111111111111111111" for tuple index
        "#]],
    );

    check(
        &run_parser!(
            let_decl(expr()),
            "let x = t.222222222222222222222.111111111111111111111111111;"
        ),
        expect_test::expect![[r#"
            @10..31: invalid integer value "222222222222222222222" for tuple index
        "#]],
    );

    check(
        &run_parser!(let_decl(expr()), "let x = t.1e5;"),
        expect_test::expect![[r#"
            @10..13: invalid value "1e5" for tuple index
        "#]],
    );

    check(
        &run_parser!(let_decl(expr()), "let bad_tuple:{} = {};"),
        expect_test::expect![[r#"
            @14..16: empty tuple types are not allowed
            @19..21: empty tuple expressions are not allowed
        "#]],
    );
}

#[test]
fn cond_exprs() {
    check(
        &run_parser!(cond_expr(expr()), r#"cond { else => a }"#),
        expect_test::expect![[
            r#"Cond(CondExpr { branches: [], else_result: Ident(Ident { path: ["a"], is_absolute: false }) })"#
        ]],
    );

    check(
        &run_parser!(cond_expr(expr()), r#"cond { else => { a } }"#),
        expect_test::expect![[
            r#"Cond(CondExpr { branches: [], else_result: Block(Block { statements: [], final_expr: Ident(Ident { path: ["a"], is_absolute: false }) }) })"#
        ]],
    );

    check(
        &run_parser!(cond_expr(expr()), r#"cond { a => b, else => c }"#),
        expect_test::expect![[
            r#"Cond(CondExpr { branches: [CondBranch { condition: Ident(Ident { path: ["a"], is_absolute: false }), result: Ident(Ident { path: ["b"], is_absolute: false }) }], else_result: Ident(Ident { path: ["c"], is_absolute: false }) })"#
        ]],
    );

    check(
        &run_parser!(cond_expr(expr()), r#"cond { a => { b }, else => c, }"#),
        expect_test::expect![[
            r#"Cond(CondExpr { branches: [CondBranch { condition: Ident(Ident { path: ["a"], is_absolute: false }), result: Block(Block { statements: [], final_expr: Ident(Ident { path: ["b"], is_absolute: false }) }) }], else_result: Ident(Ident { path: ["c"], is_absolute: false }) })"#
        ]],
    );

    check(
        &run_parser!(
            cond_expr(expr()),
            r#"cond { a => b, { true } => d, else => f, }"#
        ),
        expect_test::expect![[
            r#"Cond(CondExpr { branches: [CondBranch { condition: Ident(Ident { path: ["a"], is_absolute: false }), result: Ident(Ident { path: ["b"], is_absolute: false }) }, CondBranch { condition: Block(Block { statements: [], final_expr: Immediate(Bool(true)) }), result: Ident(Ident { path: ["d"], is_absolute: false }) }], else_result: Ident(Ident { path: ["f"], is_absolute: false }) })"#
        ]],
    );

    check(
        &run_parser!(cond_expr(expr()), r#"cond { a => b, }"#),
        expect_test::expect![[r#"
            @15..16: found "}" but expected "::", "::", "!", "+", "-", "{", "{", "(", "if", "else",  or "cond"
        "#]],
    );

    check(
        &run_parser!(cond_expr(expr()), r#"cond { else => a, b => c }"#),
        expect_test::expect![[r#"
            @18..19: found "b" but expected "}"
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
            r#"[Let(LetDecl { name: "low_val", ty: Some(Real), init: Some(Immediate(Real(1.23))) }), Let(LetDecl { name: "high_val", ty: None, init: Some(Immediate(Real(4.56))) }), Constraint(BinaryOp { op: GreaterThan, lhs: Ident(Ident { path: ["mid"], is_absolute: false }), rhs: BinaryOp { op: Mul, lhs: Ident(Ident { path: ["low_val"], is_absolute: false }), rhs: Immediate(Real(2.0)) } }), Constraint(BinaryOp { op: LessThan, lhs: Ident(Ident { path: ["mid"], is_absolute: false }), rhs: Ident(Ident { path: ["high_val"], is_absolute: false }) }), Solve(Minimize(Ident { path: ["mid"], is_absolute: false }))]"#
        ]],
    );
}

#[test]
fn with_errors() {
    check(
        &run_parser!(yurt_program(), "let low_val: bad = 1.23"),
        expect_test::expect![[r#"
            @13..16: found "bad" but expected "{", "real", "int", "bool",  or "string"
        "#]],
    );
}

#[test]
fn fn_errors() {
    check(
        &run_parser!(yurt_program(), "fn foo() {5}"),
        expect_test::expect![[r#"
            @9..10: found "{" but expected "->"
        "#]],
    );

    check(
        &run_parser!(yurt_program(), "fn foo() -> real {}"),
        expect_test::expect![[r#"
            @18..19: found "}" but expected "::", "::", "!", "+", "-", "{", "{", "(", "if", "cond", "let",  or "constraint"
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
            r#"[Solve(Maximize(Ident { path: ["low"], is_absolute: false })), Constraint(BinaryOp { op: LessThan, lhs: Ident(Ident { path: ["low"], is_absolute: false }), rhs: Ident(Ident { path: ["high"], is_absolute: false }) }), Let(LetDecl { name: "high", ty: None, init: Some(Immediate(Real(2.0))) }), Solve(Satisfy), Let(LetDecl { name: "low", ty: None, init: Some(Immediate(Real(1.0))) })]"#
        ]],
    );
}

#[test]
fn keywords_as_identifiers_errors() {
    for keyword in KEYWORDS {
        let src = format!("let {keyword} = 5;");
        assert_eq!(
            &run_parser!(yurt_program(), &src),
            &format!(
                "@4..{}: expected identifier, found keyword \"{keyword}\"\n",
                4 + format!("{keyword}").len() // End of the error span
            ),
            "Check \"identifier as keyword\" error for  keyword \"{}\"",
            keyword
        );
    }
}

#[test]
fn test_parse_str_to_ast() {
    check(
        &format!("{:?}", parse_str_to_ast("let x = 5;", "my_file")),
        expect_test::expect![[
            r#"Ok([Let(LetDecl { name: "x", ty: None, init: Some(Immediate(Int(5))) })])"#
        ]],
    );
    check(
        &format!("{:?}", parse_str_to_ast("let x = 5", "my_file")),
        expect_test::expect![[r#"Err(could not compile "my_file" due to previous error)"#]],
    );
    check(
        &format!("{:?}", parse_str_to_ast("@ @", "my_file")),
        expect_test::expect![[r#"Err(could not compile "my_file" due to 2 previous errors)"#]],
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
            r#"Let(LetDecl { name: "blah", ty: None, init: Some(Immediate(BigInt(1234567890123456789012345678901234567890))) })"#
        ]],
    );
    check(
        &run_parser!(
            let_decl(expr()),
            "let blah = 0xfeedbadf00d2adeadcafed00dbabeface;"
        ),
        // Confirmed by using the Python REPL to convert from hex to dec...
        expect_test::expect![[
            r#"Let(LetDecl { name: "blah", ty: None, init: Some(Immediate(BigInt(5421732407698601623698172315373246806734))) })"#
        ]],
    );
    check(
        &run_parser!(
            expr(),
            "0b110100101001010101010101010101010011010011010101010101010101010101010101010 + \
            0b01001001010110101010101001010101010010100100101001010010100100100001010"
        ),
        // Again confirmed using the Python REPL.  Handy.
        expect_test::expect![
            "BinaryOp { op: Add, lhs: Immediate(BigInt(31076614848392666458794)), rhs: Immediate(BigInt(676572722683907229962)) }"
        ],
    );
}
