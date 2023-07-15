use crate::{
    error::CompileError,
    lexer::{self, KEYWORDS},
    parser::*,
};
use chumsky::Stream;

// To-do tests:
// - block - constraints { .. }
// - parens in expressions
// - unary negation, boolean not
// - can't have keywords as idents

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
        &run_parser!(type_(), "(int, real, string)"),
        expect_test::expect!["Tuple([Int, Real, String])"],
    );
    check(
        &run_parser!(type_(), "(int, (real, int), string)"),
        expect_test::expect!["Tuple([Int, Tuple([Real, Int]), String])"],
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
        &run_parser!(immediate(), "-1;"),
        expect_test::expect![[r#"Int(-1)"#]],
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
        &run_parser!(immediate(), "-9223372036854775809;"),
        expect_test::expect![[r#"BigInt(-9223372036854775809)"#]],
    );
    check(
        &run_parser!(immediate(), "-1.3;"),
        expect_test::expect![[r#"Real(-1.3)"#]],
    );
}

#[test]
fn let_decls() {
    check(
        &run_parser!(let_decl(expr()), "let blah = 1.0;"),
        expect_test::expect![[
            r#"Let(LetStatement { name: Ident("blah"), ty: None, init: Immediate(Real(1.0)) })"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: real = 1.0;"),
        expect_test::expect![[
            r#"Let(LetStatement { name: Ident("blah"), ty: Some(Real), init: Immediate(Real(1.0)) })"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: real"),
        expect_test::expect![[r#"
            @14..14: found end of input but expected "="
        "#]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah = 1;"),
        expect_test::expect![[
            r#"Let(LetStatement { name: Ident("blah"), ty: None, init: Immediate(Int(1)) })"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: int = 1;"),
        expect_test::expect![[
            r#"Let(LetStatement { name: Ident("blah"), ty: Some(Int), init: Immediate(Int(1)) })"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: int"),
        expect_test::expect![[r#"
            @13..13: found end of input but expected "="
        "#]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah = true;"),
        expect_test::expect![[
            r#"Let(LetStatement { name: Ident("blah"), ty: None, init: Immediate(Bool(true)) })"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: bool = false;"),
        expect_test::expect![[
            r#"Let(LetStatement { name: Ident("blah"), ty: Some(Bool), init: Immediate(Bool(false)) })"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: bool"),
        expect_test::expect![[r#"
            @14..14: found end of input but expected "="
        "#]],
    );
    check(
        &run_parser!(let_decl(expr()), r#"let blah = "hello";"#),
        expect_test::expect![[
            r#"Let(LetStatement { name: Ident("blah"), ty: None, init: Immediate(String("hello")) })"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), r#"let blah: string = "hello";"#),
        expect_test::expect![[
            r#"Let(LetStatement { name: Ident("blah"), ty: Some(String), init: Immediate(String("hello")) })"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), r#"let blah: string"#),
        expect_test::expect![[r#"
            @16..16: found end of input but expected "="
        "#]],
    );
}

#[test]
fn var_decls() {
    check(
        &run_parser!(var_decl(expr()), "var blah;"),
        expect_test::expect![[r#"
            @0..9: type annotation or initializer needed for decision variable "blah"
        "#]],
    );
    check(
        &run_parser!(var_decl(expr()), "var blah = 1.0;"),
        expect_test::expect![[
            r#"Var(VarStatement { name: Ident("blah"), ty: None, init: Some(Immediate(Real(1.0))) })"#
        ]],
    );
    check(
        &run_parser!(var_decl(expr()), "var blah: real = 1.0;"),
        expect_test::expect![[
            r#"Var(VarStatement { name: Ident("blah"), ty: Some(Real), init: Some(Immediate(Real(1.0))) })"#
        ]],
    );
    check(
        &run_parser!(var_decl(expr()), "var blah: real;"),
        expect_test::expect![[
            r#"Var(VarStatement { name: Ident("blah"), ty: Some(Real), init: None })"#
        ]],
    );
    check(
        &run_parser!(var_decl(expr()), "var blah = 1;"),
        expect_test::expect![[
            r#"Var(VarStatement { name: Ident("blah"), ty: None, init: Some(Immediate(Int(1))) })"#
        ]],
    );
    check(
        &run_parser!(var_decl(expr()), "var blah: int = 1;"),
        expect_test::expect![[
            r#"Var(VarStatement { name: Ident("blah"), ty: Some(Int), init: Some(Immediate(Int(1))) })"#
        ]],
    );
    check(
        &run_parser!(var_decl(expr()), "var blah: int;"),
        expect_test::expect![[
            r#"Var(VarStatement { name: Ident("blah"), ty: Some(Int), init: None })"#
        ]],
    );
    check(
        &run_parser!(var_decl(expr()), "var blah = true;"),
        expect_test::expect![[
            r#"Var(VarStatement { name: Ident("blah"), ty: None, init: Some(Immediate(Bool(true))) })"#
        ]],
    );
    check(
        &run_parser!(var_decl(expr()), "var blah: bool = false;"),
        expect_test::expect![[
            r#"Var(VarStatement { name: Ident("blah"), ty: Some(Bool), init: Some(Immediate(Bool(false))) })"#
        ]],
    );
    check(
        &run_parser!(var_decl(expr()), "var blah: bool;"),
        expect_test::expect![[
            r#"Var(VarStatement { name: Ident("blah"), ty: Some(Bool), init: None })"#
        ]],
    );
    check(
        &run_parser!(var_decl(expr()), r#"var blah = "hello";"#),
        expect_test::expect![[
            r#"Var(VarStatement { name: Ident("blah"), ty: None, init: Some(Immediate(String("hello"))) })"#
        ]],
    );
    check(
        &run_parser!(var_decl(expr()), r#"var blah: string = "hello";"#),
        expect_test::expect![[
            r#"Var(VarStatement { name: Ident("blah"), ty: Some(String), init: Some(Immediate(String("hello"))) })"#
        ]],
    );
    check(
        &run_parser!(var_decl(expr()), r#"var blah: string;"#),
        expect_test::expect![[
            r#"Var(VarStatement { name: Ident("blah"), ty: Some(String), init: None })"#
        ]],
    );
}

#[test]
fn constraint_decls() {
    // Argument just needs to be any expression, as far as the parser is concerned.
    check(
        &run_parser!(constraint_decl(expr()), "constraint blah;"),
        expect_test::expect![[r#"Constraint(Ident(Ident("blah")))"#]],
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
        expect_test::expect![[r#"Solve(Minimize(Ident("foo")))"#]],
    );
    check(
        &run_parser!(solve_decl(), "solve maximize foo;"),
        expect_test::expect![[r#"Solve(Maximize(Ident("foo")))"#]],
    );

    check(
        &run_parser!(solve_decl(), "solve world hunger;"),
        expect_test::expect![[r#"
            @6..11: found "world" but expected "maximize", "minimize",  or "satisfy"
        "#]],
    );
}

#[test]
fn exprs() {
    check(
        &run_parser!(expr(), "123"),
        expect_test::expect!["Immediate(Int(123))"],
    );
    check(
        &run_parser!(expr(), "foo"),
        expect_test::expect![[r#"Ident(Ident("foo"))"#]],
    );
    check(
        &run_parser!(expr(), "!a"),
        expect_test::expect![[r#"UnaryOp { op: Not, expr: Ident(Ident("a")) }"#]],
    );
    check(
        &run_parser!(expr(), "+a"),
        expect_test::expect![[r#"UnaryOp { op: Pos, expr: Ident(Ident("a")) }"#]],
    );
    check(
        &run_parser!(expr(), "-a"),
        expect_test::expect![[r#"UnaryOp { op: Neg, expr: Ident(Ident("a")) }"#]],
    );
    check(
        &run_parser!(expr(), "-0x111"),
        expect_test::expect![[r#"UnaryOp { op: Neg, expr: Immediate(Int(273)) }"#]],
    );
    check(
        &run_parser!(expr(), "-0b111"),
        expect_test::expect![[r#"UnaryOp { op: Neg, expr: Immediate(Int(7)) }"#]],
    );
    check(
        &run_parser!(expr(), "a * 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: Mul, lhs: Ident(Ident("a")), rhs: Immediate(Real(2.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a / 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: Div, lhs: Ident(Ident("a")), rhs: Immediate(Real(2.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a % 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: Mod, lhs: Ident(Ident("a")), rhs: Immediate(Real(2.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a + 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: Add, lhs: Ident(Ident("a")), rhs: Immediate(Real(2.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a - 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: Sub, lhs: Ident(Ident("a")), rhs: Immediate(Real(2.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a < 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThan, lhs: Ident(Ident("a")), rhs: Immediate(Real(2.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a > 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: GreaterThan, lhs: Ident(Ident("a")), rhs: Immediate(Real(2.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a <= 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThanOrEqual, lhs: Ident(Ident("a")), rhs: Immediate(Real(2.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a >= 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: GreaterThanOrEqual, lhs: Ident(Ident("a")), rhs: Immediate(Real(2.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a == 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: Equal, lhs: Ident(Ident("a")), rhs: Immediate(Real(2.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a != 2.0"),
        expect_test::expect![[
            r#"BinaryOp { op: NotEqual, lhs: Ident(Ident("a")), rhs: Immediate(Real(2.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2 * b * 3"),
        expect_test::expect![[
            r#"BinaryOp { op: Mul, lhs: BinaryOp { op: Mul, lhs: Immediate(Int(2)), rhs: Ident(Ident("b")) }, rhs: Immediate(Int(3)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2 < b * 3"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThan, lhs: Immediate(Int(2)), rhs: BinaryOp { op: Mul, lhs: Ident(Ident("b")), rhs: Immediate(Int(3)) } }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2.0 > b * 3.0"),
        expect_test::expect![[
            r#"BinaryOp { op: GreaterThan, lhs: Immediate(Real(2.0)), rhs: BinaryOp { op: Mul, lhs: Ident(Ident("b")), rhs: Immediate(Real(3.0)) } }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2.0 * b < 3.0"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThan, lhs: BinaryOp { op: Mul, lhs: Immediate(Real(2.0)), rhs: Ident(Ident("b")) }, rhs: Immediate(Real(3.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2 > b < 3"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThan, lhs: BinaryOp { op: GreaterThan, lhs: Immediate(Int(2)), rhs: Ident(Ident("b")) }, rhs: Immediate(Int(3)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2 != b < 3"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThan, lhs: BinaryOp { op: NotEqual, lhs: Immediate(Int(2)), rhs: Ident(Ident("b")) }, rhs: Immediate(Int(3)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2 < b != 3"),
        expect_test::expect![[
            r#"BinaryOp { op: NotEqual, lhs: BinaryOp { op: LessThan, lhs: Immediate(Int(2)), rhs: Ident(Ident("b")) }, rhs: Immediate(Int(3)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "a > b * c < d"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThan, lhs: BinaryOp { op: GreaterThan, lhs: Ident(Ident("a")), rhs: BinaryOp { op: Mul, lhs: Ident(Ident("b")), rhs: Ident(Ident("c")) } }, rhs: Ident(Ident("d")) }"#
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
fn idents() {
    check(
        &run_parser!(ident(), "foobar"),
        expect_test::expect![[r#"Ident("foobar")"#]],
    );
    check(
        &run_parser!(ident(), "foo_bar"),
        expect_test::expect![[r#"Ident("foo_bar")"#]],
    );
    check(
        &run_parser!(ident(), "FOO_bar"),
        expect_test::expect![[r#"Ident("FOO_bar")"#]],
    );
    check(
        &run_parser!(ident(), "__FOO"),
        expect_test::expect![[r#"Ident("__FOO")"#]],
    );
    check(
        &run_parser!(ident(), "_2_FOO1"),
        expect_test::expect![[r#"Ident("_2_FOO1")"#]],
    );
    check(
        &run_parser!(ident(), "_"),
        expect_test::expect![[r#"Ident("_")"#]],
    );
    check(
        &run_parser!(ident(), "12_ab"),
        expect_test::expect![[r#"
            @0..2: found "12" but expected something else
        "#]],
    );
    check(
        // Lexer will split this into 3 tokens, ident() will parse the first one.
        &run_parser!(ident(), "ab*cd"),
        expect_test::expect![[r#"Ident("ab")"#]],
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
            r#"[Fn { name: Ident("foo"), params: [(Ident("x"), Real), (Ident("y"), Real)], return_type: Real, body: Block { statements: [Let(LetStatement { name: Ident("z"), ty: None, init: Immediate(Real(5.0)) })], final_expr: Ident(Ident("z")) } }]"#
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
            r#"[Let(LetStatement { name: Ident("x"), ty: None, init: Call { name: Ident("foo"), args: [BinaryOp { op: Mul, lhs: Ident(Ident("a")), rhs: Immediate(Int(3)) }, Ident(Ident("c"))] } })]"#
        ]],
    );
}

#[test]
fn code_blocks() {
    check(
        &run_parser!(let_decl(expr()), "let x = { 0 };"),
        expect_test::expect![[
            r#"Let(LetStatement { name: Ident("x"), ty: None, init: Block(Block { statements: [], final_expr: Immediate(Int(0)) }) })"#
        ]],
    );

    check(
        &run_parser!(let_decl(expr()), "let x = { constraint x > 0.0; 0.0 };"),
        expect_test::expect![[
            r#"Let(LetStatement { name: Ident("x"), ty: None, init: Block(Block { statements: [Constraint(BinaryOp { op: GreaterThan, lhs: Ident(Ident("x")), rhs: Immediate(Real(0.0)) })], final_expr: Immediate(Real(0.0)) }) })"#
        ]],
    );

    check(
        &run_parser!(
            constraint_decl(expr()),
            "constraint { constraint { true }; x > 0 };"
        ),
        expect_test::expect![[
            r#"Constraint(Block(Block { statements: [Constraint(Block(Block { statements: [], final_expr: Immediate(Bool(true)) }))], final_expr: BinaryOp { op: GreaterThan, lhs: Ident(Ident("x")), rhs: Immediate(Int(0)) } }))"#
        ]],
    );

    check(
        &run_parser!(let_decl(expr()), "let x = { 1.0 } * { 2.0 };"),
        expect_test::expect![[
            r#"Let(LetStatement { name: Ident("x"), ty: None, init: BinaryOp { op: Mul, lhs: Block(Block { statements: [], final_expr: Immediate(Real(1.0)) }), rhs: Block(Block { statements: [], final_expr: Immediate(Real(2.0)) }) } })"#
        ]],
    );

    check(
        &format!("{:?}", run_parser!(let_decl(expr()), "let x = {};")),
        expect_test::expect![[
            r#""@9..10: found \"}\" but expected \"!\", \"+\", \"-\", \"{\", \"(\", \"if\", \"var\", \"let\",  or \"constraint\"\n""#
        ]],
    );
}

#[test]
fn if_exprs() {
    check(
        &run_parser!(if_expr(expr()), "if cond { 1 }"),
        expect_test::expect![[r#"
            @13..13: found end of input but expected "else"
        "#]],
    );

    check(
        &run_parser!(if_expr(expr()), "if cond { 1 } else { 0 }"),
        expect_test::expect![[
            r#"If(IfExpr { condition: Ident(Ident("cond")), then_block: Block { statements: [], final_expr: Immediate(Int(1)) }, else_block: Block { statements: [], final_expr: Immediate(Int(0)) } })"#
        ]],
    );

    check(
        &run_parser!(
            if_expr(expr()),
            "if cond { if cond { 1 } else { 0 } } else { 2 }"
        ),
        expect_test::expect![[
            r#"If(IfExpr { condition: Ident(Ident("cond")), then_block: Block { statements: [], final_expr: If(IfExpr { condition: Ident(Ident("cond")), then_block: Block { statements: [], final_expr: Immediate(Int(1)) }, else_block: Block { statements: [], final_expr: Immediate(Int(0)) } }) }, else_block: Block { statements: [], final_expr: Immediate(Int(2)) } })"#
        ]],
    );

    check(
        &run_parser!(
            if_expr(expr()),
            "if cond { 1.0 } else { if cond { 2.0 } else { 3.0 } }"
        ),
        expect_test::expect![[
            r#"If(IfExpr { condition: Ident(Ident("cond")), then_block: Block { statements: [], final_expr: Immediate(Real(1.0)) }, else_block: Block { statements: [], final_expr: If(IfExpr { condition: Ident(Ident("cond")), then_block: Block { statements: [], final_expr: Immediate(Real(2.0)) }, else_block: Block { statements: [], final_expr: Immediate(Real(3.0)) } }) } })"#
        ]],
    );
}

#[test]
fn tuple_expressions() {
    check(
        &run_parser!(expr(), r#"(0,)"#),
        expect_test::expect!["Tuple([Immediate(Int(0))])"],
    );

    check(
        &run_parser!(expr(), r#"(0, 1.0, "foo")"#),
        expect_test::expect![[
            r#"Tuple([Immediate(Int(0)), Immediate(Real(1.0)), Immediate(String("foo"))])"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"(0, (1.0, "bar"), "foo")"#),
        expect_test::expect![[
            r#"Tuple([Immediate(Int(0)), Tuple([Immediate(Real(1.0)), Immediate(String("bar"))]), Immediate(String("foo"))])"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"( { 42 }, if cond { 2 } else { 3 }, foo() )"#),
        expect_test::expect![[
            r#"Tuple([Block(Block { statements: [], final_expr: Immediate(Int(42)) }), If(IfExpr { condition: Ident(Ident("cond")), then_block: Block { statements: [], final_expr: Immediate(Int(2)) }, else_block: Block { statements: [], final_expr: Immediate(Int(3)) } }), Call { name: Ident("foo"), args: [] }])"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"t.0 + t.9999999"#),
        expect_test::expect![[
            r#"BinaryOp { op: Add, lhs: TupleIndex { tuple: Ident(Ident("t")), index: 0 }, rhs: TupleIndex { tuple: Ident(Ident("t")), index: 9999999 } }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"(0, 1).0"#),
        expect_test::expect![
            "TupleIndex { tuple: Tuple([Immediate(Int(0)), Immediate(Int(1))]), index: 0 }"
        ],
    );

    check(
        &run_parser!(expr(), r#"t.0 .0"#),
        expect_test::expect![[
            r#"TupleIndex { tuple: TupleIndex { tuple: Ident(Ident("t")), index: 0 }, index: 0 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"foo().0"#),
        expect_test::expect![[
            r#"TupleIndex { tuple: Call { name: Ident("foo"), args: [] }, index: 0 }"#
        ]],
    );

    check(
        &run_parser!(expr(), r#"{ (0, 0) }.0"#),
        expect_test::expect!["TupleIndex { tuple: Block(Block { statements: [], final_expr: Tuple([Immediate(Int(0)), Immediate(Int(0))]) }), index: 0 }"],
    );

    check(
        &run_parser!(expr(), r#"if true { (0, 0) } else { (0, 0) }.0"#),
        expect_test::expect!["TupleIndex { tuple: If(IfExpr { condition: Immediate(Bool(true)), then_block: Block { statements: [], final_expr: Tuple([Immediate(Int(0)), Immediate(Int(0))]) }, else_block: Block { statements: [], final_expr: Tuple([Immediate(Int(0)), Immediate(Int(0))]) } }), index: 0 }"],
    );

    // This parses because `1 + 2` is an expression, but it should fail in semantic analysis.
    check(
        &run_parser!(expr(), "1 + 2 .3"),
        expect_test::expect!["BinaryOp { op: Add, lhs: Immediate(Int(1)), rhs: TupleIndex { tuple: Immediate(Int(2)), index: 3 } }"],
    );

    check(
        &run_parser!(let_decl(expr()), "let x = t.0xa;"),
        expect_test::expect![[r#"
            @10..13: Invalid integer value "0xa" for tuple index
        "#]],
    );

    check(
        &run_parser!(let_decl(expr()), "let x = t.xx;"),
        expect_test::expect![[r#"
            @10..12: Invalid value "xx" for tuple index
        "#]],
    );
}

#[test]
fn basic_program() {
    let src = r#"
var low_val: real = 1.23;
let high_val = 4.56;        // Implicit type.

// Here's the constraints.
constraint mid > low_val * 2.0;
constraint mid < high_val;

solve minimize mid;
"#;

    check(
        &run_parser!(yurt_program(), src),
        expect_test::expect![[
            r#"[Var(VarStatement { name: Ident("low_val"), ty: Some(Real), init: Some(Immediate(Real(1.23))) }), Let(LetStatement { name: Ident("high_val"), ty: None, init: Immediate(Real(4.56)) }), Constraint(BinaryOp { op: GreaterThan, lhs: Ident(Ident("mid")), rhs: BinaryOp { op: Mul, lhs: Ident(Ident("low_val")), rhs: Immediate(Real(2.0)) } }), Constraint(BinaryOp { op: LessThan, lhs: Ident(Ident("mid")), rhs: Ident(Ident("high_val")) }), Solve(Minimize(Ident("mid")))]"#
        ]],
    );
}

#[test]
fn with_errors() {
    check(
        &run_parser!(yurt_program(), "let low_val: bad = 1.23"),
        expect_test::expect![[r#"
            @13..16: found "bad" but expected "(", "real", "int", "bool",  or "string"
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
            @18..19: found "}" but expected "!", "+", "-", "{", "(", "if", "var", "let",  or "constraint"
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
            r#"[Solve(Maximize(Ident("low"))), Constraint(BinaryOp { op: LessThan, lhs: Ident(Ident("low")), rhs: Ident(Ident("high")) }), Let(LetStatement { name: Ident("high"), ty: None, init: Immediate(Real(2.0)) }), Solve(Satisfy), Let(LetStatement { name: Ident("low"), ty: None, init: Immediate(Real(1.0)) })]"#
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
            r#"Ok([Let(LetStatement { name: Ident("x"), ty: None, init: Immediate(Int(5)) })])"#
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
            r#"Let(LetStatement { name: Ident("blah"), ty: None, init: Immediate(BigInt(1234567890123456789012345678901234567890)) })"#
        ]],
    );
    check(
        &run_parser!(
            let_decl(expr()),
            "let blah = 0xfeedbadf00d2adeadcafed00dbabeface;"
        ),
        // Confirmed by using the Python REPL to convert from hex to dec...
        expect_test::expect![[
            r#"Let(LetStatement { name: Ident("blah"), ty: None, init: Immediate(BigInt(5421732407698601623698172315373246806734)) })"#
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
