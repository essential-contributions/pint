use crate::{
    error::{Error, ReportableError},
    intent::intermediate::{DisplayWithII, ExprKey, IntermediateIntent, VarKey},
    lexer::{self, KEYWORDS},
    parser::ParserContext,
    span::Span,
};

use std::{fmt::Write, path::Path, rc::Rc};

#[cfg(test)]
use yurt_parser as yp;

#[cfg(test)]
use lalrpop_util::lalrpop_mod;

#[cfg(test)]
lalrpop_mod!(#[allow(unused)] pub yurt_parser);

#[cfg(test)]
macro_rules! parse_and_collect {
    ($parser: expr, $source: expr, $filepath: expr, $context: expr) => {{
        let mut errors = Vec::new();

        match $parser.parse(
            &mut $context,
            &mut errors,
            lexer::Lexer::new($source, &$filepath),
        ) {
            Ok(result) => {
                if errors.is_empty() {
                    Ok(result)
                } else {
                    Err(errors)
                }
            }
            Err(lalrpop_err) => {
                errors.push(Error::Parse {
                    error: (lalrpop_err, &$filepath).into(),
                });
                Err(errors)
            }
        }
    }};
}

#[cfg(test)]
macro_rules! run_parser {
    ($parser: expr, $source: expr) => {{
        let filepath = Rc::from(Path::new(""));
        let mut context = ParserContext {
            mod_path: &[],
            mod_prefix: "",
            ii: &mut IntermediateIntent::default(),
            span_from: &|l, r| Span::new(Rc::clone(&filepath), l..r),
            use_paths: &mut vec![],
            next_paths: &mut vec![],
        };
        let result = parse_and_collect!($parser, $source, filepath, context);
        match result {
            Ok(item) => format!("{}", context.ii.with_ii(&item)),
            Err(errors) => display_errors(&errors),
        }
    }};
}

// LetDecl returns a (VarKey, Option<ExprKey>) which we need to print.
#[cfg(test)]
impl DisplayWithII for (VarKey, Option<ExprKey>) {
    fn fmt(&self, f: &mut std::fmt::Formatter, ii: &IntermediateIntent) -> std::fmt::Result {
        write!(
            f,
            "{} = {}",
            ii.with_ii(&self.0),
            self.1
                .map(|k| ii.with_ii(k).to_string())
                .unwrap_or(String::from("NONE"))
        )
    }
}

// StateDecl returns a (&'static str, usize) which we need to print.
#[cfg(test)]
impl DisplayWithII for (&'static str, usize) {
    fn fmt(&self, f: &mut std::fmt::Formatter, ii: &IntermediateIntent) -> std::fmt::Result {
        assert_eq!(self.0, "state_idx");
        write!(f, "{}", ii.with_ii(&ii.states[self.1]),)
    }
}

#[cfg(test)]
fn display_errors(errors: &[Error]) -> String {
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
}

#[cfg(test)]
fn check(actual: &str, expect: expect_test::Expect) {
    expect.assert_eq(actual);
}

#[test]
fn types() {
    let type_ = yp::TypeParser::new();

    check(&run_parser!(type_, "int"), expect_test::expect!["int"]);
    check(&run_parser!(type_, "real"), expect_test::expect!["real"]);
    check(&run_parser!(type_, "bool"), expect_test::expect!["bool"]);
    check(
        &run_parser!(type_, "string"),
        expect_test::expect!["string"],
    );
    check(
        &run_parser!(type_, "{int, real, string}"),
        expect_test::expect!["{int, real, string}"],
    );
    check(
        &run_parser!(type_, "{int, {real, int}, string}"),
        expect_test::expect!["{int, {real, int}, string}"],
    );
    check(
        &run_parser!(type_, "{int, }"),
        expect_test::expect!["{int}"],
    );
    check(&run_parser!(type_, "{int}"), expect_test::expect!["{int}"]);
    check(
        &run_parser!(type_, "{}"),
        expect_test::expect![[r#"
            empty tuple types are not allowed
            @0..2: empty tuple type found
        "#]],
    );
    check(
        &run_parser!(type_, "MyType"),
        expect_test::expect!["MyType"],
    );
    check(
        &run_parser!(type_, "A::B::C::MyType"),
        expect_test::expect!["A::B::C::MyType"],
    );
    check(
        &run_parser!(type_, "::A::B::C::MyType"),
        expect_test::expect!["::A::B::C::MyType"],
    );
}

#[test]
fn immediates() {
    let immediate = yp::ImmediateParser::new();

    check(&run_parser!(immediate, "0x88"), expect_test::expect!["136"]);
    check(&run_parser!(immediate, "0b111"), expect_test::expect!["7"]);
    check(&run_parser!(immediate, "1"), expect_test::expect!["1"]);
    check(
        &run_parser!(immediate, "0x4f3f4f3f4f3f4f3f4f3f4f3f4f"),
        expect_test::expect!["6278618198356320102092284837711"],
    );
    check(
            &run_parser!(immediate, "0b1000000000000000000000000000000000000000000000000000000000000000000000000000000000000"),
            expect_test::expect!["19342813113834066795298816"],
        );
    check(
        &run_parser!(immediate, "9223372036854775808"),
        expect_test::expect!["9223372036854775808"],
    );
    check(
        &run_parser!(immediate, "1.3"),
        expect_test::expect!["1.3e0"],
    );
}

#[test]
fn use_statements() {
    let yurt = yp::YurtParser::new();

    let run_use_parser = |src_str| {
        let mut use_paths = Vec::new();
        let filepath = Rc::from(Path::new("test"));
        let mut context = ParserContext {
            mod_path: &["foo".to_string()],
            mod_prefix: "::foo::",
            ii: &mut IntermediateIntent::default(),
            span_from: &|_, _| crate::span::empty_span(),
            use_paths: &mut use_paths,
            next_paths: &mut vec![],
        };

        match parse_and_collect!(yurt, src_str, filepath, context) {
            Ok(_) => use_paths
                .into_iter()
                .map(|up| up.to_string())
                .collect::<Vec<_>>()
                .join(", "),
            Err(errors) => display_errors(&errors),
        }
    };

    check(
        &run_use_parser("use {}; use ::{};"),
        expect_test::expect![""],
    );

    check(
        &run_use_parser("use a; use ::b; use ::c as d;"),
        expect_test::expect!["foo::a, b, c as d"],
    );

    check(
        &run_use_parser("use a::b; use ::b::c; use ::c::d as c;"),
        expect_test::expect!["foo::a::b, b::c, c::d as c"],
    );

    check(
        &run_use_parser("use a::{b, c as d};"),
        expect_test::expect!["foo::a::b, foo::a::c as d"],
    );

    check(
        &run_use_parser("use a::{{c as d, { e as f }}};"),
        expect_test::expect!["foo::a::c as d, foo::a::e as f"],
    );

    // Errors - TODO: improve these
    check(
        &run_use_parser("use ;"),
        expect_test::expect![[r#"
            expected `::`, `ident`, or `{`, found `;`
            @4..5: expected `::`, `ident`, or `{`
        "#]],
    );

    check(
        &run_use_parser("use ::;"),
        expect_test::expect![[r#"
            expected `ident`, or `{`, found `;`
            @6..7: expected `ident`, or `{`
        "#]],
    );

    check(
        &run_use_parser("use a::;"),
        expect_test::expect![[r#"
            expected `ident`, or `{`, found `;`
            @7..8: expected `ident`, or `{`
        "#]],
    );
}

#[test]
fn let_decls() {
    let letp = yp::LetDeclParser::new();
    let filepath = Rc::from(Path::new("test"));

    // We're re-using this context for each test, rather than building a new one each time.  This
    // is a) more efficient and b) dangerous -- we must be careful to reset it properly each time.
    let mut ctx = ParserContext {
        mod_path: &["foo".to_string()],
        mod_prefix: "::foo::",
        ii: &mut IntermediateIntent::default(),
        span_from: &|l, r| Span::new(Rc::clone(&filepath), l..r),
        use_paths: &mut Vec::new(),
        next_paths: &mut Vec::new(),
    };

    let mut run_let_parser = |src_str| {
        let out_str = match parse_and_collect!(letp, src_str, filepath, ctx) {
            Ok(res) => {
                let constraints_str = ctx
                    .ii
                    .constraints
                    .iter()
                    .map(|(expr_key, _)| format!("constraint {}", ctx.ii.with_ii(expr_key)))
                    .collect::<Vec<_>>()
                    .join("\n");

                format!("{}\n{}\n", ctx.ii.with_ii(res), constraints_str)
            }
            Err(errors) => display_errors(&errors),
        };

        *ctx.ii = IntermediateIntent::default();
        ctx.use_paths.clear();
        ctx.next_paths.clear();

        out_str
    };

    check(
        &run_let_parser("let blah"),
        expect_test::expect![[r#"
            type annotation or initializer needed for variable `blah`
            @0..8: type annotation or initializer needed
            consider giving `blah` an explicit type or an initializer"#]],
    );
    check(
        &run_let_parser("let blah = 1.0"),
        expect_test::expect![[r#"
            var ::foo::blah = 1e0
            constraint (var ::foo::blah == 1e0)
        "#]],
    );
    check(
        &run_let_parser("let blah: real = 1.0"),
        expect_test::expect![[r#"
            var ::foo::blah: real = 1e0
            constraint (var ::foo::blah: real == 1e0)
        "#]],
    );
    check(
        &run_let_parser("let blah: real"),
        expect_test::expect![[r#"
            var ::foo::blah: real = NONE

        "#]],
    );
    check(
        &run_let_parser("let blah = 1"),
        expect_test::expect![[r#"
            var ::foo::blah = 1
            constraint (var ::foo::blah == 1)
        "#]],
    );
    check(
        &run_let_parser("let blah: int = 1"),
        expect_test::expect![[r#"
            var ::foo::blah: int = 1
            constraint (var ::foo::blah: int == 1)
        "#]],
    );
    check(
        &run_let_parser("let blah: int"),
        expect_test::expect![[r#"
            var ::foo::blah: int = NONE

        "#]],
    );
    check(
        &run_let_parser("let blah = true"),
        expect_test::expect![[r#"
            var ::foo::blah = true
            constraint (var ::foo::blah == true)
        "#]],
    );
    check(
        &run_let_parser("let blah: bool = false"),
        expect_test::expect![[r#"
            var ::foo::blah: bool = false
            constraint (var ::foo::blah: bool == false)
        "#]],
    );
    check(
        &run_let_parser("let blah: bool"),
        expect_test::expect![[r#"
            var ::foo::blah: bool = NONE

        "#]],
    );
    check(
        &run_let_parser(r#"let blah = "hello""#),
        expect_test::expect![[r#"
            var ::foo::blah = "hello"
            constraint (var ::foo::blah == "hello")
        "#]],
    );
    check(
        &run_let_parser(r#"let blah: string = "hello""#),
        expect_test::expect![[r#"
            var ::foo::blah: string = "hello"
            constraint (var ::foo::blah: string == "hello")
        "#]],
    );
    check(
        &run_let_parser(r#"let blah: string"#),
        expect_test::expect![[r#"
            var ::foo::blah: string = NONE

        "#]],
    );
}

#[test]
fn state_decls() {
    let state = yp::StateDeclParser::new();

    check(
        &run_parser!(state, "state x: int = MyContract::foo()"),
        expect_test::expect!["state x: int = MyContract::foo()"],
    );
    check(
        &run_parser!(state, "state y = MyContract::bar()"),
        expect_test::expect!["state y = MyContract::bar()"],
    );
}

#[test]
fn constraint_decls() {
    // Argument just needs to be any expression, as far as the parser is concerned.
    let constraint_decl = yp::ConstraintDeclParser::new();

    check(
        &run_parser!(constraint_decl, "constraint blah"),
        expect_test::expect!["blah"],
    );
}

#[test]
fn solve_decls() {
    let solve_decl = yp::SolveDeclParser::new();

    check(
        &run_parser!(solve_decl, "solve satisfy"),
        expect_test::expect!["solve satisfy"],
    );
    check(
        &run_parser!(solve_decl, "solve minimize foo"),
        expect_test::expect!["solve minimize foo"],
    );
    check(
        &run_parser!(solve_decl, "solve maximize foo"),
        expect_test::expect!["solve maximize foo"],
    );

    check(
        &run_parser!(solve_decl, "solve maximize x + y"),
        expect_test::expect!["solve maximize (x + y)"],
    );

    check(
        &run_parser!(solve_decl, "solve world hunger"),
        expect_test::expect![[r#"
            expected `maximize`, `minimize`, or `satisfy`, found `world`
            @6..11: expected `maximize`, `minimize`, or `satisfy`
        "#]],
    );
}

#[test]
fn basic_exprs() {
    let expr = yp::ExprParser::new();
    check(&run_parser!(expr, "123"), expect_test::expect!["123"]);
    check(&run_parser!(expr, "foo"), expect_test::expect!["foo"]);
}

#[test]
fn unary_op_exprs() {
    let expr = yp::ExprParser::new();

    check(&run_parser!(expr, "!a"), expect_test::expect!["!a"]);
    check(&run_parser!(expr, "+a"), expect_test::expect!["+a"]);
    check(&run_parser!(expr, "-a"), expect_test::expect!["-a"]);
    check(&run_parser!(expr, "+7"), expect_test::expect!["+7"]);
    check(&run_parser!(expr, "+3.4"), expect_test::expect!["+3.4e0"]);
    check(&run_parser!(expr, "+0x456"), expect_test::expect!["+1110"]);
    check(
        &run_parser!(expr, "+0b01010101"),
        expect_test::expect!["+85"],
    );
    check(
        &run_parser!(
            expr,
            "+0b1101000000001100101010101010101111111111101010101101010101010101"
        ),
        expect_test::expect!["+14991544915315053909"],
    );
    check(&run_parser!(expr, "-1.0"), expect_test::expect!["-1e0"]);
    check(&run_parser!(expr, "-1"), expect_test::expect!["-1"]);
    check(&run_parser!(expr, "-0x133"), expect_test::expect!["-307"]);
    check(&run_parser!(expr, "-0b1101"), expect_test::expect!["-13"]);
    check(
        &run_parser!(
            expr,
            "-0b1101000000001100101010101010101010101010101010101101010101010101"
        ),
        expect_test::expect!["-14991544909594023253"],
    );
    check(
        &run_parser!(expr, "! - - !  -+  -1"),
        expect_test::expect!["!--!-+-1"],
    );
    check(
        &run_parser!(expr, "+ {- x} '  '  "),
        expect_test::expect!["+-x''"],
    );
}

#[test]
fn binary_op_exprs() {
    let expr = yp::ExprParser::new();

    check(
        &run_parser!(expr, "a * 2.0"),
        expect_test::expect!["(a * 2e0)"],
    );
    check(
        &run_parser!(expr, "a / 2.0"),
        expect_test::expect!["(a / 2e0)"],
    );
    check(
        &run_parser!(expr, "a % 2.0"),
        expect_test::expect!["(a % 2e0)"],
    );
    check(
        &run_parser!(expr, "a + 2.0"),
        expect_test::expect!["(a + 2e0)"],
    );
    check(
        &run_parser!(expr, "a - 2.0"),
        expect_test::expect!["(a - 2e0)"],
    );
    check(
        &run_parser!(expr, "a+2.0"),
        expect_test::expect!["(a + 2e0)"],
    );
    check(
        &run_parser!(expr, "a-2.0"),
        expect_test::expect!["(a - 2e0)"],
    );
    check(
        &run_parser!(expr, "a < 2.0"),
        expect_test::expect!["(a < 2e0)"],
    );
    check(
        &run_parser!(expr, "a > 2.0"),
        expect_test::expect!["(a > 2e0)"],
    );
    check(
        &run_parser!(expr, "a <= 2.0"),
        expect_test::expect!["(a <= 2e0)"],
    );
    check(
        &run_parser!(expr, "a >= 2.0"),
        expect_test::expect!["(a >= 2e0)"],
    );
    check(
        &run_parser!(expr, "a == 2.0"),
        expect_test::expect!["(a == 2e0)"],
    );
    check(
        &run_parser!(expr, "a != 2.0"),
        expect_test::expect!["(a != 2e0)"],
    );
    check(
        &run_parser!(expr, "a && b"),
        expect_test::expect!["(a && b)"],
    );

    check(
        &run_parser!(expr, "a || b"),
        expect_test::expect!["(a || b)"],
    );

    check(
        &run_parser!(expr, "a || b && c || d && !e"),
        expect_test::expect!["((a || (b && c)) || (d && !e))"],
    );
}

#[test]
fn complex_exprs() {
    let expr = yp::ExprParser::new();

    check(
        &run_parser!(expr, "2 * b * 3"),
        expect_test::expect!["((2 * b) * 3)"],
    );
    check(
        &run_parser!(expr, "2 < b * 3"),
        expect_test::expect!["(2 < (b * 3))"],
    );
    check(
        &run_parser!(expr, "2.0 > b * 3.0"),
        expect_test::expect!["(2e0 > (b * 3e0))"],
    );
    check(
        &run_parser!(expr, "2.0 * b < 3.0"),
        expect_test::expect!["((2e0 * b) < 3e0)"],
    );
    check(
        &run_parser!(expr, "2 > b < 3"),
        expect_test::expect!["((2 > b) < 3)"],
    );
    check(
        &run_parser!(expr, "2 != b < 3"),
        expect_test::expect!["((2 != b) < 3)"],
    );
    check(
        &run_parser!(expr, "2 < b != 3"),
        expect_test::expect!["((2 < b) != 3)"],
    );
    check(
        &run_parser!(expr, "a > b * c < d"),
        expect_test::expect!["((a > (b * c)) < d)"],
    );
    check(
        &run_parser!(expr, "2 + 3 * 4"),
        expect_test::expect!["(2 + (3 * 4))"],
    );
    check(
        &run_parser!(expr, "10 - 8 / 4"),
        expect_test::expect!["(10 - (8 / 4))"],
    );
    check(
        &run_parser!(expr, "10 + 8 % 4"),
        expect_test::expect!["(10 + (8 % 4))"],
    );
    check(
        &run_parser!(expr, "2 + 3 * 4 < 5"),
        expect_test::expect!["((2 + (3 * 4)) < 5)"],
    );
    check(
        &run_parser!(expr, "2 * 3 / 4 < 5"),
        expect_test::expect!["(((2 * 3) / 4) < 5)"],
    );
    check(
        &run_parser!(expr, "10 - 5 + 3 > 7"),
        expect_test::expect!["(((10 - 5) + 3) > 7)"],
    );
    check(
        &run_parser!(expr, "10 % 2 * 4 < 3"),
        expect_test::expect!["(((10 % 2) * 4) < 3)"],
    );
    check(
        &run_parser!(expr, "2 + 3 * 4 - 5 / 2 > 1"),
        expect_test::expect!["(((2 + (3 * 4)) - (5 / 2)) > 1)"],
    );
}

#[test]
fn parens_exprs() {
    let expr = yp::ExprParser::new();

    check(
        &run_parser!(expr, "(1 + 2) * 3"),
        expect_test::expect!["((1 + 2) * 3)"],
    );
    check(
        &run_parser!(expr, "1 * (2 + 3)"),
        expect_test::expect!["(1 * (2 + 3))"],
    );
    check(
        &run_parser!(expr, "(1 + 2) * (3 + 4)"),
        expect_test::expect!["((1 + 2) * (3 + 4))"],
    );
    check(
        &run_parser!(expr, "(1 + (2 * 3)) * 4"),
        expect_test::expect!["((1 + (2 * 3)) * 4)"],
    );
    check(
        &run_parser!(expr, "(1 * (2 + 3)) * 4"),
        expect_test::expect!["((1 * (2 + 3)) * 4)"],
    );
    check(
        &run_parser!(expr, "((1 + 2) * 3) * 4"),
        expect_test::expect!["(((1 + 2) * 3) * 4)"],
    );
    check(
        &run_parser!(expr, "((1 + 2) * (3 + 4)) * 5"),
        expect_test::expect!["(((1 + 2) * (3 + 4)) * 5)"],
    );
    check(
        &run_parser!(expr, "(1 + 2) * 3 / 4"),
        expect_test::expect!["(((1 + 2) * 3) / 4)"],
    );
    check(
        &run_parser!(expr, "1 / (2 + 3) * 4"),
        expect_test::expect!["((1 / (2 + 3)) * 4)"],
    );
    check(
        &run_parser!(expr, "(1 < 2) && (3 > 4)"),
        expect_test::expect!["((1 < 2) && (3 > 4))"],
    );
    check(
        &run_parser!(expr, "(1 == 2) || (3 != 4)"),
        expect_test::expect!["((1 == 2) || (3 != 4))"],
    );
    check(
        &run_parser!(expr, "1 < (2 && 3) > 4"),
        expect_test::expect!["((1 < (2 && 3)) > 4)"],
    );
    check(
        &run_parser!(expr, "1 && (2 || 3)"),
        expect_test::expect!["(1 && (2 || 3))"],
    );
    check(
        &run_parser!(expr, "1 == (2 || 3) != 4"),
        expect_test::expect!["((1 == (2 || 3)) != 4)"],
    );
    check(
        &run_parser!(expr, "-(1 + 2)"),
        expect_test::expect!["-(1 + 2)"],
    );
    check(
        &run_parser!(expr, "!(a < b)"),
        expect_test::expect!["!(a < b)"],
    );
    check(&run_parser!(expr, "(1)"), expect_test::expect!["1"]);
    check(&run_parser!(expr, "(a)"), expect_test::expect!["a"]);
    check(
        &run_parser!(expr, "()"),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `false`, `ident`, `if`, `int_lit`, `real_lit`, `str_lit`, `true`, or `{`, found `)`
            @1..2: expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `false`, `ident`, `if`, `int_lit`, `real_lit`, `str_lit`, `true`, or `{`
        "#]],
    );

    check(
        &run_parser!(expr, "(if a < b { 1 } else { 2 })"),
        expect_test::expect!["if (a < b) { 1 } else { 2 }"],
    );
    check(
        &run_parser!(expr, "(foo(a, b, c))"),
        expect_test::expect!["foo(a, b, c)"],
    );
}

#[test]
fn enums() {
    let enum_decl = yp::EnumDeclParser::new();
    let expr = yp::ExprParser::new();
    let let_decl = yp::LetDeclParser::new();

    check(
        &run_parser!(enum_decl, "enum MyEnum = Variant1 | Variant2"),
        expect_test::expect!["enum MyEnum = Variant1 | Variant2"],
    );
    check(
        &run_parser!(enum_decl, "enum MyEnum = Variant1"),
        expect_test::expect!["enum MyEnum = Variant1"],
    );
    check(
        &run_parser!(expr, "MyEnum::Variant1"),
        expect_test::expect!["MyEnum::Variant1"],
    );
    check(
        &run_parser!(
            let_decl,
            r#"
                let x = MyEnum::Variant3
                "#
        ),
        expect_test::expect!["var x = MyEnum::Variant3"],
    );
    check(
        &run_parser!(
            let_decl,
            r#"
                let e: ::path::to::MyEnum
                "#
        ),
        expect_test::expect!["var e: ::path::to::MyEnum = NONE"],
    );
}

#[test]
fn custom_types() {
    let type_ = yp::TypeParser::new();
    let type_decl = yp::NewTypeDeclParser::new();

    check(
        &run_parser!(type_, "custom_type"),
        expect_test::expect!["custom_type"],
    );
    check(
        &run_parser!(type_decl, "type MyInt = int"),
        expect_test::expect!["type MyInt = int"],
    );
    check(
        &run_parser!(type_decl, "type MyReal = real"),
        expect_test::expect!["type MyReal = real"],
    );
    check(
        &run_parser!(type_decl, "type MyBool = bool"),
        expect_test::expect!["type MyBool = bool"],
    );
    check(
        &run_parser!(type_decl, "type MyString = string"),
        expect_test::expect!["type MyString = string"],
    );
    check(
        &run_parser!(type_decl, "type IntArray = int[5]"),
        expect_test::expect!["type IntArray = int[5]"],
    );
    check(
        &run_parser!(type_decl, "type MyTuple = { int, real, z: string }"),
        expect_test::expect!["type MyTuple = {int, real, z: string}"],
    );
    check(
        &run_parser!(type_decl, "type MyAliasInt = MyInt"),
        expect_test::expect!["type MyAliasInt = MyInt"],
    );
}

#[test]
fn ranges() {
    let range = yp::RangeParser::new();
    let let_decl = yp::LetDeclParser::new();
    let expr = yp::ExprParser::new();

    check(&run_parser!(range, "1..2"), expect_test::expect!["1..2"]);
    check(
        &run_parser!(range, "1.1..2.2e3"),
        expect_test::expect!["1.1e0..2.2e3"],
    );
    check(
        &run_parser!(range, "A[x]..t.2"),
        expect_test::expect!["A[x]..t.2"],
    );
    check(
        &run_parser!(range, "1+2..3+4"),
        expect_test::expect!["(1 + 2)..(3 + 4)"],
    );
    check(
        &run_parser!(range, "-100.. -if c { 10 } else { 9 }"),
        expect_test::expect!["-100..-if c { 10 } else { 9 }"],
    );
    check(
        &run_parser!(range, "1...2"),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `false`, `ident`, `if`, `int_lit`, `real_lit`, `str_lit`, `true`, or `{`, found `.`
            @3..4: expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `false`, `ident`, `if`, `int_lit`, `real_lit`, `str_lit`, `true`, or `{`
        "#]],
    );

    // Range allow in let decls
    check(
        &run_parser!(let_decl, "let x = 1..2"),
        expect_test::expect!["var x = 1..2"],
    );

    // Ranges allowed after `in`
    check(
        &run_parser!(expr, "x in 1..2"),
        expect_test::expect![[r#"x in 1..2"#]],
    );

    // Ranges not allowed in binary ops
    check(
        &run_parser!(expr, "(1..2) + 3"),
        expect_test::expect![[r#"
            expected `!=`, `&&`, `)`, `+`, `-`, `<`, `<=`, `==`, `>`, `>=`, `in`, or `||`, found `..`
            @2..4: expected `!=`, `&&`, `)`, `+`, `-`, `<`, `<=`, `==`, `>`, `>=`, `in`, or `||`
        "#]],
    );
}

#[test]
fn idents() {
    let ident = yp::IdentParser::new();

    check(
        &run_parser!(ident, "foobar"),
        expect_test::expect!["foobar"],
    );
    check(
        &run_parser!(ident, "foo_bar"),
        expect_test::expect!["foo_bar"],
    );
    check(
        &run_parser!(ident, "FOO_bar"),
        expect_test::expect!["FOO_bar"],
    );
    check(&run_parser!(ident, "__FOO"), expect_test::expect!["__FOO"]);
    check(
        &run_parser!(ident, "_2_FOO1"),
        expect_test::expect!["_2_FOO1"],
    );
    check(&run_parser!(ident, "_"), expect_test::expect!["_"]);
    check(
        &run_parser!(ident, "12_ab"),
        expect_test::expect![[r#"
            expected `ident`, found `12`
            @0..2: expected `ident`
        "#]],
    );
    check(
        // Lexer will split this into 3 tokens, ident will parse the first one.
        // This shows that we're not able to parser `ab*cd` as a single identifier
        &run_parser!(ident, "ab*cd"),
        expect_test::expect![[r#"
            expected something else, found `*`
            @2..3: expected something else
        "#]],
    );
}

#[test]
fn paths() {
    let path = yp::PathParser::new();

    check(
        &run_parser!(path, "foo::bar"),
        expect_test::expect!["foo::bar"],
    );
    check(
        &run_parser!(path, "_foo_::_bar"),
        expect_test::expect!["_foo_::_bar"],
    );
    check(&run_parser!(path, "_::_"), expect_test::expect!["_::_"]);
    check(
        &run_parser!(path, "t2::_3t::t4_::t"),
        expect_test::expect!["t2::_3t::t4_::t"],
    );
    check(
        &run_parser!(path, "::foo::bar"),
        expect_test::expect!["::foo::bar"],
    );

    // As long as these two produce an error... it should be expecting 'ident'.
    check(
        &run_parser!(path, "foo::"),
        expect_test::expect![[r#"
            expected `ident`, found `end of file`
            @5..5: expected `ident`
        "#]],
    );
    check(
        &run_parser!(path, "::foo::"),
        expect_test::expect![[r#"
            expected `ident`, found `end of file`
            @7..7: expected `ident`
        "#]],
    );
}

//#[test]
//fn fn_decl_test() {
//    let src = r#"
//fn foo(x: real, y: real) -> real {
//    let z = 5.0;
//    z
//}
//"#;
//
//    check(
//        &run_parser!(yurt_program(), src),
//        expect_test::expect!["fn foo(x: real, y: real) -> real { let z = 5e0; z };"],
//    );
//}

#[test]
fn fn_call() {
    check(
        &run_parser!(yp::LetDeclParser::new(), r#"let x = foo(a*3, c)"#),
        expect_test::expect!["var x = foo((a * 3), c)"],
    );

    check(
        &run_parser!(yp::ExprParser::new(), "A::B::foo(-a, b+c)"),
        expect_test::expect!["A::B::foo(-a, (b + c))"],
    );
}

#[test]
fn code_blocks() {
    let expr = yp::ExprParser::new();
    let filepath = Rc::from(Path::new("test"));

    // We're re-using this context for each test, rather than building a new one each time.  This
    // is a) more efficient and b) dangerous -- we must be careful to reset it properly each time.
    let mut ctx = ParserContext {
        mod_path: &["foo".to_string()],
        mod_prefix: "::foo::",
        ii: &mut IntermediateIntent::default(),
        span_from: &|l, r| Span::new(Rc::clone(&filepath), l..r),
        use_paths: &mut Vec::new(),
        next_paths: &mut Vec::new(),
    };

    let mut run_block_parser = |src_str| {
        let out_str = match parse_and_collect!(expr, src_str, filepath, ctx) {
            Ok(res) => {
                let var_str = ctx
                    .ii
                    .vars
                    .iter()
                    .map(|(var_key, _)| format!("{}; ", ctx.ii.with_ii(var_key)))
                    .collect::<Vec<_>>()
                    .join("");

                let constraints_str = ctx
                    .ii
                    .constraints
                    .iter()
                    .map(|(expr_key, _)| format!("constraint {}; ", ctx.ii.with_ii(expr_key)))
                    .collect::<Vec<_>>()
                    .join("");

                format!(
                    "{{ {}{}{} }}\n",
                    var_str,
                    constraints_str,
                    ctx.ii.with_ii(res)
                )
            }
            Err(errors) => display_errors(&errors),
        };

        *ctx.ii = IntermediateIntent::default();
        ctx.use_paths.clear();
        ctx.next_paths.clear();

        out_str
    };

    check(
        &run_block_parser("{ 0 }"),
        expect_test::expect![[r#"
            { 0 }
        "#]],
    );

    check(
        &run_block_parser("{ let y: real = 0; constraint x > 0.0; 0.0 }"),
        expect_test::expect![[r#"
            { var ::foo::y: real; constraint (var ::foo::y: real == 0); constraint (::foo::x > 0e0); 0e0 }
        "#]],
    );

    check(
        &run_block_parser("{ constraint { true }; x > 0 }"),
        expect_test::expect![[r#"
            { constraint true; (::foo::x > 0) }
        "#]],
    );

    check(
        &run_block_parser("{}"),
        expect_test::expect![[r#"
            empty tuple expressions are not allowed
            @0..2: empty tuple expression found
        "#]],
    );
}

#[test]
fn if_exprs() {
    let expr = yp::ExprParser::new();

    check(
        &run_parser!(expr, "if c { 1 }"),
        expect_test::expect![[r#"
            expected `else`, found `end of file`
            @10..10: expected `else`
        "#]],
    );

    check(
        &run_parser!(expr, "if c { 1 } else { 0 }"),
        expect_test::expect!["if c { 1 } else { 0 }"],
    );

    check(
        &run_parser!(expr, "if c { if c { 1 } else { 0 } } else { 2 }"),
        expect_test::expect!["if c { if c { 1 } else { 0 } } else { 2 }"],
    );
}

#[test]
fn array_type() {
    let type_ = yp::TypeParser::new();

    check(
        &run_parser!(type_, r#"int[5]"#),
        expect_test::expect!["int[5]"],
    );

    check(
        &run_parser!(type_, r#"int[MyEnum]"#),
        expect_test::expect!["int[MyEnum]"],
    );

    check(
        &run_parser!(type_, r#"int[N]"#),
        expect_test::expect!["int[N]"],
    );

    check(
        &run_parser!(type_, r#"string[foo()][{ 7 }][if true { 1 } else { 2 }]"#),
        expect_test::expect!["string[if true { 1 } else { 2 }][7][foo()]"],
    );

    check(
        &run_parser!(type_, r#"real[N][9][M][3]"#),
        expect_test::expect!["real[3][M][9][N]"],
    );

    check(
        &run_parser!(type_, r#"{int, { real, string }}[N][9]"#),
        expect_test::expect!["{int, {real, string}}[9][N]"],
    );

    check(
        &run_parser!(yp::LetDeclParser::new(), r#"let a: int[]"#),
        expect_test::expect![[r#"
            empty array types are not allowed
            @7..12: empty array type found
        "#]],
    );
}

#[test]
fn array_expressions() {
    let expr = yp::ExprParser::new();

    check(&run_parser!(expr, r#"[5]"#), expect_test::expect!["[5]"]);

    check(&run_parser!(expr, r#"[5,]"#), expect_test::expect!["[5]"]);

    check(
        &run_parser!(expr, r#"[5, 4]"#),
        expect_test::expect!["[5, 4]"],
    );

    check(
        &run_parser!(expr, r#"[[ 1 ],]"#),
        expect_test::expect!["[[1]]"],
    );

    check(
        &run_parser!(expr, r#"[[1, 2], 3]"#), // This should fail in semantic analysis
        expect_test::expect!["[[1, 2], 3]"],
    );

    check(
        &run_parser!(expr, r#"[[1, 2], [3, 4]]"#),
        expect_test::expect!["[[1, 2], [3, 4]]"],
    );

    check(
        &run_parser!(expr, r#"[[foo(), 2], [if true { 1 } else { 2 }, t.0]]"#),
        expect_test::expect!["[[foo(), 2], [if true { 1 } else { 2 }, t.0]]"],
    );
}

#[test]
fn array_element_accesses() {
    let expr = yp::ExprParser::new();

    check(&run_parser!(expr, r#"a[5]"#), expect_test::expect!["a[5]"]);

    check(
        &run_parser!(expr, r#"a[N][5][t.0]"#),
        expect_test::expect!["a[N][5][t.0]"],
    );

    check(
        &run_parser!(expr, r#"{ a }[N][foo()][M][4]"#),
        expect_test::expect!["a[N][foo()][M][4]"],
    );

    check(
        &run_parser!(expr, r#"foo()[{ M }][if true { 1 } else { 3 }]"#),
        expect_test::expect!["foo()[M][if true { 1 } else { 3 }]"],
    );

    check(
        &run_parser!(yp::LetDeclParser::new(), r#"let x = a[]"#),
        expect_test::expect![[r#"
            missing array index
            @8..11: missing array element index
        "#]],
    );

    check(
        &run_parser!(expr, r#"a[MyEnum::Variant1]"#),
        expect_test::expect!["a[MyEnum::Variant1]"],
    );
}

#[test]
fn tuple_expressions() {
    let expr = yp::ExprParser::new();

    check(
        &run_parser!(expr, r#"{0}"#), // This is not a tuple. It is a code block expr.
        expect_test::expect!["0"],
    );

    check(
        &run_parser!(expr, r#"{x: 0}"#), // This is a tuple because the field is named so there is no ambiguity
        expect_test::expect!["{x: 0}"],
    );

    check(
        &run_parser!(expr, r#"{0,}"#), // This is a tuple
        expect_test::expect!["{0}"],
    );

    check(
        &run_parser!(expr, r#"{x: 0,}"#), // This is a tuple
        expect_test::expect!["{x: 0}"],
    );

    check(
        &run_parser!(expr, r#"{0, 1.0, "foo"}"#),
        expect_test::expect![[r#"{0, 1e0, "foo"}"#]],
    );

    check(
        &run_parser!(expr, r#"{x: 0, y: 1.0, z: "foo"}"#),
        expect_test::expect![[r#"{x: 0, y: 1e0, z: "foo"}"#]],
    );

    check(
        &run_parser!(expr, r#"{0, {1.0, "bar"}, "foo"}"#),
        expect_test::expect![[r#"{0, {1e0, "bar"}, "foo"}"#]],
    );

    check(
        &run_parser!(expr, r#"{x: 0, {y: 1.0, "bar"}, z: "foo"}"#),
        expect_test::expect![[r#"{x: 0, {y: 1e0, "bar"}, z: "foo"}"#]],
    );

    check(
        &run_parser!(expr, r#"{ { 42 }, if c { 2 } else { 3 }, foo() }"#),
        expect_test::expect!["{42, if c { 2 } else { 3 }, foo()}"],
    );

    check(
        &run_parser!(expr, r#"{ x: { 42 }, y: if c { 2 } else { 3 }, z: foo() }"#),
        expect_test::expect!["{x: 42, y: if c { 2 } else { 3 }, z: foo()}"],
    );
}

#[test]
fn tuple_field_accesses() {
    let expr = yp::ExprParser::new();

    check(
        &run_parser!(expr, r#"t.0 + t.9999999 + t.x"#),
        expect_test::expect!["((t.0 + t.9999999) + t.x)"],
    );

    check(
        &run_parser!(expr, r#"t.1.1"#),
        expect_test::expect!["t.1.1"],
    );

    check(
        &run_parser!(expr, r#"{0, 1}.0"#),
        expect_test::expect!["{0, 1}.0"],
    );

    check(
        &run_parser!(expr, r#"{0, 1}.x"#),
        expect_test::expect!["{0, 1}.x"],
    );

    check(
        &run_parser!(expr, r#"t.0 .0"#),
        expect_test::expect!["t.0.0"],
    );

    check(
        &run_parser!(expr, r#"t.x .y"#),
        expect_test::expect!["t.x.y"],
    );

    check(
        &run_parser!(expr, "t \r .1 .2.2. \n 3 . \t 13 . 1.1"),
        expect_test::expect!["t.1.2.2.3.13.1.1"],
    );

    check(
        &run_parser!(expr, "t \r .x .1.2. \n w . \t t. 3.4"),
        expect_test::expect!["t.x.1.2.w.t.3.4"],
    );

    check(
        &run_parser!(expr, r#"foo().0.1"#),
        expect_test::expect!["foo().0.1"],
    );

    check(
        &run_parser!(expr, r#"foo().a.b.0.1"#),
        expect_test::expect!["foo().a.b.0.1"],
    );

    check(
        &run_parser!(expr, r#"{ {0, 0} }.0"#),
        expect_test::expect!["{0, 0}.0"],
    );

    check(
        &run_parser!(expr, r#"{ {0, 0} }.a"#),
        expect_test::expect!["{0, 0}.a"],
    );

    check(
        &run_parser!(expr, r#"if true { {0, 0} } else { {0, 0} }.0"#),
        expect_test::expect!["if true { {0, 0} } else { {0, 0} }.0"],
    );

    check(
        &run_parser!(expr, r#"if true { {0, 0} } else { {0, 0} }.x"#),
        expect_test::expect!["if true { {0, 0} } else { {0, 0} }.x"],
    );

    // This parses because `1 + 2` is an expression, but it should fail in semantic analysis.
    check(
        &run_parser!(expr, "1 + 2 .3"),
        expect_test::expect!["(1 + 2.3)"],
    );

    // This parses because `1 + 2` is an expression, but it should fail in semantic analysis.
    check(
        &run_parser!(expr, "1 + 2 .a"),
        expect_test::expect!["(1 + 2.a)"],
    );

    let let_decl = yp::LetDeclParser::new();

    check(
        &run_parser!(let_decl, "let x = t.0xa"),
        expect_test::expect![[r#"
                invalid integer `0xa` as tuple index
                @10..13: invalid integer as tuple index
            "#]],
    );

    check(
        &run_parser!(let_decl, "let x = t.111111111111111111111111111"),
        expect_test::expect![[r#"
                invalid integer `111111111111111111111111111` as tuple index
                @10..37: invalid integer as tuple index
            "#]],
    );

    check(
        &run_parser!(let_decl, "let x = t.111111111111111111111111111.2"),
        expect_test::expect![[r#"
                invalid integer `111111111111111111111111111` as tuple index
                @10..37: invalid integer as tuple index
            "#]],
    );

    check(
        &run_parser!(let_decl, "let x = t.2.111111111111111111111111111"),
        expect_test::expect![[r#"
                invalid integer `111111111111111111111111111` as tuple index
                @12..39: invalid integer as tuple index
            "#]],
    );

    check(
        &run_parser!(
            let_decl,
            "let x = t.222222222222222222222.111111111111111111111111111"
        ),
        expect_test::expect![[r#"
                invalid integer `222222222222222222222` as tuple index
                @10..31: invalid integer as tuple index
                invalid integer `111111111111111111111111111` as tuple index
                @32..59: invalid integer as tuple index
            "#]],
    );

    check(
        &run_parser!(let_decl, "let x = t.1e5"),
        expect_test::expect![[r#"
                invalid value `1e5` as tuple index
                @10..13: invalid value as tuple index
            "#]],
    );

    check(
        &run_parser!(let_decl, "let bad_tuple:{} = {}"),
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
    let expr = yp::ExprParser::new();

    check(
        &run_parser!(expr, r#"cond { else => a, }"#),
        expect_test::expect!["a"],
    );

    check(
        &run_parser!(expr, r#"cond { else => { a } }"#),
        expect_test::expect!["a"],
    );

    check(
        &run_parser!(expr, r#"cond { a => b, else => c }"#),
        expect_test::expect!["if a { b } else { c }"],
    );

    check(
        &run_parser!(expr, r#"cond { a => { b }, else => c, }"#),
        expect_test::expect!["if a { b } else { c }"],
    );

    check(
        &run_parser!(expr, r#"cond { a => b, { true } => d, else => f, }"#),
        expect_test::expect!["if a { b } else { if true { d } else { f } }"],
    );

    check(
        &run_parser!(expr, r#"cond { a => b, }"#),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `else`, `false`, `ident`, `if`, `int_lit`, `real_lit`, `str_lit`, `true`, or `{`, found `}`
            @15..16: expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `else`, `false`, `ident`, `if`, `int_lit`, `real_lit`, `str_lit`, `true`, or `{`
        "#]],
    );

    check(
        &run_parser!(expr, r#"cond { else => a, b => c }"#),
        expect_test::expect![[r#"
            expected `}`, found `b`
            @18..19: expected `}`
        "#]],
    );
}

#[test]
fn casting() {
    let expr = yp::ExprParser::new();
    let let_decl = yp::LetDeclParser::new();

    check(
        &run_parser!(expr, r#"(5 as int) as real as int"#),
        expect_test::expect!["5 as int as real as int"],
    );

    check(
        &run_parser!(expr, r#"t.0.1 as real * a[5][3] as int"#),
        expect_test::expect!["(t.0.1 as real * a[5][3] as int)"],
    );

    check(
        &run_parser!(let_decl, r#"let x = foo() as real as { int, real }"#),
        expect_test::expect!["var x = foo() as real as {int, real}"],
    );

    check(
        &run_parser!(let_decl, r#"let x = 5 as"#),
        expect_test::expect![[r#"
            expected `::`, `bool_ty`, `ident`, `int_ty`, `real_ty`, `string_ty`, or `{`, found `end of file`
            @12..12: expected `::`, `bool_ty`, `ident`, `int_ty`, `real_ty`, `string_ty`, or `{`
        "#]],
    );
}

#[test]
fn in_expr() {
    let expr = yp::ExprParser::new();

    check(
        &run_parser!(expr, r#"x in a"#),
        expect_test::expect!["x in a"],
    );

    check(
        &run_parser!(expr, r#"x in { 1, 2 }"#),
        expect_test::expect!["x in {1, 2}"],
    );

    check(
        &run_parser!(expr, r#"x in [ 1, 2 ] in { true, false }"#),
        expect_test::expect!["x in [1, 2] in {true, false}"],
    );

    check(
        &run_parser!(expr, r#"x as int in { 1, 2 }"#),
        expect_test::expect!["x as int in {1, 2}"],
    );

    check(
        &run_parser!(expr, r#"[1] in foo() in [[1]]"#),
        expect_test::expect!["[1] in foo() in [[1]]"],
    );

    check(
        &run_parser!(yp::LetDeclParser::new(), r#"let x = 5 in"#),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `false`, `ident`, `if`, `int_lit`, `real_lit`, `str_lit`, `true`, or `{`, found `end of file`
            @12..12: expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `false`, `ident`, `if`, `int_lit`, `real_lit`, `str_lit`, `true`, or `{`
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

    let filepath = Rc::from(Path::new("test"));
    let mut context = ParserContext {
        mod_path: &[String::from("foo")],
        mod_prefix: "::foo::",
        ii: &mut IntermediateIntent::default(),
        span_from: &|l, r| Span::new(Rc::clone(&filepath), l..r),
        use_paths: &mut vec![],
        next_paths: &mut vec![],
    };

    assert!(parse_and_collect!(yp::YurtParser::new(), src, filepath, context).is_ok());

    check(
        &context.ii.vars.iter().fold(String::new(), |mut s, (_, v)| {
            let _ = write!(s, "{}; ", context.ii.with_ii(&v));
            s
        }),
        expect_test::expect![["var ::foo::low_val: real; var ::foo::high_val; "]],
    );

    // This might be overkill...
    check(
        &context
            .ii
            .exprs
            .iter()
            .fold(String::new(), |mut s, (k, e)| {
                let _ = writeln!(s, "{k:?} -> {};", context.ii.with_ii(&e));
                s
            }),
        expect_test::expect![[r#"
             ExprKey(1v1) -> 1.23e0;
             ExprKey(2v1) -> var ::foo::low_val: real;
             ExprKey(3v1) -> (var ::foo::low_val: real == 1.23e0);
             ExprKey(4v1) -> 4.56e0;
             ExprKey(5v1) -> var ::foo::high_val;
             ExprKey(6v1) -> (var ::foo::high_val == 4.56e0);
             ExprKey(7v1) -> ::foo::mid;
             ExprKey(8v1) -> ::foo::low_val;
             ExprKey(9v1) -> 2e0;
             ExprKey(10v1) -> (::foo::low_val * 2e0);
             ExprKey(11v1) -> (::foo::mid > (::foo::low_val * 2e0));
             ExprKey(12v1) -> ::foo::mid;
             ExprKey(13v1) -> ::foo::high_val;
             ExprKey(14v1) -> (::foo::mid < ::foo::high_val);
             ExprKey(15v1) -> ::foo::mid;
        "#]],
    );

    check(
        &context
            .ii
            .constraints
            .iter()
            .fold(String::new(), |mut s, (e, _)| {
                let _ = writeln!(s, "constraint {};", context.ii.with_ii(e));
                s
            }),
        expect_test::expect![[r#"
            constraint (var ::foo::low_val: real == 1.23e0);
            constraint (var ::foo::high_val == 4.56e0);
            constraint (::foo::mid > (::foo::low_val * 2e0));
            constraint (::foo::mid < ::foo::high_val);
         "#]],
    );

    check(
        &context
            .ii
            .directives
            .iter()
            .fold(String::new(), |mut s, (sf, _)| {
                let _ = writeln!(s, "{};", context.ii.with_ii(sf));
                s
            }),
        expect_test::expect![[r#"
            solve minimize ::foo::mid;
         "#]],
    );
}

//#[test]
//fn fn_errors() {
//    check(
//        &run_parser!(yurt_program(), "fn foo() {5}"),
//        expect_test::expect![[r#"
//            expected `->`, found `{`
//            @9..10: expected `->`
//        "#]],
//    );
//
//    check(
//        &run_parser!(yurt_program(), "fn foo() -> real {}"),
//        expect_test::expect![[r#"
//            expected `!`, `(`, `+`, `-`, `::`, `::`, `[`, `cond`, `constraint`, `if`, `let`, `state`, `{`, or `{`, found `}`
//            @18..19: expected `!`, `(`, `+`, `-`, `::`, `::`, `[`, `cond`, `constraint`, `if`, `let`, `state`, `{`, or `{`
//        "#]],
//    );
//}

#[test]
fn out_of_order_decls() {
    let src = r#"
solve maximize low;
constraint low < high;
let high = 2.0;
solve satisfy;
let low = 1.0;
"#;

    let filepath = Rc::from(Path::new("test"));
    let mut context = ParserContext {
        mod_path: &[],
        mod_prefix: "",
        ii: &mut IntermediateIntent::default(),
        span_from: &|l, r| Span::new(Rc::clone(&filepath), l..r),
        use_paths: &mut vec![],
        next_paths: &mut vec![],
    };

    assert!(parse_and_collect!(yp::YurtParser::new(), src, filepath, context).is_ok());
}

#[test]
fn keywords_as_identifiers_errors() {
    let yurt = yp::YurtParser::new();
    let run_yurt_parser = |src_str: &String| {
        let filepath = Rc::from(Path::new("test"));
        let mut context = ParserContext {
            mod_path: &[],
            mod_prefix: "",
            ii: &mut IntermediateIntent::default(),
            span_from: &|l, r| Span::new(Rc::clone(&filepath), l..r),
            use_paths: &mut vec![],
            next_paths: &mut vec![],
        };

        match parse_and_collect!(yurt, src_str, filepath, context) {
            Ok(_) => "".to_string(),
            Err(errors) => display_errors(&errors),
        }
    };

    // TODO: Ideally, we emit a special error here. Instead, we currently get a generic "expected..
    // found" error.
    for keyword in KEYWORDS {
        let src = format!("let {keyword} = 5;").to_string();
        assert_eq!(
            &run_yurt_parser(&src),
            &format!(
                "expected `ident`, found `{keyword}`\n@4..{}: expected `ident`\n",
                4 + format!("{keyword}").len() // End of the error span)
            ),
            "Check \"identifier as keyword\" error for  keyword \"{}\"",
            keyword
        );
    }
}

#[test]
fn big_ints() {
    let let_decl = yp::LetDeclParser::new();

    check(
        &run_parser!(
            let_decl,
            "let blah = 1234567890123456789012345678901234567890"
        ),
        expect_test::expect!["var blah = 1234567890123456789012345678901234567890"],
    );
    check(
        &run_parser!(let_decl, "let blah = 0xfeedbadf00d2adeadcafed00dbabeface"),
        // Confirmed by using the Python REPL to convert from hex to dec...
        expect_test::expect!["var blah = 5421732407698601623698172315373246806734"],
    );
    check(
        &run_parser!(
            yp::ExprParser::new(),
            "0b110100101001010101010101010101010011010011010101010101010101010101010101010 + \
            0b01001001010110101010101001010101010010100100101001010010100100100001010"
        ),
        // Again confirmed using the Python REPL.  Handy.
        expect_test::expect!["(31076614848392666458794 + 676572722683907229962)"],
    );
}

#[test]
fn interface_test() {
    let interface_decl = yp::InterfaceDeclParser::new();

    let src = r#"
interface Foo {
    fn foo(x: real, y: int[5]) -> real;
    fn bar(x: bool,) -> real;
    fn baz() -> { int, real };
}
"#;

    check(
        &run_parser!(interface_decl, src),
        expect_test::expect!["interface Foo { fn Foo::foo(x: real, y: int[5]) -> real; fn Foo::bar(x: bool) -> real; fn Foo::baz() -> {int, real}; }"],
    );

    check(
        &run_parser!(interface_decl, "interface Foo {}"),
        expect_test::expect!["interface Foo { }"],
    );
}

#[test]
fn contract_test() {
    let contract_decl = yp::ContractDeclParser::new();

    check(
        &run_parser!(contract_decl, "contract Foo(0) {}"),
        expect_test::expect!["contract Foo(0) { }"],
    );

    check(
        &run_parser!(
            contract_decl,
            "contract Foo(0) { fn foo(x: int) -> real; fn bar(y: real, z: { Bar, }) -> string;}"
        ),
        expect_test::expect!["contract Foo(0) { fn Foo::foo(x: int) -> real; fn Foo::bar(y: real, z: {Bar}) -> string; }"],
    );

    check(
        &run_parser!(contract_decl, "contract Foo(if true {0} else {1}) {}"),
        expect_test::expect!["contract Foo(if true { 0 } else { 1 }) { }"],
    );

    check(
        &run_parser!(
            contract_decl,
            "contract Foo(0) implements X::Bar, ::Y::Baz {}"
        ),
        expect_test::expect!["contract Foo(0) implements X::Bar, ::Y::Baz { }"],
    );

    check(
        &run_parser!(
            contract_decl,
            "contract Foo(0) implements Bar { fn baz(x: real) -> int; }"
        ),
        expect_test::expect!["contract Foo(0) implements Bar { fn Foo::baz(x: real) -> int; }"],
    );

    check(
        &run_parser!(contract_decl, "contract Foo { }"),
        expect_test::expect![[r#"
            expected `(`, found `{`
            @13..14: expected `(`
        "#]],
    );

    check(
        &run_parser!(contract_decl, "contract Foo(0) implements { }"),
        expect_test::expect![[r#"
            expected `::`, or `ident`, found `{`
            @27..28: expected `::`, or `ident`
        "#]],
    );
}

#[test]
fn extern_test() {
    let extern_decl = yp::ExternDeclParser::new();

    check(
        &run_parser!(extern_decl, "extern {}"),
        expect_test::expect!["extern { }"],
    );
    check(
        &run_parser!(extern_decl, "extern { fn foo() -> string; }"),
        expect_test::expect!["extern { fn foo() -> string; }"],
    );
    check(
        &run_parser!(extern_decl, "extern { fn foo(x: int, y: real) -> int; }"),
        expect_test::expect!["extern { fn foo(x: int, y: real) -> int; }"],
    );
    check(
        &run_parser!(extern_decl, "extern { fn foo() -> int; fn bar() -> real; }"),
        expect_test::expect!["extern { fn foo() -> int; fn bar() -> real; }"],
    );
    check(
        &run_parser!(extern_decl, "extern { fn foo(); }"),
        expect_test::expect![[r#"
            expected `->`, found `;`
            @17..18: expected `->`
        "#]],
    );
}

#[test]
fn error_recovery() {
    let yurt = yp::YurtParser::new();
    let run_yurt_parser = |src_str| {
        let filepath = Rc::from(Path::new("test"));
        let mut context = ParserContext {
            mod_path: &[],
            mod_prefix: "",
            ii: &mut IntermediateIntent::default(),
            span_from: &|l, r| Span::new(Rc::clone(&filepath), l..r),
            use_paths: &mut vec![],
            next_paths: &mut vec![],
        };

        match parse_and_collect!(yurt, src_str, filepath, context) {
            Ok(_) => "".to_string(),
            Err(errors) => display_errors(&errors),
        }
    };

    let src = r#"
let untyped;
let clash = 5;
let clash = 5;
let clash = 5;
let empty_tuple: {} = {};
let empty_array: int[] = [];
let empty_index = a[];
let bad_integer_index = t.0x5;
let bad_real_index = t.1e5;
let parse_error
"#;

    check(
        &run_yurt_parser(src),
        expect_test::expect![[r#"
        type annotation or initializer needed for variable `untyped`
        @1..12: type annotation or initializer needed
        consider giving `untyped` an explicit type or an initializersymbol `clash` has already been declared

        @18..23: previous declaration of the value `clash` here
        @33..38: `clash` redeclared here
        `clash` must be declared or imported only once in this scopesymbol `clash` has already been declared

        @18..23: previous declaration of the value `clash` here
        @48..53: `clash` redeclared here
        `clash` must be declared or imported only once in this scopeempty tuple types are not allowed
        @76..78: empty tuple type found
        empty tuple expressions are not allowed
        @81..83: empty tuple expression found
        empty array types are not allowed
        @102..107: empty array type found
        missing array index
        @132..135: missing array element index
        invalid integer `0x5` as tuple index
        @163..166: invalid integer as tuple index
        invalid value `1e5` as tuple index
        @191..194: invalid value as tuple index
        expected `:`, `;`, or `=`, found `end of file`
        @211..211: expected `:`, `;`, or `=`
    "#]],
    );
}
