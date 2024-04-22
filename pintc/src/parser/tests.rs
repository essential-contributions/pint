use crate::{
    error::{Error, Handler, ReportableError},
    intermediate::{DisplayWithII, IntermediateIntent, Program, ProgramKind},
    lexer::{self, KEYWORDS},
    parser::ParserContext,
    span::Span,
};
use std::{collections::BTreeMap, path::Path, rc::Rc};

#[cfg(test)]
use pint_parser as yp;

#[cfg(test)]
use lalrpop_util::lalrpop_mod;

#[cfg(test)]
lalrpop_mod!(#[allow(unused)] pub pint_parser);

/// Given a parser, some source code, and a parser context, parse the source code and return the
/// parsed result or the full set of errors
#[cfg(test)]
macro_rules! parse_and_collect_errors {
    ($parser: expr, $source: expr, $context: expr) => {{
        let filepath = Rc::from(Path::new("test"));

        let handler = Handler::default();
        match $parser.parse(
            &mut $context,
            &handler,
            lexer::Lexer::new($source, &filepath, &[]),
        ) {
            Ok(result) => {
                if handler.has_errors() {
                    Err(handler.consume())
                } else {
                    Ok(result)
                }
            }
            Err(lalrpop_err) => {
                handler.emit_err(Error::Parse {
                    error: (lalrpop_err, &filepath).into(),
                });
                Err(handler.consume())
            }
        }
    }};
}

/// Generate a `ParserContext` given a path to the current module and a vector `use_paths` that
/// collects all use statements encountered by a parser.
#[cfg(test)]
macro_rules! context {
    ($mod_path: expr, $use_paths: expr) => {{
        ParserContext {
            mod_path: &$mod_path,
            mod_prefix: &(!$mod_path.is_empty())
                .then(|| format!("::{}::", $mod_path.join("::")))
                .unwrap_or("::".to_string()),
            local_scope: None,
            program: &mut Program {
                kind: ProgramKind::Stateless,
                iis: BTreeMap::from([(
                    Program::ROOT_II_NAME.to_string(),
                    IntermediateIntent::default(),
                )]),
            },
            current_ii: &mut Program::ROOT_II_NAME.to_string(),
            macros: &mut vec![],
            macro_calls: &mut BTreeMap::from([(
                Program::ROOT_II_NAME.to_string(),
                slotmap::SecondaryMap::new(),
            )]),
            span_from: &|l, r| Span::new(Rc::from(Path::new("")), l..r),
            use_paths: &mut $use_paths,
            next_paths: &mut vec![],
        }
    }};
}

/// Run a parser and print the following, in order:
/// - All use statements, broken down into individual paths
/// - The full content of the `IntermediateIntent` post parsing (useful for decls).
/// - The output of the parser itself (useful for expressions and types).
#[cfg(test)]
macro_rules! run_parser {
    ($parser: expr, $source: expr) => {{
        let mod_path = Vec::<String>::new();
        run_parser!(@internal $parser, $source, mod_path)
    }};

    ($parser: expr, $source: expr, $mod_path: expr) => {{
        run_parser!(@internal $parser, $source, $mod_path)
    }};

    (@internal $parser: expr, $source: expr, $mod_path: expr) => {{
        let mut use_paths = Vec::new();
        let mut context = context!($mod_path, &mut use_paths);
        let result = parse_and_collect_errors!($parser, $source, context);

        let parser_output = match result {
            Ok(item) => {
                let result =
                    format!("{}{}",
                        context.program,
                        context.program.root_ii().with_ii(&item)
                    );
                format!("{}{}",
                    use_paths
                        .iter()
                        .map(|up| "use ".to_owned() + &up.to_string())
                        .collect::<Vec<_>>()
                        .join(";\n")
                        , result.trim_end().to_owned())
            }
            Err(errors) => display_errors(&errors),
        };

        format!("{}", parser_output)
    }};
}

/// Many parsers return () which we may need to print. Just do nothing!
#[cfg(test)]
impl DisplayWithII for () {
    fn fmt(&self, _f: &mut std::fmt::Formatter, _ii: &IntermediateIntent) -> std::fmt::Result {
        Ok(())
    }
}

#[cfg(test)]
fn display_errors(errors: &[Error]) -> String {
    // Print each error on one line. For each error, start with the span.
    errors.iter().fold(String::new(), |acc, error| {
        format!("{}{}", acc, error.display_raw())
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
        expect_test::expect!["::MyType"],
    );
    check(
        &run_parser!(type_, "A::B::C::MyType"),
        expect_test::expect!["::A::B::C::MyType"],
    );
    check(
        &run_parser!(type_, "::A::B::C::MyType"),
        expect_test::expect!["::A::B::C::MyType"],
    );

    // Not allowed in arbitrary types
    check(
        &run_parser!(type_, "(int => bool)"),
        expect_test::expect![[r#"
            expected `::`, `b256_ty`, `bool_ty`, `ident`, `int_ty`, `real_ty`, `string_ty`, or `{`, found `(`
            @0..1: expected `::`, `b256_ty`, `bool_ty`, `ident`, `int_ty`, `real_ty`, `string_ty`, or `{`
        "#]],
    );
}

#[test]
fn storage_types() {
    let storage_var_type = yp::StorageVarTypeParser::new();

    // while not all of these are currently supported e2e, the `StorageVarTypeParser` does allow
    // them, though some of them get blocked by the parser in `StateDeclParser`

    check(
        &run_parser!(storage_var_type, "int"),
        expect_test::expect!["int"],
    );
    check(
        &run_parser!(storage_var_type, "real"),
        expect_test::expect!["real"],
    );
    check(
        &run_parser!(storage_var_type, "bool"),
        expect_test::expect!["bool"],
    );
    check(
        &run_parser!(storage_var_type, "string"),
        expect_test::expect!["string"],
    );
    check(
        &run_parser!(storage_var_type, "{int, real, string}"),
        expect_test::expect!["{int, real, string}"],
    );
    check(
        &run_parser!(storage_var_type, "{int, {real, int}, string}"),
        expect_test::expect!["{int, {real, int}, string}"],
    );
    check(
        &run_parser!(storage_var_type, "{int, }"),
        expect_test::expect!["{int}"],
    );
    check(
        &run_parser!(storage_var_type, "{int}"),
        expect_test::expect!["{int}"],
    );
    check(
        &run_parser!(storage_var_type, "{}"),
        expect_test::expect![[r#"
            empty tuple types are not allowed
            @0..2: empty tuple type found
        "#]],
    );
    check(
        &run_parser!(storage_var_type, "MyType"),
        expect_test::expect!["::MyType"],
    );
    check(
        &run_parser!(storage_var_type, "A::B::C::MyType"),
        expect_test::expect!["::A::B::C::MyType"],
    );
    check(
        &run_parser!(storage_var_type, "::A::B::C::MyType"),
        expect_test::expect!["::A::B::C::MyType"],
    );

    // maps are allowed here
    check(
        &run_parser!(storage_var_type, "(int => bool)"),
        expect_test::expect!["( int => bool )"],
    );

    check(
        &run_parser!(storage_var_type, "(int => (int => (b256 => b256)))"),
        expect_test::expect!["( int => ( int => ( b256 => b256 ) ) )"],
    );
}

#[test]
fn immediates() {
    let immediate = yp::ImmediateParser::new();

    check(&run_parser!(immediate, "0x88"), expect_test::expect!["136"]);
    check(&run_parser!(immediate, "0b111"), expect_test::expect!["7"]);
    check(&run_parser!(immediate, "1"), expect_test::expect!["1"]);

    // i64 hex literals
    check(
        &run_parser!(immediate, "0x0000000000000011"),
        expect_test::expect!["17"],
    );

    check(
        &run_parser!(
            immediate,
            "0xFFFFFFFFFFFFFFF" // 15 digits
        ),
        expect_test::expect!["1152921504606846975"],
    );

    check(
        &run_parser!(
            immediate,
            "0x7FFFFFFFFFFFFFFF" // 16 digits and positive (top bit is 0)
        ),
        expect_test::expect!["9223372036854775807"],
    );

    check(
        &run_parser!(
            immediate,
            "0x8FFFFFFFFFFFFFFF" // 16 digits and negative (top bit is 1)
        ),
        expect_test::expect!["-8070450532247928833"],
    );

    check(
        &run_parser!(immediate, "0xFFFFFFFFFFFFFFFF"),
        expect_test::expect!["-1"],
    );

    // i64 binary literals
    check(
        &run_parser!(
            immediate,
            "0b0000000000000000000000000000000000000000000000000000000000010001"
        ),
        expect_test::expect!["17"],
    );

    check(
        &run_parser!(
            immediate,
            "0b111111111111111111111111111111111111111111111111111111111111111" // 63 digits
        ),
        expect_test::expect!["9223372036854775807"],
    );

    check(
        &run_parser!(
            immediate,
            "0b0111111111111111111111111111111111111111111111111111111111111111" // 64 digits and positive (top bit is 0)
        ),
        expect_test::expect!["9223372036854775807"],
    );

    check(
        &run_parser!(
            immediate,
            "0b1111111111111111111111111111111111111111111111111111111111111111" // 64 digits and negative (top bit is 1)
        ),
        expect_test::expect!["-1"],
    );

    // b256 hex literals
    check(
        &run_parser!(
            immediate,
            "0x3333333333333333333333333333333333333333333333333333333333333333"
        ),
        expect_test::expect!["0x3333333333333333333333333333333333333333333333333333333333333333"],
    );

    check(
        &run_parser!(
            immediate,
            "0x8000000000000000000000000000000000000000000000000000000000000001"
        ),
        expect_test::expect!["0x8000000000000000000000000000000000000000000000000000000000000001"],
    );

    check(
        &run_parser!(
            immediate,
            "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
        ),
        expect_test::expect!["0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"],
    );

    // b256 binary literals
    check(
        &run_parser!(immediate, "0b1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"),
        expect_test::expect!["0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"],
    );

    check(
        &run_parser!(immediate, "0b1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011"),
        expect_test::expect!["0x8000000000000000000000000000000000000000000000000000000000000003"],
    );

    // Bad lengths
    check(
        &run_parser!(immediate, "0x4f3f4f3f4f3f4f3f4f3f4f3f4f"),
        expect_test::expect![[r#"
            unexpected hexadecimal integer literal length
            @0..28: 26 is not a valid number of digits in a hexadecimal integer literal
            number of digits must be either 64 or between 1 and 16
        "#]],
    );

    check(
        &run_parser!(
            immediate,
            "0x18000000000000000000000000000000000000000000000000000000000000001"
        ),
        expect_test::expect![[r#"
            unexpected hexadecimal integer literal length
            @0..67: 65 is not a valid number of digits in a hexadecimal integer literal
            number of digits must be either 64 or between 1 and 16
        "#]],
    );

    check(
            &run_parser!(immediate, "0b1000000000000000000000000000000000000000000000000000000000000000000000000000000000000"),
            expect_test::expect![[r#"
                unexpected binary integer literal length
                @0..87: 85 is not a valid number of digits in a binary integer literal
                number of digits must be either 256 or between 1 and 64
            "#]],
        );

    check(
        &run_parser!(immediate, "0b11000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011"),
        expect_test::expect![[r#"
            unexpected binary integer literal length
            @0..259: 257 is not a valid number of digits in a binary integer literal
            number of digits must be either 256 or between 1 and 64
        "#]],
    );

    check(
        &run_parser!(immediate, "9223372036854775808"),
        expect_test::expect![[r#"
            integer literal is too large
            @0..19: integer literal is too large
            value exceeds limit of `9,223,372,036,854,775,807`
        "#]],
    );

    check(
        &run_parser!(immediate, "1.3"),
        expect_test::expect!["1.3e0"],
    );
}

#[test]
fn use_statements() {
    let pint = yp::PintParser::new();
    let mod_path = vec!["foo".to_string()];

    check(
        &run_parser!(pint, "use {}; use ::{};", mod_path),
        expect_test::expect![""],
    );

    check(
        &run_parser!(pint, "use a; use ::b; use ::c as d;", mod_path),
        expect_test::expect![[r#"
            use foo::a;
            use b;
            use c as d"#]],
    );

    check(
        &run_parser!(pint, "use a::b; use ::b::c; use ::c::d as d;", mod_path),
        expect_test::expect![[r#"
            use foo::a::b;
            use b::c;
            use c::d as d"#]],
    );

    check(
        &run_parser!(pint, "use a::{b, c as d};", mod_path),
        expect_test::expect![[r#"
            use foo::a::b;
            use foo::a::c as d"#]],
    );

    check(
        &run_parser!(pint, "use a::{{c as d, { e as f }}};", mod_path),
        expect_test::expect![[r#"
            use foo::a::c as d;
            use foo::a::e as f"#]],
    );

    // Errors - TODO: improve these
    check(
        &run_parser!(pint, "use ;", mod_path),
        expect_test::expect![[r#"
            expected `::`, `ident`, `macro_name`, `self`, or `{`, found `;`
            @4..5: expected `::`, `ident`, `macro_name`, `self`, or `{`
        "#]],
    );

    check(
        &run_parser!(pint, "use ::;", mod_path),
        expect_test::expect![[r#"
            expected `ident`, `macro_name`, `self`, or `{`, found `;`
            @6..7: expected `ident`, `macro_name`, `self`, or `{`
        "#]],
    );

    check(
        &run_parser!(pint, "use a::;", mod_path),
        expect_test::expect![[r#"
            expected `ident`, `macro_name`, `self`, or `{`, found `;`
            @7..8: expected `ident`, `macro_name`, `self`, or `{`
        "#]],
    );

    check(
        &run_parser!(pint, "use a::b; use a::c as b; use ::c::d as b;", mod_path),
        expect_test::expect![[r#"
            symbol `b` has already been declared
            @7..8: previous declaration of the symbol `b` here
            @17..23: `b` redeclared here
            `b` must be declared or imported only once in this scope
            symbol `b` has already been declared
            @7..8: previous declaration of the symbol `b` here
            @34..40: `b` redeclared here
            `b` must be declared or imported only once in this scope
        "#]],
    );

    check(
        &run_parser!(pint, "use a::{{self as t}, b, c::{self, d}, e};", mod_path),
        expect_test::expect![[r#"
            use foo::a as t;
            use foo::a::b;
            use foo::a::c;
            use foo::a::c::d;
            use foo::a::e"#]],
    );

    check(
        &run_parser!(
            pint,
            "use {self}; use self; use self::a; use self::self; use a::{b as self}",
            mod_path
        ),
        expect_test::expect![[r#"
            `self` import can only appear in an import list with a non-empty prefix
            @5..9: can only appear in an import list with a non-empty prefix
            `self` import can only appear in an import list with a non-empty prefix
            @16..20: can only appear in an import list with a non-empty prefix
            `self` is only allowed at the end of a use path
            @26..33: `self` can only appear at the end of a use path
            `self` is only allowed at the end of a use path
            @39..49: `self` can only appear at the end of a use path
            expected `ident`, found `self`
            @64..68: expected `ident`
        "#]],
    );
}

#[test]
fn storage_decl() {
    let pint = yp::PintParser::new();

    let src = r#"
storage {
    integer: int,    
    boolean: bool,
}
"#;

    check(
        &run_parser!(pint, src),
        expect_test::expect![[r#"
            storage {
                integer: int,
                boolean: bool,
            }"#]],
    );

    // No trailing comma
    check(
        &run_parser!(pint, r#"storage { x: int, y: bool }"#),
        expect_test::expect![[r#"
            storage {
                x: int,
                y: bool,
            }"#]],
    );

    check(
        &run_parser!(
            pint,
            r#"storage { x: int, y: bool, z: b256, w: (int => (bool => b256)) }"#
        ),
        expect_test::expect![[r#"
            storage {
                x: int,
                y: bool,
                z: b256,
                w: ( int => ( bool => b256 ) ),
            }"#]],
    );

    check(
        &run_parser!(pint, r#"storage { }"#),
        expect_test::expect![[r#"
            storage {
            }"#]],
    );

    check(
        &run_parser!(pint, r#"storage { x, y }"#),
        expect_test::expect![[r#"
            expected `:`, found `,`
            @11..12: expected `:`
        "#]],
    );
}

#[test]
fn extern_decl() {
    let pint = yp::PintParser::new();

    let src = r#"
extern Foo(0x1111111111111111111111111111111111111111111111111111111111111111) {
    storage {
        integer: int,    
        boolean: bool,
    }
}
"#;

    check(
        &run_parser!(pint, src),
        expect_test::expect![[r#"
            extern ::Foo(0x1111111111111111111111111111111111111111111111111111111111111111) {
                storage {
                    integer: int,
                    boolean: bool,
                }
            }"#]],
    );

    // No trailing comma
    check(
        &run_parser!(
            pint,
            r#"
            extern Foo(0x1111111111111111111111111111111111111111111111111111111111111111) {
                 storage { x: int, y: bool }
            }"#
        ),
        expect_test::expect![[r#"
            extern ::Foo(0x1111111111111111111111111111111111111111111111111111111111111111) {
                storage {
                    x: int,
                    y: bool,
                }
            }"#]],
    );

    check(
        &run_parser!(
            pint,
            r#"
            extern Foo(0x1111111111111111111111111111111111111111111111111111111111111111) {
                 storage { x: int, y: bool }
            }
            extern Bar(0x2222222222222222222222222222222222222222222222222222222222222222) {
                 storage { x: int, y: bool }
            }
            "#
        ),
        expect_test::expect![[r#"
            extern ::Foo(0x1111111111111111111111111111111111111111111111111111111111111111) {
                storage {
                    x: int,
                    y: bool,
                }
            }
            extern ::Bar(0x2222222222222222222222222222222222222222222222222222222222222222) {
                storage {
                    x: int,
                    y: bool,
                }
            }"#]],
    );

    check(
        &run_parser!(
            pint,
            r#"
            extern Foo(0x1111111111111111111111111111111111111111111111111111111111111111) {
                storage { x: int, y: bool, z: b256, w: (int => (bool => b256)) }
            }"#
        ),
        expect_test::expect![[r#"
            extern ::Foo(0x1111111111111111111111111111111111111111111111111111111111111111) {
                storage {
                    x: int,
                    y: bool,
                    z: b256,
                    w: ( int => ( bool => b256 ) ),
                }
            }"#]],
    );

    check(
        &run_parser!(
            pint,
            r#"
            extern Foo(0x1111111111111111111111111111111111111111111111111111111111111111) { storage { } }"#
        ),
        expect_test::expect![[r#"
            extern ::Foo(0x1111111111111111111111111111111111111111111111111111111111111111) {
                storage {
                }
            }"#]],
    );
    check(
        &run_parser!(
            pint,
            r#"
            extern Foo(0x1111111111111111111111111111111111111111111111111111111111111111) { storage { } }
            extern Foo(0x2222222222222222222222222222222222222222222222222222222222222222) { storage { } }
            "#
        ),
        expect_test::expect![[r#"
            symbol `Foo` has already been declared
            @20..23: previous declaration of the symbol `Foo` here
            @127..130: `Foo` redeclared here
            `Foo` must be declared or imported only once in this scope
        "#]],
    );

    check(
        &run_parser!(
            pint,
            r#"
            extern Foo(0x1111111111111111111111111111111111111111111111111111111111111111) {
                storage { x, y }
            }"#
        ),
        expect_test::expect![[r#"
            expected `:`, found `,`
            @121..122: expected `:`
        "#]],
    );

    check(
        &run_parser!(
            pint,
            r#"
            extern Foo { storage { x: int, y: int } }"#
        ),
        expect_test::expect![[r#"
            expected `(`, found `{`
            @24..25: expected `(`
        "#]],
    );

    check(
        &run_parser!(
            pint,
            r#"
            extern Foo(0x1111111111111111111111111111111111111111111111111111111111111111) { }"#
        ),
        expect_test::expect![[r#"
            expected `storage`, found `}`
            @94..95: expected `storage`
        "#]],
    );

    check(
        &run_parser!(
            pint,
            r#"
            extern Foo(0x1111111111111111111111111111111111111111111111111111111111111111) { 
                storage { }
                storage { }
            }"#
        ),
        expect_test::expect![[r#"
            expected `}`, found `storage`
            @139..146: expected `}`
        "#]],
    );
}

#[test]
fn storage_access() {
    let expr = yp::StateInitParser::new();

    check(
        &run_parser!(expr, r#"storage::foo"#),
        expect_test::expect!["storage::foo"],
    );

    check(
        &run_parser!(expr, r#"storage::balances[0x111]"#),
        expect_test::expect!["storage::balances[273]"],
    );

    check(
        &run_parser!(expr, r#"storage::balances[0x111][foo()][t[3].5]"#),
        expect_test::expect!["storage::balances[273][::foo()][::t[3].5]"],
    );

    check(
        &run_parser!(expr, r#"storage_lib::foo()"#),
        expect_test::expect!["::storage_lib::foo()"],
    );

    let pint = yp::PintParser::new();

    check(
        &run_parser!(pint, r#"let x = storage::foo;"#),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `exists`, `false`, `forall`, `ident`, `if`, `int_lit`, `macro_name`, `real_lit`, `str_lit`, `true`, or `{`, found `storage`
            @8..15: expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `exists`, `false`, `forall`, `ident`, `if`, `int_lit`, `macro_name`, `real_lit`, `str_lit`, `true`, or `{`
        "#]],
    );

    check(
        &run_parser!(pint, r#"let x = storage::map[4][3];"#),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `exists`, `false`, `forall`, `ident`, `if`, `int_lit`, `macro_name`, `real_lit`, `str_lit`, `true`, or `{`, found `storage`
            @8..15: expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `exists`, `false`, `forall`, `ident`, `if`, `int_lit`, `macro_name`, `real_lit`, `str_lit`, `true`, or `{`
        "#]],
    );

    check(
        &run_parser!(pint, r#"constraint storage::map[69] == 0;"#),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `exists`, `false`, `forall`, `ident`, `if`, `int_lit`, `macro_name`, `real_lit`, `str_lit`, `true`, or `{`, found `storage`
            @11..18: expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `exists`, `false`, `forall`, `ident`, `if`, `int_lit`, `macro_name`, `real_lit`, `str_lit`, `true`, or `{`
        "#]],
    );
}

#[test]
fn external_storage_access() {
    let expr = yp::StateInitParser::new();

    check(
        &run_parser!(expr, r#"Foo::storage::foo"#),
        expect_test::expect!["::Foo::storage::foo"],
    );

    check(
        &run_parser!(expr, r#"Foo::Bar::storage::balances[0x111]"#),
        expect_test::expect!["::Foo::Bar::storage::balances[273]"],
    );

    check(
        &run_parser!(expr, r#"::Foo::storage::balances[0x111][foo()][t[3].5]"#),
        expect_test::expect!["::Foo::storage::balances[273][::foo()][::t[3].5]"],
    );

    check(
        &run_parser!(expr, r#"Bar::storage_lib::foo()"#),
        expect_test::expect!["::Bar::storage_lib::foo()"],
    );

    let pint = yp::PintParser::new();

    check(
        &run_parser!(pint, r#"let x = ::Foo::storage::foo;"#),
        expect_test::expect![[r#"
            expected `ident`, or `macro_name`, found `storage`
            @15..22: expected `ident`, or `macro_name`
        "#]],
    );

    check(
        &run_parser!(pint, r#"let x = Bar::storage::map[4][3];"#),
        expect_test::expect![[r#"
            expected `ident`, or `macro_name`, found `storage`
            @13..20: expected `ident`, or `macro_name`
        "#]],
    );

    check(
        &run_parser!(pint, r#"constraint ::Foo::storage::map[69] == 0;"#),
        expect_test::expect![[r#"
            expected `ident`, or `macro_name`, found `storage`
            @18..25: expected `ident`, or `macro_name`
        "#]],
    );
}

#[test]
fn let_decls() {
    let mod_path = vec!["foo".to_string()];

    check(
        &run_parser!(yp::LetDeclParser::new(), "let blah", mod_path),
        expect_test::expect![[r#"
            type annotation or initializer needed for variable `blah`
            @0..8: type annotation or initializer needed
            consider giving `blah` an explicit type or an initializer
        "#]],
    );
    check(
        &run_parser!(yp::LetDeclParser::new(), "let blah = 1.0", mod_path),
        expect_test::expect![[r#"
            var ::foo::blah;
            constraint (::foo::blah == 1e0);"#]],
    );
    check(
        &run_parser!(yp::LetDeclParser::new(), "let blah: real = 1.0", mod_path),
        expect_test::expect![[r#"
            var ::foo::blah: real;
            constraint (::foo::blah == 1e0);"#]],
    );
    check(
        &run_parser!(yp::LetDeclParser::new(), "let blah: real", mod_path),
        expect_test::expect!["var ::foo::blah: real;"],
    );
    check(
        &run_parser!(yp::LetDeclParser::new(), "let blah = 1", mod_path),
        expect_test::expect![[r#"
            var ::foo::blah;
            constraint (::foo::blah == 1);"#]],
    );
    check(
        &run_parser!(yp::LetDeclParser::new(), "let blah: int = 1", mod_path),
        expect_test::expect![[r#"
            var ::foo::blah: int;
            constraint (::foo::blah == 1);"#]],
    );
    check(
        &run_parser!(yp::LetDeclParser::new(), "let blah: int", mod_path),
        expect_test::expect!["var ::foo::blah: int;"],
    );
    check(
        &run_parser!(yp::LetDeclParser::new(), "let blah = true", mod_path),
        expect_test::expect![[r#"
            var ::foo::blah;
            constraint (::foo::blah == true);"#]],
    );
    check(
        &run_parser!(yp::LetDeclParser::new(), "let blah: bool = false", mod_path),
        expect_test::expect![[r#"
            var ::foo::blah: bool;
            constraint (::foo::blah == false);"#]],
    );
    check(
        &run_parser!(yp::LetDeclParser::new(), "let blah: bool", mod_path),
        expect_test::expect!["var ::foo::blah: bool;"],
    );
    check(
        &run_parser!(yp::LetDeclParser::new(), r#"let blah = "hello""#, mod_path),
        expect_test::expect![[r#"
            var ::foo::blah;
            constraint (::foo::blah == "hello");"#]],
    );
    check(
        &run_parser!(
            yp::LetDeclParser::new(),
            r#"let blah: string = "hello""#,
            mod_path
        ),
        expect_test::expect![[r#"
            var ::foo::blah: string;
            constraint (::foo::blah == "hello");"#]],
    );
    check(
        &run_parser!(yp::LetDeclParser::new(), r#"let blah: string"#, mod_path),
        expect_test::expect!["var ::foo::blah: string;"],
    );
}

#[test]
fn state_decls() {
    let state = yp::StateDeclParser::new();

    check(
        &run_parser!(state, "state x: int = MyPath::foo()"),
        expect_test::expect!["state ::x: int = ::MyPath::foo();"],
    );
    check(
        &run_parser!(state, "state y = MyPath::bar()"),
        expect_test::expect!["state ::y = ::MyPath::bar();"],
    );
}

#[test]
fn constraint_decls() {
    // Argument just needs to be any expression, as far as the parser is concerned.
    let constraint_decl = yp::ConstraintDeclParser::new();

    check(
        &run_parser!(constraint_decl, "constraint blah"),
        expect_test::expect!["constraint ::blah;"],
    );
}

#[test]
fn solve_decls() {
    let solve_decl = yp::SolveDeclParser::new();

    check(
        &run_parser!(solve_decl, "solve satisfy"),
        expect_test::expect!["solve satisfy;"],
    );
    check(
        &run_parser!(solve_decl, "solve minimize foo"),
        expect_test::expect!["solve minimize ::foo;"],
    );
    check(
        &run_parser!(solve_decl, "solve maximize foo"),
        expect_test::expect!["solve maximize ::foo;"],
    );

    check(
        &run_parser!(solve_decl, "solve maximize x + y"),
        expect_test::expect!["solve maximize (::x + ::y);"],
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
    check(&run_parser!(expr, "foo"), expect_test::expect!["::foo"]);
}

#[test]
fn unary_op_exprs() {
    let expr = yp::ExprParser::new();

    check(&run_parser!(expr, "!a"), expect_test::expect!["!::a"]);
    check(&run_parser!(expr, "-a"), expect_test::expect!["-::a"]);
    check(&run_parser!(expr, "-1.0"), expect_test::expect!["-1e0"]);
    check(&run_parser!(expr, "-1"), expect_test::expect!["-1"]);
    check(&run_parser!(expr, "-0x133"), expect_test::expect!["-307"]);
    check(&run_parser!(expr, "-0b1101"), expect_test::expect!["-13"]);
    check(
        &run_parser!(
            expr,
            "-0b1101000000001100101010101010101010101010101010101101010101010101"
        ),
        expect_test::expect!["--3455199164115528363"],
    );
    check(
        &run_parser!(expr, "! - - !  --  -1"),
        expect_test::expect!["!--!---1"],
    );
    check(
        &run_parser!(expr, "! {- x} '  '  "),
        expect_test::expect!["!-::x''"],
    );
    check(
        &run_parser!(expr, "+ { + x} '  '  "),
        expect_test::expect![[r#"
            leading `+` is not supported
            @0..1: unexpected `+`
            try removing the `+`
            leading `+` is not supported
            @4..5: unexpected `+`
            try removing the `+`
        "#]],
    );
    check(
        &run_parser!(expr, "1 + + { + x}"),
        expect_test::expect![[r#"
            leading `+` is not supported
            @4..5: unexpected `+`
            try removing the `+`
            leading `+` is not supported
            @8..9: unexpected `+`
            try removing the `+`
        "#]],
    );
}

#[test]
fn binary_op_exprs() {
    let expr = yp::ExprParser::new();

    check(
        &run_parser!(expr, "a * 2.0"),
        expect_test::expect!["(::a * 2e0)"],
    );
    check(
        &run_parser!(expr, "a / 2.0"),
        expect_test::expect!["(::a / 2e0)"],
    );
    check(
        &run_parser!(expr, "a % 2.0"),
        expect_test::expect!["(::a % 2e0)"],
    );
    check(
        &run_parser!(expr, "a + 2.0"),
        expect_test::expect!["(::a + 2e0)"],
    );
    check(
        &run_parser!(expr, "a - 2.0"),
        expect_test::expect!["(::a - 2e0)"],
    );
    check(
        &run_parser!(expr, "a+2.0"),
        expect_test::expect!["(::a + 2e0)"],
    );
    check(
        &run_parser!(expr, "a-2.0"),
        expect_test::expect!["(::a - 2e0)"],
    );
    check(
        &run_parser!(expr, "a < 2.0"),
        expect_test::expect!["(::a < 2e0)"],
    );
    check(
        &run_parser!(expr, "a > 2.0"),
        expect_test::expect!["(::a > 2e0)"],
    );
    check(
        &run_parser!(expr, "a <= 2.0"),
        expect_test::expect!["(::a <= 2e0)"],
    );
    check(
        &run_parser!(expr, "a >= 2.0"),
        expect_test::expect!["(::a >= 2e0)"],
    );
    check(
        &run_parser!(expr, "a == 2.0"),
        expect_test::expect!["(::a == 2e0)"],
    );
    check(
        &run_parser!(expr, "a != 2.0"),
        expect_test::expect!["(::a != 2e0)"],
    );
    check(
        &run_parser!(expr, "a && b"),
        expect_test::expect!["(::a && ::b)"],
    );

    check(
        &run_parser!(expr, "a || b"),
        expect_test::expect!["(::a || ::b)"],
    );

    check(
        &run_parser!(expr, "a || b && c || d && !e"),
        expect_test::expect!["((::a || (::b && ::c)) || (::d && !::e))"],
    );
}

#[test]
fn complex_exprs() {
    let expr = yp::ExprParser::new();

    check(
        &run_parser!(expr, "2 * b * 3"),
        expect_test::expect!["((2 * ::b) * 3)"],
    );
    check(
        &run_parser!(expr, "2 < b * 3"),
        expect_test::expect!["(2 < (::b * 3))"],
    );
    check(
        &run_parser!(expr, "2.0 > b * 3.0"),
        expect_test::expect!["(2e0 > (::b * 3e0))"],
    );
    check(
        &run_parser!(expr, "2.0 * b < 3.0"),
        expect_test::expect!["((2e0 * ::b) < 3e0)"],
    );
    check(
        &run_parser!(expr, "2 > b < 3"),
        expect_test::expect!["((2 > ::b) < 3)"],
    );
    check(
        &run_parser!(expr, "2 != b < 3"),
        expect_test::expect!["((2 != ::b) < 3)"],
    );
    check(
        &run_parser!(expr, "2 < b != 3"),
        expect_test::expect!["((2 < ::b) != 3)"],
    );
    check(
        &run_parser!(expr, "a > b * c < d"),
        expect_test::expect!["((::a > (::b * ::c)) < ::d)"],
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
        expect_test::expect!["!(::a < ::b)"],
    );
    check(&run_parser!(expr, "(1)"), expect_test::expect!["1"]);
    check(&run_parser!(expr, "(a)"), expect_test::expect!["::a"]);
    check(
        &run_parser!(expr, "()"),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `exists`, `false`, `forall`, `ident`, `if`, `int_lit`, `macro_name`, `real_lit`, `str_lit`, `true`, or `{`, found `)`
            @1..2: expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `exists`, `false`, `forall`, `ident`, `if`, `int_lit`, `macro_name`, `real_lit`, `str_lit`, `true`, or `{`
        "#]],
    );

    check(
        &run_parser!(expr, "(if a < b { 1 } else { 2 })"),
        expect_test::expect!["if (::a < ::b) { 1 } else { 2 }"],
    );
    check(
        &run_parser!(expr, "(foo(a, b, c))"),
        expect_test::expect!["::foo(::a, ::b, ::c)"],
    );
}

#[test]
fn enums() {
    let enum_decl = yp::EnumDeclParser::new();
    let expr = yp::ExprParser::new();
    let let_decl = yp::LetDeclParser::new();

    check(
        &run_parser!(enum_decl, "enum MyEnum = Variant1 | Variant2"),
        expect_test::expect!["enum ::MyEnum = Variant1 | Variant2;"],
    );
    check(
        &run_parser!(enum_decl, "enum MyEnum = Variant1"),
        expect_test::expect!["enum ::MyEnum = Variant1;"],
    );
    check(
        &run_parser!(expr, "MyEnum::Variant1"),
        expect_test::expect!["::MyEnum::Variant1"],
    );
    check(
        &run_parser!(
            let_decl,
            r#"
                let x = MyEnum::Variant3
                "#
        ),
        expect_test::expect![[r#"
            var ::x;
            constraint (::x == ::MyEnum::Variant3);"#]],
    );
    check(
        &run_parser!(
            let_decl,
            r#"
                let e: ::path::to::MyEnum
                "#
        ),
        expect_test::expect![[r#"
            var ::e: ::path::to::MyEnum;"#]],
    );
}

#[test]
fn custom_types() {
    let type_ = yp::TypeParser::new();
    let type_decl = yp::NewTypeDeclParser::new();

    check(
        &run_parser!(type_, "custom_type"),
        expect_test::expect!["::custom_type"],
    );
    check(
        &run_parser!(type_decl, "type MyInt = int"),
        expect_test::expect!["type ::MyInt = int;"],
    );
    check(
        &run_parser!(type_decl, "type MyReal = real"),
        expect_test::expect!["type ::MyReal = real;"],
    );
    check(
        &run_parser!(type_decl, "type MyBool = bool"),
        expect_test::expect!["type ::MyBool = bool;"],
    );
    check(
        &run_parser!(type_decl, "type MyString = string"),
        expect_test::expect!["type ::MyString = string;"],
    );
    check(
        &run_parser!(type_decl, "type IntArray = int[5]"),
        expect_test::expect!["type ::IntArray = int[5];"],
    );
    check(
        &run_parser!(type_decl, "type MyTuple = { int, real, z: string }"),
        expect_test::expect!["type ::MyTuple = {int, real, z: string};"],
    );
    check(
        &run_parser!(type_decl, "type MyAliasInt = MyInt"),
        expect_test::expect!["type ::MyAliasInt = ::MyInt;"],
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
        expect_test::expect!["::A[::x]..::t.2"],
    );
    check(
        &run_parser!(range, "1+2..3+4"),
        expect_test::expect!["(1 + 2)..(3 + 4)"],
    );
    check(
        &run_parser!(range, "-100.. -if c { 10 } else { 9 }"),
        expect_test::expect!["-100..-if ::c { 10 } else { 9 }"],
    );
    check(
        &run_parser!(range, "1...2"),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `exists`, `false`, `forall`, `ident`, `if`, `int_lit`, `macro_name`, `real_lit`, `str_lit`, `true`, or `{`, found `.`
            @3..4: expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `exists`, `false`, `forall`, `ident`, `if`, `int_lit`, `macro_name`, `real_lit`, `str_lit`, `true`, or `{`
        "#]],
    );

    // Range allow in let decls
    check(
        &run_parser!(let_decl, "let x = 1..2"),
        expect_test::expect![[r#"
            var ::x;
            constraint (::x >= 1);
            constraint (::x <= 2);"#]],
    );

    // Ranges allowed after `in`
    check(
        &run_parser!(expr, "x in 1..2"),
        expect_test::expect!["::x in 1..2"],
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
        expect_test::expect!["::foo::bar"],
    );
    check(
        &run_parser!(path, "_foo_::_bar"),
        expect_test::expect!["::_foo_::_bar"],
    );
    check(&run_parser!(path, "_::_"), expect_test::expect!["::_::_"]);
    check(
        &run_parser!(path, "t2::_3t::t4_::t"),
        expect_test::expect!["::t2::_3t::t4_::t"],
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

#[test]
fn macro_decl() {
    let src = r#"
          macro @foo($x, $y, &z) {
              let a = 5.0 + $x * $y;
              a
          }
      "#;

    let mut context = context!(Vec::<String>::new(), Vec::new());
    let result = parse_and_collect_errors!(yp::MacroDeclParser::new(), src, context);

    assert!(result.is_ok());
    assert!(context.macros.len() == 1);

    check(
        &context.macros[0].to_string(),
        expect_test::expect!["macro ::@foo($x, $y, &z) { let a = 5.0 + $x * $y ; a }"],
    );
}

#[test]
fn macro_decl_good_params() {
    let mut context = context!(Vec::<String>::new(), Vec::new());

    assert!(parse_and_collect_errors!(
        yp::MacroDeclParser::new(),
        r#"macro @foo($a) { x }"#,
        context
    )
    .is_ok());

    assert!(parse_and_collect_errors!(
        yp::MacroDeclParser::new(),
        r#"macro @foo($a,) { x }"#,
        context
    )
    .is_ok());

    assert!(parse_and_collect_errors!(
        yp::MacroDeclParser::new(),
        r#"macro @foo($a,$b) { x }"#,
        context
    )
    .is_ok());

    assert!(parse_and_collect_errors!(
        yp::MacroDeclParser::new(),
        r#"macro @foo($a,$b,) { x }"#,
        context
    )
    .is_ok());

    assert!(parse_and_collect_errors!(
        yp::MacroDeclParser::new(),
        r#"macro @foo($a,&rest) { x }"#,
        context
    )
    .is_ok());

    assert!(parse_and_collect_errors!(
        yp::MacroDeclParser::new(),
        r#"macro @foo($a,&rest,) { x }"#,
        context
    )
    .is_ok());
}

#[test]
fn macro_decl_bad_params() {
    let mut context = context!(Vec::<String>::new(), Vec::new());

    let result = parse_and_collect_errors!(
        yp::MacroDeclParser::new(),
        r#"macro @foo(&rest) { x }"#,
        context
    );
    assert!(result.is_err());
    check(
        &display_errors(&result.unwrap_err()),
        expect_test::expect![[r#"
            expected `macro_param`, found `&rest`
            @11..16: expected `macro_param`
        "#]],
    );

    let result = parse_and_collect_errors!(
        yp::MacroDeclParser::new(),
        r#"macro @foo(&rest,) { x }"#,
        context
    );
    assert!(result.is_err());
    check(
        &display_errors(&result.unwrap_err()),
        expect_test::expect![[r#"
            expected `macro_param`, found `&rest`
            @11..16: expected `macro_param`
        "#]],
    );

    let result = parse_and_collect_errors!(
        yp::MacroDeclParser::new(),
        r#"macro @foo(&rest,$a) { x }"#,
        context
    );
    assert!(result.is_err());
    check(
        &display_errors(&result.unwrap_err()),
        expect_test::expect![[r#"
            expected `macro_param`, found `&rest`
            @11..16: expected `macro_param`
        "#]],
    );

    let result = parse_and_collect_errors!(
        yp::MacroDeclParser::new(),
        r#"macro @foo($a,,$b) { x }"#,
        context
    );
    assert!(result.is_err());
    check(
        &display_errors(&result.unwrap_err()),
        expect_test::expect![[r#"
            expected `)`, `macro_param`, or `macro_param_pack`, found `,`
            @14..15: expected `)`, `macro_param`, or `macro_param_pack`
        "#]],
    );
}

#[test]
fn macro_call() {
    let src = r#"@foo(a * 3; int; <= =>)"#;
    let mut context = context!(Vec::<String>::new(), Vec::new());
    let result = parse_and_collect_errors!(yp::ExprParser::new(), src, context);

    assert!(result.is_ok());
    assert!(
        context
            .macro_calls
            .get(Program::ROOT_II_NAME)
            .unwrap()
            .len()
            == 1
    );

    check(
        &context
            .program
            .root_ii()
            .with_ii(&result.unwrap())
            .to_string(),
        expect_test::expect!["::@foo(...)"],
    );

    check(
        &context
            .macro_calls
            .get(Program::ROOT_II_NAME)
            .unwrap()
            .iter()
            .next()
            .unwrap()
            .1
             .1
            .to_string(),
        expect_test::expect!["::@foo(a * 3; int; <= =>)"],
    );
}

#[test]
fn fn_call() {
    check(
        &run_parser!(yp::LetDeclParser::new(), r#"let x = foo(a*3, c)"#),
        expect_test::expect![[r#"
            var ::x;
            constraint (::x == ::foo((::a * 3), ::c));"#]],
    );

    check(
        &run_parser!(yp::ExprParser::new(), "A::B::foo(-a, b+c)"),
        expect_test::expect!["::A::B::foo(-::a, (::b + ::c))"],
    );
}

#[test]
fn code_blocks() {
    let expr = yp::ExprParser::new();
    let mod_path = vec!["foo".to_string()];

    check(
        &run_parser!(expr, "{ 0 }", mod_path),
        expect_test::expect!["0"],
    );

    check(
        &run_parser!(
            expr,
            "{
                constraint y == 0;
                constraint x > 0.0;
                0.0
            }",
            mod_path
        ),
        expect_test::expect![[r#"
            constraint (::foo::y == 0);
            constraint (::foo::x > 0e0);
            0e0"#]],
    );

    check(
        &run_parser!(expr, "{ constraint { true }; x > 0 }", mod_path),
        expect_test::expect![[r#"
            constraint true;
            (::foo::x > 0)"#]],
    );

    check(
        &run_parser!(
            expr,
            "{
                constraint {
                    constraint y == 0;
                    constraint x > 0.0;
                    true
                };
                0.0
            }",
            mod_path
        ),
        expect_test::expect![[r#"
            constraint (::foo::y == 0);
            constraint (::foo::x > 0e0);
            constraint true;
            0e0"#]],
    );

    // No final expr
    check(
        &run_parser!(expr, "{ constraint x == 0; }", mod_path),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `constraint`, `exists`, `false`, `forall`, `ident`, `if`, `int_lit`, `macro_name`, `real_lit`, `str_lit`, `true`, or `{`, found `}`
            @21..22: expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `constraint`, `exists`, `false`, `forall`, `ident`, `if`, `int_lit`, `macro_name`, `real_lit`, `str_lit`, `true`, or `{`
        "#]],
    );

    // Use of `let`
    check(
        &run_parser!(expr, "{ let x = 0; 5 }", mod_path),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `constraint`, `exists`, `false`, `forall`, `ident`, `if`, `int_lit`, `macro_name`, `real_lit`, `str_lit`, `true`, `{`, or `}`, found `let`
            @2..5: expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `constraint`, `exists`, `false`, `forall`, `ident`, `if`, `int_lit`, `macro_name`, `real_lit`, `str_lit`, `true`, `{`, or `}`
        "#]],
    );

    check(
        &run_parser!(expr, "{}", mod_path),
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
        expect_test::expect!["if ::c { 1 } else { 0 }"],
    );

    check(
        &run_parser!(expr, "if c { if c { 1 } else { 0 } } else { 2 }"),
        expect_test::expect!["if ::c { if ::c { 1 } else { 0 } } else { 2 }"],
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
        expect_test::expect!["int[::MyEnum]"],
    );

    check(
        &run_parser!(type_, r#"int[N]"#),
        expect_test::expect!["int[::N]"],
    );

    check(
        &run_parser!(type_, r#"string[foo()][{ 7 }][if true { 1 } else { 2 }]"#),
        expect_test::expect!["string[if true { 1 } else { 2 }][7][::foo()]"],
    );

    check(
        &run_parser!(type_, r#"real[N][9][M][3]"#),
        expect_test::expect!["real[3][::M][9][::N]"],
    );

    check(
        &run_parser!(type_, r#"{int, { real, string }}[N][9]"#),
        expect_test::expect!["{int, {real, string}}[9][::N]"],
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
        expect_test::expect!["[[::foo(), 2], [if true { 1 } else { 2 }, ::t.0]]"],
    );
}

#[test]
fn array_element_accesses() {
    let expr = yp::ExprParser::new();

    check(
        &run_parser!(expr, r#"a[5]"#),
        expect_test::expect!["::a[5]"],
    );

    check(
        &run_parser!(expr, r#"a[N][5][t.0]"#),
        expect_test::expect!["::a[::N][5][::t.0]"],
    );

    check(
        &run_parser!(expr, r#"{ a }[N][foo()][M][4]"#),
        expect_test::expect!["::a[::N][::foo()][::M][4]"],
    );

    check(
        &run_parser!(expr, r#"foo()[{ M }][if true { 1 } else { 3 }]"#),
        expect_test::expect!["::foo()[::M][if true { 1 } else { 3 }]"],
    );

    check(
        &run_parser!(yp::LetDeclParser::new(), r#"let x = a[]"#),
        expect_test::expect![[r#"
            missing array or map index
            @8..11: missing array or map element index
        "#]],
    );

    check(
        &run_parser!(expr, r#"a[MyEnum::Variant1]"#),
        expect_test::expect!["::a[::MyEnum::Variant1]"],
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
        expect_test::expect!["{42, if ::c { 2 } else { 3 }, ::foo()}"],
    );

    check(
        &run_parser!(expr, r#"{ x: { 42 }, y: if c { 2 } else { 3 }, z: foo() }"#),
        expect_test::expect!["{x: 42, y: if ::c { 2 } else { 3 }, z: ::foo()}"],
    );
}

#[test]
fn tuple_field_accesses() {
    let expr = yp::ExprParser::new();

    check(
        &run_parser!(expr, r#"t.0 + t.9999999 + t.x"#),
        expect_test::expect!["((::t.0 + ::t.9999999) + ::t.x)"],
    );

    check(
        &run_parser!(expr, r#"t.1.1"#),
        expect_test::expect!["::t.1.1"],
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
        expect_test::expect!["::t.0.0"],
    );

    check(
        &run_parser!(expr, r#"t.x .y"#),
        expect_test::expect!["::t.x.y"],
    );

    check(
        &run_parser!(expr, "t \r .1 .2.2. \n 3 . \t 13 . 1.1"),
        expect_test::expect!["::t.1.2.2.3.13.1.1"],
    );

    check(
        &run_parser!(expr, "t \r .x .1.2. \n w . \t t. 3.4"),
        expect_test::expect!["::t.x.1.2.w.t.3.4"],
    );

    check(
        &run_parser!(expr, r#"foo().0.1"#),
        expect_test::expect!["::foo().0.1"],
    );

    check(
        &run_parser!(expr, r#"foo().a.b.0.1"#),
        expect_test::expect!["::foo().a.b.0.1"],
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
        expect_test::expect!["::a"],
    );

    check(
        &run_parser!(expr, r#"cond { else => { a } }"#),
        expect_test::expect!["::a"],
    );

    check(
        &run_parser!(expr, r#"cond { a => b, else => c }"#),
        expect_test::expect!["if ::a { ::b } else { ::c }"],
    );

    check(
        &run_parser!(expr, r#"cond { a => { b }, else => c, }"#),
        expect_test::expect!["if ::a { ::b } else { ::c }"],
    );

    check(
        &run_parser!(expr, r#"cond { a => b, { true } => d, else => f, }"#),
        expect_test::expect!["if ::a { ::b } else { if true { ::d } else { ::f } }"],
    );

    check(
        &run_parser!(expr, r#"cond { a => b, }"#),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `else`, `exists`, `false`, `forall`, `ident`, `if`, `int_lit`, `macro_name`, `real_lit`, `str_lit`, `true`, or `{`, found `}`
            @15..16: expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `else`, `exists`, `false`, `forall`, `ident`, `if`, `int_lit`, `macro_name`, `real_lit`, `str_lit`, `true`, or `{`
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
        expect_test::expect!["(::t.0.1 as real * ::a[5][3] as int)"],
    );

    check(
        &run_parser!(let_decl, r#"let x = foo() as real as { int, real }"#),
        expect_test::expect![[r#"
            var ::x;
            constraint (::x == ::foo() as real as {int, real});"#]],
    );

    check(
        &run_parser!(let_decl, r#"let x = 5 as"#),
        expect_test::expect![[r#"
            expected `::`, `b256_ty`, `bool_ty`, `ident`, `int_ty`, `real_ty`, `string_ty`, or `{`, found `end of file`
            @12..12: expected `::`, `b256_ty`, `bool_ty`, `ident`, `int_ty`, `real_ty`, `string_ty`, or `{`
        "#]],
    );
}

#[test]
fn in_expr() {
    let expr = yp::ExprParser::new();

    check(
        &run_parser!(expr, r#"x in a"#),
        expect_test::expect!["::x in ::a"],
    );

    check(
        &run_parser!(expr, r#"x in { 1, 2 }"#),
        expect_test::expect!["::x in {1, 2}"],
    );

    check(
        &run_parser!(expr, r#"x in [ 1, 2 ] in { true, false }"#),
        expect_test::expect!["::x in [1, 2] in {true, false}"],
    );

    check(
        &run_parser!(expr, r#"x as int in { 1, 2 }"#),
        expect_test::expect!["::x as int in {1, 2}"],
    );

    check(
        &run_parser!(expr, r#"[1] in foo() in [[1]]"#),
        expect_test::expect!["[1] in ::foo() in [[1]]"],
    );

    check(
        &run_parser!(yp::LetDeclParser::new(), r#"let x = 5 in"#),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `exists`, `false`, `forall`, `ident`, `if`, `int_lit`, `macro_name`, `real_lit`, `str_lit`, `true`, or `{`, found `end of file`
            @12..12: expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `exists`, `false`, `forall`, `ident`, `if`, `int_lit`, `macro_name`, `real_lit`, `str_lit`, `true`, or `{`
        "#]],
    );
}

#[test]
fn forall_expr() {
    let expr = yp::ExprParser::new();
    check(
        &run_parser!(expr, r#"forall i in 0..3 { true }"#),
        expect_test::expect!["forall i in 0..3, { true }"],
    );

    check(
        &run_parser!(expr, r#"forall i in 0..3, j in i..k { true }"#),
        expect_test::expect!["forall i in 0..3, j in ::i..::k, { true }"],
    );

    check(
        &run_parser!(expr, r#"forall i in 0..3 where i > 4 { true }"#),
        expect_test::expect!["forall i in 0..3, where (::i > 4) { true }"],
    );

    check(
        &run_parser!(
            expr,
            r#"forall i in 0..3, j in 0..3 where i > 2, j < 3, i != j && true { A[i] > A[j] }"#
        ),
        expect_test::expect!["forall i in 0..3, j in 0..3, where (::i > 2), (::j < 3), ((::i != ::j) && true) { (::A[::i] > ::A[::j]) }"],
    );

    check(
        &run_parser!(expr, r#"forall { true }"#),
        expect_test::expect![[r#"
            expected `ident`, found `{`
            @7..8: expected `ident`
        "#]],
    );

    check(
        &run_parser!(expr, r#"forall range { true }"#),
        expect_test::expect![[r#"
            expected `in`, found `{`
            @13..14: expected `in`
        "#]],
    );

    check(
        &run_parser!(expr, r#"forall i in 0..3 { constraint x; true }"#),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `exists`, `false`, `forall`, `ident`, `if`, `int_lit`, `macro_name`, `real_lit`, `str_lit`, `true`, or `{`, found `constraint`
            @19..29: expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `exists`, `false`, `forall`, `ident`, `if`, `int_lit`, `macro_name`, `real_lit`, `str_lit`, `true`, or `{`
        "#]],
    );
}

#[test]
fn exists_expr() {
    let expr = yp::ExprParser::new();
    check(
        &run_parser!(expr, r#"exists i in 0..3 { true }"#),
        expect_test::expect!["exists i in 0..3, { true }"],
    );

    check(
        &run_parser!(expr, r#"exists i in 0..3, j in i..k { true }"#),
        expect_test::expect!["exists i in 0..3, j in ::i..::k, { true }"],
    );

    check(
        &run_parser!(expr, r#"exists i in 0..3 where i > 4 { true }"#),
        expect_test::expect!["exists i in 0..3, where (::i > 4) { true }"],
    );

    check(
        &run_parser!(
            expr,
            r#"exists i in 0..3, j in 0..3 where i > 2, j < 3, i != j && true { A[i] > A[j] }"#
        ),
        expect_test::expect!["exists i in 0..3, j in 0..3, where (::i > 2), (::j < 3), ((::i != ::j) && true) { (::A[::i] > ::A[::j]) }"],
    );

    check(
        &run_parser!(expr, r#"exists { true }"#),
        expect_test::expect![[r#"
            expected `ident`, found `{`
            @7..8: expected `ident`
        "#]],
    );

    check(
        &run_parser!(expr, r#"exists range { true }"#),
        expect_test::expect![[r#"
            expected `in`, found `{`
            @13..14: expected `in`
        "#]],
    );

    check(
        &run_parser!(expr, r#"exists i in 0..3 { constraint x; true }"#),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `exists`, `false`, `forall`, `ident`, `if`, `int_lit`, `macro_name`, `real_lit`, `str_lit`, `true`, or `{`, found `constraint`
            @19..29: expected `!`, `(`, `+`, `-`, `::`, `[`, `cond`, `exists`, `false`, `forall`, `ident`, `if`, `int_lit`, `macro_name`, `real_lit`, `str_lit`, `true`, or `{`
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
        &run_parser!(yp::PintParser::new(), src),
        expect_test::expect![[r#"
            var ::low_val: real;
            var ::high_val;
            constraint (::low_val == 1.23e0);
            constraint (::high_val == 4.56e0);
            constraint (::mid > (::low_val * 2e0));
            constraint (::mid < ::high_val);
            solve minimize ::mid;"#]],
    );
}

#[test]
fn intents_decls() {
    let src = r#"
intent Foo { }
intent Bar { 
    let x: int;
    constraint x == 1;
}
intent Baz {
    enum MyEnum = A | B;
    type MyType = MyEnum;
}
"#;

    check(
        &run_parser!(yp::PintParser::new(), src),
        expect_test::expect![[r#"

            intent ::Bar {
                var ::x: int;
                constraint (::x == 1);
            }

            intent ::Baz {
                enum ::MyEnum = A | B;
                type ::MyType = ::MyEnum;
            }

            intent ::Foo {
            }"#]],
    );
}

//#[test]
//fn fn_errors() {
//    check(
//        &run_parser!(pint_program(), "fn foo() {5}"),
//        expect_test::expect![[r#"
//            expected `->`, found `{`
//            @9..10: expected `->`
//        "#]],
//    );
//
//    check(
//        &run_parser!(pint_program(), "fn foo() -> real {}"),
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
let low = 1.0;
"#;

    check(
        &run_parser!(yp::PintParser::new(), src),
        expect_test::expect![[r#"
            var ::high;
            var ::low;
            constraint (::low < ::high);
            constraint (::high == 2e0);
            constraint (::low == 1e0);
            solve maximize ::low;"#]],
    );
}

#[test]
fn keywords_as_identifiers_errors() {
    // TODO: Ideally, we emit a special error here. Instead, we currently get a generic "expected..
    // found" error.
    for keyword in KEYWORDS {
        let src = format!("let {keyword} = 5;").to_string();
        assert_eq!(
            &run_parser!(yp::PintParser::new(), &src),
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
        expect_test::expect![[r#"
            integer literal is too large
            @11..51: integer literal is too large
            value exceeds limit of `9,223,372,036,854,775,807`
        "#]],
    );

    check(
        &run_parser!(let_decl, "let blah = 0xfeedbadfd2adeadc"),
        // Confirmed by using the Python REPL to convert from hex to dec...
        expect_test::expect![[r#"
            var ::blah;
            constraint (::blah == -77200148120343844);"#]],
    );

    check(
        &run_parser!(
            let_decl,
            "let blah = 0xfeedbadfd2adeadcafed00dbabefacefeedbadf00d2adeadcafed00dbabeface"
        ),
        expect_test::expect![[r#"
            var ::blah;
            constraint (::blah == 0xFEEDBADFD2ADEADCAFED00DBABEFACEFEEDBADF00D2ADEADCAFED00DBABEFACE);"#]],
    );

    check(
        &run_parser!(
            yp::ExprParser::new(),
            "0b1101001010010101010101010101010100110100110101010101010101010101 + \
             0b0100100101011010101010100101010101001010010010100101001010010010"
        ),
        expect_test::expect!["(-3272615729767819947 + 5285724395968025234)"],
    );
}

#[test]
fn error_recovery() {
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
        &run_parser!(yp::PintParser::new(), src),
        expect_test::expect![[r#"
            type annotation or initializer needed for variable `untyped`
            @1..12: type annotation or initializer needed
            consider giving `untyped` an explicit type or an initializer
            symbol `clash` has already been declared
            @18..23: previous declaration of the symbol `clash` here
            @33..38: `clash` redeclared here
            `clash` must be declared or imported only once in this scope
            symbol `clash` has already been declared
            @18..23: previous declaration of the symbol `clash` here
            @48..53: `clash` redeclared here
            `clash` must be declared or imported only once in this scope
            empty tuple types are not allowed
            @76..78: empty tuple type found
            empty tuple expressions are not allowed
            @81..83: empty tuple expression found
            empty array types are not allowed
            @102..107: empty array type found
            missing array or map index
            @132..135: missing array or map element index
            invalid integer `0x5` as tuple index
            @163..166: invalid integer as tuple index
            invalid value `1e5` as tuple index
            @191..194: invalid value as tuple index
            expected `:`, `;`, or `=`, found `end of file`
            @211..211: expected `:`, `;`, or `=`
        "#]],
    );
}
