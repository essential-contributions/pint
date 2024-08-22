use crate::{
    error::{Error, Handler, ReportableError},
    lexer::{self, KEYWORDS},
    parser::ParserContext,
    predicate::{Contract, DisplayWithContract},
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
    ($contract: expr, $mod_path: expr, $use_paths: expr) => {{
        ParserContext {
            mod_path: &$mod_path,
            mod_prefix: &(!$mod_path.is_empty())
                .then(|| format!("::{}::", $mod_path.join("::")))
                .unwrap_or("::".to_string()),
            local_scope: None,
            contract: &mut $contract,
            current_pred_key: None,
            macros: &mut vec![],
            macro_calls: &mut BTreeMap::default(),
            span_from: &|l, r| Span::new(Rc::from(Path::new("")), l..r),
            use_paths: &mut $use_paths,
            next_paths: &mut vec![],
        }
    }};
}

/// Run a parser and print the following, in order:
/// - All use statements, broken down into individual paths
/// - The full content of the `Predicate` post parsing (useful for decls).
/// - The output of the parser itself (useful for expressions and types).
#[cfg(test)]
macro_rules! run_parser {
    ($parser: expr, $source: expr) => {{
        let mod_path = Vec::<String>::new();
        run_parser!(@internal $parser, $source, &mod_path)
    }};

    ($parser: expr, $source: expr, $mod_path: expr) => {{
        run_parser!(@internal $parser, $source, $mod_path)
    }};

    (@internal $parser: expr, $source: expr, $mod_path: expr) => {{
        let source = if $parser.1 == "" {
            $source.to_owned()
        } else {
            "###".to_owned() + $parser.1 + "### " + $source
        };
        let mut contract = Contract::default();
        let mut use_paths = Vec::new();
        let mut context = context!(contract, $mod_path, &mut use_paths);
        let result = parse_and_collect_errors!($parser.0, &source, context);

        let parser_output = match result {
            Ok(item) => {
                let result =
                    format!("{}{}",
                        context.contract,
                        context.contract.with_ctrct(&item)
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
impl DisplayWithContract for () {
    fn fmt(&self, _f: &mut std::fmt::Formatter, _contract: &Contract) -> std::fmt::Result {
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
    let type_ = (yp::TestDelegateParser::new(), "type");

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
            @11..13: empty tuple type found
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
    check(
        &run_parser!(type_, "(int => bool)"),
        expect_test::expect!["( int => bool )"],
    );
}

#[test]
fn storage_types() {
    let storage_var_type = (yp::TestDelegateParser::new(), "svtype");

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
            @13..15: empty tuple type found
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

    check(
        &run_parser!(storage_var_type, "int[]"),
        expect_test::expect!["int[]"],
    );

    check(
        &run_parser!(storage_var_type, "b256[]"),
        expect_test::expect!["b256[]"],
    );

    check(
        &run_parser!(storage_var_type, "bool[]"),
        expect_test::expect!["bool[]"],
    );

    // Multi dimensional vectors are not yet supported
    check(
        &run_parser!(storage_var_type, "int[][]"),
        expect_test::expect![[r#"
            expected something else, found `[`
            @18..19: expected something else
        "#]],
    );
}

#[test]
fn immediates() {
    let immediate = (yp::TestDelegateParser::new(), "expr");

    check(&run_parser!(immediate, "nil"), expect_test::expect!["nil"]);

    check(
        &run_parser!(immediate, "true"),
        expect_test::expect!["true"],
    );
    check(
        &run_parser!(immediate, "false"),
        expect_test::expect!["false"],
    );

    check(&run_parser!(immediate, "0x88"), expect_test::expect!["136"]);
    check(&run_parser!(immediate, "0b111"), expect_test::expect!["7"]);
    check(&run_parser!(immediate, "1"), expect_test::expect!["1"]);

    check(
        &run_parser!(immediate, "0x000_88"),
        expect_test::expect!["136"],
    );
    check(
        &run_parser!(immediate, "0b000_111"),
        expect_test::expect!["7"],
    );
    check(
        &run_parser!(immediate, "1_000"),
        expect_test::expect!["1000"],
    );

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

    check(
        &run_parser!(immediate, "0x0_000_000_011"),
        expect_test::expect!["17"],
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

    check(
        &run_parser!(
            immediate,
            "0xF_FFF_FFF_FFF_FFF_FFF_FFF_FFF_FFF_FFF_FFF_FFF_FFF_FFF_FFF_FFF_FFF_FFF_FFF_FFF_FFF_FFF"
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

    check(
        &run_parser!(immediate, "0b1_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_00000_0000_0000_0000_0000_00000_0000_0000_00000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0011"),
        expect_test::expect!["0x8000000000000000000000000000000000000000000000000000000000000003"],
    );

    // Bad lengths
    check(
        &run_parser!(immediate, "0x4f3f4f3f4f3f4f3f4f3f4f3f4f"),
        expect_test::expect![[r#"
            unexpected hexadecimal integer literal length
            @11..39: 26 is not a valid number of digits in a hexadecimal integer literal
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
            @11..78: 65 is not a valid number of digits in a hexadecimal integer literal
            number of digits must be either 64 or between 1 and 16
        "#]],
    );

    check(
            &run_parser!(immediate, "0b1000000000000000000000000000000000000000000000000000000000000000000000000000000000000"),
            expect_test::expect![[r#"
                unexpected binary integer literal length
                @11..98: 85 is not a valid number of digits in a binary integer literal
                number of digits must be either 256 or between 1 and 64
            "#]],
        );

    check(
        &run_parser!(immediate, "0b11000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011"),
        expect_test::expect![[r#"
            unexpected binary integer literal length
            @11..270: 257 is not a valid number of digits in a binary integer literal
            number of digits must be either 256 or between 1 and 64
        "#]],
    );

    check(
        &run_parser!(immediate, "9223372036854775808"),
        expect_test::expect![[r#"
            integer literal is too large
            @11..30: integer literal is too large
            value exceeds limit of `9,223,372,036,854,775,807`
        "#]],
    );

    check(
        &run_parser!(immediate, "2.34e2_000"),
        expect_test::expect!["inf"],
    );

    // real literals

    check(
        &run_parser!(immediate, "2.05"),
        expect_test::expect!["2.05e0"],
    );

    check(&run_parser!(immediate, "3.0"), expect_test::expect!["3e0"]);

    check(
        &run_parser!(immediate, "3.5e-3"),
        expect_test::expect!["3.5e-3"],
    );

    check(
        &run_parser!(immediate, "2.3E6"),
        expect_test::expect!["2.3e6"],
    );

    check(
        &run_parser!(immediate, "0.45"),
        expect_test::expect!["4.5e-1"],
    );

    check(
        &run_parser!(immediate, "0.000_045"),
        expect_test::expect!["4.5e-5"],
    );

    check(
        &run_parser!(immediate, "2.34_e5"),
        expect_test::expect!["2.34e5"],
    );

    check(
        &run_parser!(immediate, "2.34e5_"),
        expect_test::expect!["2.34e5"],
    );

    check(
        &run_parser!(immediate, "2_000.45"),
        expect_test::expect!["2.00045e3"],
    );

    check(
        &run_parser!(immediate, "2_000.450_00e0_000"),
        expect_test::expect!["2.00045e3"],
    );
}

#[test]
fn use_statements() {
    let pint = (yp::PintParser::new(), "");
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
            expected `::`, `an identifier`, `macro_name`, `self`, or `{`, found `;`
            @4..5: expected `::`, `an identifier`, `macro_name`, `self`, or `{`
        "#]],
    );

    check(
        &run_parser!(pint, "use ::;", mod_path),
        expect_test::expect![[r#"
            expected `an identifier`, `macro_name`, `self`, or `{`, found `;`
            @6..7: expected `an identifier`, `macro_name`, `self`, or `{`
        "#]],
    );

    check(
        &run_parser!(pint, "use a::;", mod_path),
        expect_test::expect![[r#"
            expected `an identifier`, `macro_name`, `self`, or `{`, found `;`
            @7..8: expected `an identifier`, `macro_name`, `self`, or `{`
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
            expected `an identifier`, found `self`
            @64..68: expected `an identifier`
        "#]],
    );
}

#[test]
fn storage_decl() {
    let pint = (yp::PintParser::new(), "");

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
fn interface() {
    let pint = (yp::PintParser::new(), "");

    let src = r#"
interface Foo {
    storage {
        integer: int,
        boolean: bool,
    }

    predicate Bar;
    predicate Baz { pub var x: int; }
}
"#;

    check(
        &run_parser!(pint, src),
        expect_test::expect![[r#"
            interface ::Foo {
                storage {
                    integer: int,
                    boolean: bool,
                }
                predicate Bar;
                predicate Baz {
                    pub var x: int;
                }
            }"#]],
    );

    // No trailing comma
    check(
        &run_parser!(
            pint,
            r#"
            interface Foo {
                 storage { x: int, y: bool }
            }"#
        ),
        expect_test::expect![[r#"
            interface ::Foo {
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
            interface Foo {
                 storage { x: int, y: bool }
            }
            interface Bar {
                 storage { x: int, y: bool }
            }
            "#
        ),
        expect_test::expect![[r#"
            interface ::Foo {
                storage {
                    x: int,
                    y: bool,
                }
            }
            interface ::Bar {
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
            interface Foo {
                storage { x: int, y: bool, z: b256, w: (int => (bool => b256)) }
            }"#
        ),
        expect_test::expect![[r#"
            interface ::Foo {
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
            interface Foo { storage { } predicate Baz { } }"#
        ),
        expect_test::expect![[r#"
            interface ::Foo {
                storage {
                }
                predicate Baz;
            }"#]],
    );
    check(
        &run_parser!(
            pint,
            r#"
            interface Foo { storage { } }
            interface Foo { storage { } }
            "#
        ),
        expect_test::expect![[r#"
            symbol `Foo` has already been declared
            @23..26: previous declaration of the symbol `Foo` here
            @65..68: `Foo` redeclared here
            `Foo` must be declared or imported only once in this scope
        "#]],
    );

    check(
        &run_parser!(
            pint,
            r#"
            interface Foo {
                storage { x, y }
            }"#
        ),
        expect_test::expect![[r#"
            expected `:`, found `,`
            @56..57: expected `:`
        "#]],
    );

    check(
        &run_parser!(
            pint,
            r#"
            interface Foo {
                predicate Baz { var x: int }
            }"#
        ),
        expect_test::expect![[r#"
            expected `pub`, or `}`, found `var`
            @61..64: expected `pub`, or `}`
        "#]],
    );

    check(
        &run_parser!(
            pint,
            r#"
            interface Foo {
                predicate Baz { pub var x; }
            }"#
        ),
        expect_test::expect![[r#"
            expected `:`, found `;`
            @70..71: expected `:`
        "#]],
    );

    check(
        &run_parser!(
            pint,
            r#"
            interface Foo {
                predicate Baz { pub var x: int; pub var y: b256; }
            }"#
        ),
        expect_test::expect![[r#"
            interface ::Foo {
                predicate Baz {
                    pub var x: int;
                    pub var y: b256;
                }
            }"#]],
    );

    check(
        &run_parser!(
            pint,
            r#"
            interface Foo { }"#
        ),
        expect_test::expect![[r#"
            interface ::Foo {
            }"#]],
    );

    check(
        &run_parser!(
            pint,
            r#"
            interface Foo {
                storage { }
                storage { }
            }"#
        ),
        expect_test::expect![[r#"
            `storage` block has already been declared
            @45..56: previous declaration of a `storage` block here
            @73..84: another `storage` block is declared here
        "#]],
    );
}

#[test]
fn interface_instance() {
    let pint = (yp::PintParser::new(), "");

    let src = r#"predicate test {
        interface FooInstance =
            FooInstance(0x0000111100001111000011110000111100001111000011110000111100001111);
    }"#;

    check(
        &run_parser!(pint, src),
        expect_test::expect![[r#"

            predicate ::test {
                interface ::FooInstance = ::FooInstance(0x0000111100001111000011110000111100001111000011110000111100001111)
            }"#]],
    );

    let src = r#"predicate test {
        var addr: b256;
        interface FooInstance =
            ::path::to::FooInstance(addr);
    }"#;

    check(
        &run_parser!(pint, src),
        expect_test::expect![[r#"

            predicate ::test {
                interface ::FooInstance = ::path::to::FooInstance(::addr)
                var ::addr: b256;
            }"#]],
    );
}

#[test]
fn predicate_instance() {
    let pint = (yp::PintParser::new(), "");

    let src = r#" predicate test {
        predicate FooInstance =
            InterfaceInstance::FooInstance(0x0000111100001111000011110000111100001111000011110000111100001111);
    }"#;

    check(
        &run_parser!(pint, src),
        expect_test::expect![[r#"

            predicate ::test {
                predicate ::FooInstance = ::InterfaceInstance::FooInstance(0x0000111100001111000011110000111100001111000011110000111100001111)
                var __::FooInstance_pathway: int;
            }"#]],
    );

    let src = r#"predicate test {
        predicate FooInstance =
            ::InterfaceInstance::FooInstance(0x0000111100001111000011110000111100001111000011110000111100001111);
    }"#;

    check(
        &run_parser!(pint, src),
        expect_test::expect![[r#"

            predicate ::test {
                predicate ::FooInstance = ::InterfaceInstance::FooInstance(0x0000111100001111000011110000111100001111000011110000111100001111)
                var __::FooInstance_pathway: int;
            }"#]],
    );

    let src = r#"predicate test {
        var addr: b256;
        predicate FooInstance = path::to::FooInstance(addr);
    }"#;

    check(
        &run_parser!(pint, src),
        expect_test::expect![[r#"

            predicate ::test {
                predicate ::FooInstance = ::path::to::FooInstance(::addr)
                var ::addr: b256;
                var __::FooInstance_pathway: int;
            }"#]],
    );

    let src = r#"predicate test {
        var addr: b256;
        predicate FooInstance = ::path::to::FooInstance(addr);
    }"#;

    check(
        &run_parser!(pint, src),
        expect_test::expect![[r#"

            predicate ::test {
                predicate ::FooInstance = ::path::to::FooInstance(::addr)
                var ::addr: b256;
                var __::FooInstance_pathway: int;
            }"#]],
    );

    let src = r#"predicate test {
        var addr: b256;
        predicate FooInstance = FooInstance(addr);
    }"#;

    check(
        &run_parser!(pint, src),
        expect_test::expect![[r#"
            path `FooInstance` to a predicate interface is too short
            @49..90: path `FooInstance` is too short and cannot refer to a predicate interface
            a path to a predicate interface must contain a path to an interface instance followed by the name of the predicate, separated by a `::`
        "#]],
    );

    let src = r#"predicate test {
        var addr: b256;
        predicate FooInstance = ::FooInstance(addr);
    }"#;

    check(
        &run_parser!(pint, src),
        expect_test::expect![[r#"
            path `::FooInstance` to a predicate interface is too short
            @49..92: path `::FooInstance` is too short and cannot refer to a predicate interface
            a path to a predicate interface must contain a path to an interface instance followed by the name of the predicate, separated by a `::`
        "#]],
    );
}

#[test]
fn storage_access() {
    let expr = (yp::TestDelegateParser::new(), "state");

    check(
        &run_parser!(expr, r#"storage::foo"#),
        expect_test::expect!["storage::foo"],
    );

    check(
        &run_parser!(expr, r#"mut storage::foo.0.3"#),
        expect_test::expect!["mut storage::foo.0.3"],
    );

    check(
        &run_parser!(expr, r#"storage::balances[0x111]"#),
        expect_test::expect!["storage::balances[273]"],
    );

    check(
        &run_parser!(
            expr,
            r#"mut storage::balances[0x111][__this_address()][t[3].5].2"#
        ),
        expect_test::expect!["mut storage::balances[273][__this_address()][::t[3].5].2"],
    );

    check(
        &run_parser!(expr, r#"__this_address()"#),
        expect_test::expect!["__this_address()"],
    );

    let pint = (yp::PintParser::new(), "");

    check(
        &run_parser!(pint, r#"predicate test { var x = storage::foo; }"#),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `[`, `a boolean`, `a literal`, `an identifier`, `cond`, `exists`, `forall`, `intrinsic_name`, `macro_name`, or `{`, found `storage`
            @25..32: expected `!`, `(`, `+`, `-`, `::`, `[`, `a boolean`, `a literal`, `an identifier`, `cond`, `exists`, `forall`, `intrinsic_name`, `macro_name`, or `{`
        "#]],
    );

    check(
        &run_parser!(pint, r#"predicate test { var x = storage::map[4][3]; }"#),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `[`, `a boolean`, `a literal`, `an identifier`, `cond`, `exists`, `forall`, `intrinsic_name`, `macro_name`, or `{`, found `storage`
            @25..32: expected `!`, `(`, `+`, `-`, `::`, `[`, `a boolean`, `a literal`, `an identifier`, `cond`, `exists`, `forall`, `intrinsic_name`, `macro_name`, or `{`
        "#]],
    );

    check(
        &run_parser!(pint, r#"constraint storage::map[69] == 0;"#),
        expect_test::expect![[r#"
            expected `::`, `an identifier`, `const`, `enum`, `interface`, `macro`, `macro_name`, `predicate`, `storage`, `type`, or `use`, found `constraint`
            @0..10: expected `::`, `an identifier`, `const`, `enum`, `interface`, `macro`, `macro_name`, `predicate`, `storage`, `type`, or `use`
        "#]],
    );
}

#[test]
fn external_storage_access() {
    let expr = (yp::TestDelegateParser::new(), "state");

    check(
        &run_parser!(expr, r#"Foo::storage::foo"#),
        expect_test::expect!["::Foo::storage::foo"],
    );

    check(
        &run_parser!(expr, r#"Foo::Bar::storage::balances[0x111]"#),
        expect_test::expect!["::Foo::Bar::storage::balances[273]"],
    );

    check(
        &run_parser!(
            expr,
            r#"::Foo::storage::balances[0x111][__this_address()][t[3].5]"#
        ),
        expect_test::expect!["::Foo::storage::balances[273][__this_address()][::t[3].5]"],
    );

    let pint = (yp::PintParser::new(), "");

    check(
        &run_parser!(pint, r#"predicate test { var x = ::Foo::storage::foo; }"#),
        expect_test::expect![[r#"
            expected `an identifier`, or `macro_name`, found `storage`
            @32..39: expected `an identifier`, or `macro_name`
        "#]],
    );

    check(
        &run_parser!(
            pint,
            r#"predicate test { var x = Bar::storage::map[4][3]; }"#
        ),
        expect_test::expect![[r#"
            expected `an identifier`, or `macro_name`, found `storage`
            @30..37: expected `an identifier`, or `macro_name`
        "#]],
    );

    check(
        &run_parser!(pint, r#"constraint ::Foo::storage::map[69] == 0;"#),
        expect_test::expect![[r#"
            expected `::`, `an identifier`, `const`, `enum`, `interface`, `macro`, `macro_name`, `predicate`, `storage`, `type`, or `use`, found `constraint`
            @0..10: expected `::`, `an identifier`, `const`, `enum`, `interface`, `macro`, `macro_name`, `predicate`, `storage`, `type`, or `use`
        "#]],
    );
}

#[test]
fn var_decls() {
    let mod_path = vec!["foo".to_string()];
    let pint = (yp::PintParser::new(), "");

    check(
        &run_parser!(pint, "predicate test { var blah; }", mod_path),
        expect_test::expect![[r#"
            type annotation or initializer needed for variable `blah`
            @17..25: type annotation or initializer needed
            consider giving `blah` an explicit type or an initializer
        "#]],
    );
    check(
        &run_parser!(pint, "predicate test { var blah = 1.0; }", mod_path),
        expect_test::expect![[r#"

            predicate ::foo::test {
                var ::foo::blah;
                constraint (::foo::blah == 1e0);
            }"#]],
    );
    check(
        &run_parser!(pint, "predicate test { var blah: real = 1.0; }", mod_path),
        expect_test::expect![[r#"

            predicate ::foo::test {
                var ::foo::blah: real;
                constraint (::foo::blah == 1e0);
            }"#]],
    );
    check(
        &run_parser!(pint, "predicate test { var blah: real; }", mod_path),
        expect_test::expect![[r#"

            predicate ::foo::test {
                var ::foo::blah: real;
            }"#]],
    );
    check(
        &run_parser!(pint, "predicate test { var blah = 1; }", mod_path),
        expect_test::expect![[r#"

            predicate ::foo::test {
                var ::foo::blah;
                constraint (::foo::blah == 1);
            }"#]],
    );
    check(
        &run_parser!(pint, "predicate test { var blah: int = 1; }", mod_path),
        expect_test::expect![[r#"

            predicate ::foo::test {
                var ::foo::blah: int;
                constraint (::foo::blah == 1);
            }"#]],
    );
    check(
        &run_parser!(pint, "predicate test { var blah: int; }", mod_path),
        expect_test::expect![[r#"

            predicate ::foo::test {
                var ::foo::blah: int;
            }"#]],
    );
    check(
        &run_parser!(pint, "predicate test { var blah = true; }", mod_path),
        expect_test::expect![[r#"

            predicate ::foo::test {
                var ::foo::blah;
                constraint (::foo::blah == true);
            }"#]],
    );
    check(
        &run_parser!(pint, "predicate test { var blah: bool = false; }", mod_path),
        expect_test::expect![[r#"

            predicate ::foo::test {
                var ::foo::blah: bool;
                constraint (::foo::blah == false);
            }"#]],
    );
    check(
        &run_parser!(pint, "predicate test { var blah: bool; }", mod_path),
        expect_test::expect![[r#"

            predicate ::foo::test {
                var ::foo::blah: bool;
            }"#]],
    );
    check(
        &run_parser!(pint, r#"predicate test { var blah = "hello"; }"#, mod_path),
        expect_test::expect![[r#"

            predicate ::foo::test {
                var ::foo::blah;
                constraint (::foo::blah == "hello");
            }"#]],
    );
    check(
        &run_parser!(
            pint,
            r#"predicate test { var blah: string = "hello"; }"#,
            mod_path
        ),
        expect_test::expect![[r#"

            predicate ::foo::test {
                var ::foo::blah: string;
                constraint (::foo::blah == "hello");
            }"#]],
    );
    check(
        &run_parser!(pint, r#"predicate test { var blah: string; }"#, mod_path),
        expect_test::expect![[r#"

            predicate ::foo::test {
                var ::foo::blah: string;
            }"#]],
    );
}

#[test]
fn pub_var_decls() {
    let mod_path = vec!["foo".to_string()];
    let pint = (yp::PintParser::new(), "");

    check(
        &run_parser!(pint, "predicate test { pub var blah; }", mod_path),
        expect_test::expect![[r#"
            type annotation or initializer needed for variable `blah`
            @17..29: type annotation or initializer needed
            consider giving `blah` an explicit type or an initializer
        "#]],
    );
    check(
        &run_parser!(pint, "predicate test { pub var blah = 1.0; }", mod_path),
        expect_test::expect![[r#"

            predicate ::foo::test {
                pub var ::foo::blah;
                constraint (::foo::blah == 1e0);
            }"#]],
    );
    check(
        &run_parser!(
            pint,
            "predicate test { pub var blah: real = 1.0; }",
            mod_path
        ),
        expect_test::expect![[r#"

            predicate ::foo::test {
                pub var ::foo::blah: real;
                constraint (::foo::blah == 1e0);
            }"#]],
    );
    check(
        &run_parser!(pint, "predicate test { pub var blah: real; }", mod_path),
        expect_test::expect![[r#"

            predicate ::foo::test {
                pub var ::foo::blah: real;
            }"#]],
    );
    check(
        &run_parser!(pint, "predicate test { pub var blah = 1; }", mod_path),
        expect_test::expect![[r#"

            predicate ::foo::test {
                pub var ::foo::blah;
                constraint (::foo::blah == 1);
            }"#]],
    );
    check(
        &run_parser!(pint, "predicate test { pub var blah: int = 1; }", mod_path),
        expect_test::expect![[r#"

            predicate ::foo::test {
                pub var ::foo::blah: int;
                constraint (::foo::blah == 1);
            }"#]],
    );
    check(
        &run_parser!(pint, "predicate test { pub var blah: int; }", mod_path),
        expect_test::expect![[r#"

            predicate ::foo::test {
                pub var ::foo::blah: int;
            }"#]],
    );
    check(
        &run_parser!(pint, "predicate test { pub var blah = true; }", mod_path),
        expect_test::expect![[r#"

            predicate ::foo::test {
                pub var ::foo::blah;
                constraint (::foo::blah == true);
            }"#]],
    );
    check(
        &run_parser!(
            pint,
            "predicate test { pub var blah: bool = false; }",
            mod_path
        ),
        expect_test::expect![[r#"

            predicate ::foo::test {
                pub var ::foo::blah: bool;
                constraint (::foo::blah == false);
            }"#]],
    );
    check(
        &run_parser!(pint, "predicate test { pub var blah: bool; }", mod_path),
        expect_test::expect![[r#"

            predicate ::foo::test {
                pub var ::foo::blah: bool;
            }"#]],
    );
    check(
        &run_parser!(
            pint,
            r#"predicate test { pub var blah = "hello"; }"#,
            mod_path
        ),
        expect_test::expect![[r#"

            predicate ::foo::test {
                pub var ::foo::blah;
                constraint (::foo::blah == "hello");
            }"#]],
    );
    check(
        &run_parser!(
            pint,
            r#"predicate test { pub var blah: string = "hello"; }"#,
            mod_path
        ),
        expect_test::expect![[r#"

            predicate ::foo::test {
                pub var ::foo::blah: string;
                constraint (::foo::blah == "hello");
            }"#]],
    );
    check(
        &run_parser!(
            pint,
            r#"predicate test { pub var blah: string; }"#,
            mod_path
        ),
        expect_test::expect![[r#"

            predicate ::foo::test {
                pub var ::foo::blah: string;
            }"#]],
    );
}

#[test]
fn state_decls() {
    let pint = (yp::PintParser::new(), "");

    check(
        &run_parser!(pint, "predicate test { state x: int = __this_address(); }"),
        expect_test::expect![[r#"

            predicate ::test {
                state ::x: int = __this_address();
            }"#]],
    );
    check(
        &run_parser!(pint, "predicate test { state y = __vec_len(); }"),
        expect_test::expect![[r#"

            predicate ::test {
                state ::y = __vec_len();
            }"#]],
    );
}

#[test]
fn const_decls() {
    let pint = (yp::PintParser::new(), "");

    check(
        &run_parser!(pint, "const x: int = 11;"),
        expect_test::expect!["const ::x: int = 11;"],
    );
    check(
        &run_parser!(pint, "const y = 22;"),
        expect_test::expect!["const ::y = 22;"],
    );
    check(
        &run_parser!(pint, "const z: int;"),
        expect_test::expect![[r#"
            expected `=`, found `;`
            @12..13: expected `=`
        "#]],
    );
}

#[test]
fn constraint_decls() {
    // Argument just needs to be any expression, as far as the parser is concerned.
    let pint = (yp::PintParser::new(), "");

    check(
        &run_parser!(pint, "predicate test { constraint blah; }"),
        expect_test::expect![[r#"

            predicate ::test {
                constraint ::blah;
            }"#]],
    );
}

#[test]
fn if_decls() {
    // Argument just needs to be any expression, as far as the parser is concerned.
    let pint = (yp::PintParser::new(), "");

    check(
        &run_parser!(
            pint,
            r#"predicate test {
                if true { }
            }"#
        ),
        expect_test::expect![[r#"

            predicate ::test {
                if true {
                }
            }"#]],
    );

    check(
        &run_parser!(
            pint,
            r#"predicate test {
                if true { } else { }
            }"#
        ),
        expect_test::expect![[r#"

            predicate ::test {
                if true {
                } else {
                }
            }"#]],
    );

    check(
        &run_parser!(
            pint,
            r#"predicate test {
                if condition { constraint x; }
            }"#
        ),
        expect_test::expect![[r#"

            predicate ::test {
                if ::condition {
                    constraint ::x
                }
            }"#]],
    );

    check(
        &run_parser!(
            pint,
            r#"predicate test {
                if true { constraint x; } else { constraint y; }
            }"#
        ),
        expect_test::expect![[r#"

            predicate ::test {
                if true {
                    constraint ::x
                } else {
                    constraint ::y
                }
            }"#]],
    );

    check(
        &run_parser!(
            pint,
            r#"predicate test {
                if true { if false { constraint x; } else { constraint y; } }
                else { if __this_address() { if boo && y { constraint true; constraint false; } } }
            }"#
        ),
        expect_test::expect![[r#"

            predicate ::test {
                if true {
                    if false {
                        constraint ::x
                    } else {
                        constraint ::y
                    }
                } else {
                    if __this_address() {
                        if (::boo && ::y) {
                            constraint true
                            constraint false
                        }
                    }
                }
            }"#]],
    );
}

#[test]
fn basic_exprs() {
    let expr = (yp::TestDelegateParser::new(), "expr");
    check(&run_parser!(expr, "123"), expect_test::expect!["123"]);
    check(&run_parser!(expr, "foo"), expect_test::expect!["::foo"]);
}

#[test]
fn unary_op_exprs() {
    let expr = (yp::TestDelegateParser::new(), "expr");

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
        &run_parser!(expr, "! - - !  --  -t.0.1.2"),
        expect_test::expect!["!--!---::t.0.1.2"],
    );
    check(
        &run_parser!(expr, "! - - !  --  -a[5][3].1"),
        expect_test::expect!["!--!---::a[5][3].1"],
    );
    check(
        &run_parser!(expr, "! - x '  '  "),
        expect_test::expect!["!-::x''"],
    );
    check(
        &run_parser!(expr, "+ + x '  '  "),
        expect_test::expect![[r#"
            leading `+` is not supported
            @11..12: unexpected `+`
            try removing the `+`
            leading `+` is not supported
            @13..14: unexpected `+`
            try removing the `+`
        "#]],
    );
    check(
        &run_parser!(expr, "1 + +  + x"),
        expect_test::expect![[r#"
            leading `+` is not supported
            @15..16: unexpected `+`
            try removing the `+`
            leading `+` is not supported
            @18..19: unexpected `+`
            try removing the `+`
        "#]],
    );
}

#[test]
fn binary_op_exprs() {
    let expr = (yp::TestDelegateParser::new(), "expr");

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
    let expr = (yp::TestDelegateParser::new(), "expr");

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
    let expr = (yp::TestDelegateParser::new(), "expr");

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
            expected `!`, `(`, `+`, `-`, `::`, `[`, `a boolean`, `a literal`, `an identifier`, `cond`, `exists`, `forall`, `intrinsic_name`, `macro_name`, or `{`, found `)`
            @12..13: expected `!`, `(`, `+`, `-`, `::`, `[`, `a boolean`, `a literal`, `an identifier`, `cond`, `exists`, `forall`, `intrinsic_name`, `macro_name`, or `{`
        "#]],
    );

    check(
        &run_parser!(expr, "(a < b) ? 1 : 2"),
        expect_test::expect!["((::a < ::b) ? 1 : 2)"],
    );
    check(
        &run_parser!(expr, "(foo(a, b, c))"),
        expect_test::expect![[r#"
            expected `!=`, `%`, `&&`, `'`, `)`, `*`, `+`, `-`, `.`, `/`, `::`, `<`, `<=`, `==`, `>`, `>=`, `?`, `[`, `as`, `in`, or `||`, found `(`
            @15..16: expected `!=`, `%`, `&&`, `'`, `)`, `*`, `+`, `-`, `.`, `/`, `::`, `<`, `<=`, `==`, `>`, `>=`, `?`, `[`, `as`, `in`, or `||`
        "#]],
    );
}

#[test]
fn enums() {
    let pint = (yp::PintParser::new(), "");
    let expr = (yp::TestDelegateParser::new(), "expr");

    check(
        &run_parser!(pint, "enum MyEnum = Variant1 | Variant2;"),
        expect_test::expect!["enum ::MyEnum = Variant1 | Variant2;"],
    );
    check(
        &run_parser!(pint, "enum MyEnum = Variant1;"),
        expect_test::expect!["enum ::MyEnum = Variant1;"],
    );
    check(
        &run_parser!(expr, "MyEnum::Variant1"),
        expect_test::expect!["::MyEnum::Variant1"],
    );
    check(
        &run_parser!(
            pint,
            r#"predicate test {
                var x = MyEnum::Variant3;
            }"#
        ),
        expect_test::expect![[r#"

            predicate ::test {
                var ::x;
                constraint (::x == ::MyEnum::Variant3);
            }"#]],
    );
    check(
        &run_parser!(
            pint,
            r#"predicate test {
                var e: ::path::to::MyEnum;
            }"#
        ),
        expect_test::expect![[r#"

            predicate ::test {
                var ::e: ::path::to::MyEnum;
            }"#]],
    );
}

#[test]
fn custom_types() {
    let type_ = (yp::TestDelegateParser::new(), "type");
    let pint = (yp::PintParser::new(), "");

    check(
        &run_parser!(type_, "custom_type"),
        expect_test::expect!["::custom_type"],
    );
    check(
        &run_parser!(pint, "type MyInt = int;"),
        expect_test::expect!["type ::MyInt = int;"],
    );
    check(
        &run_parser!(pint, "type MyReal = real;"),
        expect_test::expect!["type ::MyReal = real;"],
    );
    check(
        &run_parser!(pint, "type MyBool = bool;"),
        expect_test::expect!["type ::MyBool = bool;"],
    );
    check(
        &run_parser!(pint, "type MyString = string;"),
        expect_test::expect!["type ::MyString = string;"],
    );
    check(
        &run_parser!(pint, "type IntArray = int[5];"),
        expect_test::expect!["type ::IntArray = int[5];"],
    );
    check(
        &run_parser!(pint, "type MyTuple = { int, real, z: string };"),
        expect_test::expect!["type ::MyTuple = {int, real, z: string};"],
    );
    check(
        &run_parser!(pint, "type MyAliasInt = MyInt;"),
        expect_test::expect!["type ::MyAliasInt = ::MyInt;"],
    );
}

#[test]
fn ranges() {
    let pint = (yp::PintParser::new(), "");
    let range = (yp::TestDelegateParser::new(), "range");
    let expr = (yp::TestDelegateParser::new(), "expr");

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
        &run_parser!(range, "-100.. (- ( c ? 10 : 9 ))"),
        expect_test::expect!["-100..-(::c ? 10 : 9)"],
    );
    check(
        &run_parser!(range, "1...2"),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `[`, `a boolean`, `a literal`, `an identifier`, `cond`, `exists`, `forall`, `intrinsic_name`, `macro_name`, or `{`, found `.`
            @15..16: expected `!`, `(`, `+`, `-`, `::`, `[`, `a boolean`, `a literal`, `an identifier`, `cond`, `exists`, `forall`, `intrinsic_name`, `macro_name`, or `{`
        "#]],
    );

    // Range allow in let decls
    check(
        &run_parser!(pint, "predicate test { var x = 1..2; }"),
        expect_test::expect![[r#"

            predicate ::test {
                var ::x;
                constraint (::x >= 1);
                constraint (::x <= 2);
            }"#]],
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
            expected `!=`, `&&`, `)`, `+`, `-`, `<`, `<=`, `==`, `>`, `>=`, `?`, `in`, or `||`, found `..`
            @13..15: expected `!=`, `&&`, `)`, `+`, `-`, `<`, `<=`, `==`, `>`, `>=`, `?`, `in`, or `||`
        "#]],
    );
}

#[test]
fn idents() {
    let ident = (yp::TestDelegateParser::new(), "ident");

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
    check(&run_parser!(ident, "_FOO"), expect_test::expect!["_FOO"]);
    check(
        &run_parser!(ident, "_2_FOO1"),
        expect_test::expect!["_2_FOO1"],
    );
    check(&run_parser!(ident, "_"), expect_test::expect!["_"]);
    check(
        &run_parser!(ident, "__this_address"),
        expect_test::expect![[r#"
            expected `an identifier`, found `__this_address`
            @12..26: expected `an identifier`
        "#]],
    );
    check(
        &run_parser!(ident, "12_ab"),
        expect_test::expect![[r#"
            expected `an identifier`, found `12`
            @12..14: expected `an identifier`
        "#]],
    );
    check(
        // Lexer will split this into 3 tokens, ident will parse the first one.
        // This shows that we're not able to parser `ab*cd` as a single identifier
        &run_parser!(ident, "ab*cd"),
        expect_test::expect![[r#"
            expected something else, found `*`
            @14..15: expected something else
        "#]],
    );
}

#[test]
fn intrinsic_name() {
    let intrinsic_name = (yp::TestDelegateParser::new(), "intrinsic");

    check(
        &run_parser!(intrinsic_name, "__this_addressbar"),
        expect_test::expect!["__this_addressbar"],
    );

    check(
        &run_parser!(intrinsic_name, "foo_bar"),
        expect_test::expect![[r#"
            expected `intrinsic_name`, found `foo_bar`
            @16..23: expected `intrinsic_name`
        "#]],
    );
}

#[test]
fn paths() {
    let expr = (yp::TestDelegateParser::new(), "expr");

    check(
        &run_parser!(expr, "foo::bar"),
        expect_test::expect!["::foo::bar"],
    );
    check(
        &run_parser!(expr, "_foo_::_bar"),
        expect_test::expect!["::_foo_::_bar"],
    );
    check(&run_parser!(expr, "_::_"), expect_test::expect!["::_::_"]);
    check(
        &run_parser!(expr, "t2::_3t::t4_::t"),
        expect_test::expect!["::t2::_3t::t4_::t"],
    );
    check(
        &run_parser!(expr, "::foo::bar"),
        expect_test::expect!["::foo::bar"],
    );

    // As long as these two produce an error... it should be expecting 'ident'.
    check(
        &run_parser!(expr, "foo::"),
        expect_test::expect![[r#"
            expected `an identifier`, or `macro_name`, found `end of file`
            @16..16: expected `an identifier`, or `macro_name`
        "#]],
    );
    check(
        &run_parser!(expr, "::foo::"),
        expect_test::expect![[r#"
            expected `an identifier`, or `macro_name`, found `end of file`
            @18..18: expected `an identifier`, or `macro_name`
        "#]],
    );
}

#[test]
fn macro_decl() {
    let src = r#"
          macro @foo($x, $y, &z) {
              var a = 5.0 + $x * $y;
              a
          }
      "#;

    let mut contract = Contract::default();
    let mut context = context!(contract, &Vec::<String>::new(), &mut Vec::new());
    let result = parse_and_collect_errors!(yp::PintParser::new(), src, context);

    assert!(result.is_ok());
    assert!(context.macros.len() == 1);

    check(
        &context.macros[0].to_string(),
        expect_test::expect!["macro ::@foo($x, $y, &z) { var a = 5.0 + $x * $y ; a }"],
    );
}

#[test]
fn macro_decl_good_params() {
    let mut contract = Contract::default();
    let mut context = context!(contract, Vec::<String>::new(), Vec::new());
    let parser = yp::PintParser::new();

    assert!(parse_and_collect_errors!(parser, r#"macro @foo($a) { x }"#, context).is_ok());

    assert!(parse_and_collect_errors!(parser, r#"macro @foo($a,) { x }"#, context).is_ok());

    assert!(parse_and_collect_errors!(parser, r#"macro @foo($a,$b) { x }"#, context).is_ok());

    assert!(parse_and_collect_errors!(parser, r#"macro @foo($a,$b,) { x }"#, context).is_ok());

    assert!(parse_and_collect_errors!(parser, r#"macro @foo($a,&rest) { x }"#, context).is_ok());

    assert!(parse_and_collect_errors!(parser, r#"macro @foo($a,&rest,) { x }"#, context).is_ok());
}

#[test]
fn macro_decl_bad_params() {
    let mut contract = Contract::default();
    let mut context = context!(contract, Vec::<String>::new(), Vec::new());
    let parser = yp::PintParser::new();

    let result = parse_and_collect_errors!(parser, r#"macro @foo(&rest) { x }"#, context);
    assert!(result.is_err());
    check(
        &display_errors(&result.unwrap_err()),
        expect_test::expect![[r#"
            expected `)`, or `macro_param`, found `&rest`
            @11..16: expected `)`, or `macro_param`
        "#]],
    );

    let result = parse_and_collect_errors!(parser, r#"macro @foo(&rest,) { x }"#, context);
    assert!(result.is_err());
    check(
        &display_errors(&result.unwrap_err()),
        expect_test::expect![[r#"
            expected `)`, or `macro_param`, found `&rest`
            @11..16: expected `)`, or `macro_param`
        "#]],
    );

    let result = parse_and_collect_errors!(parser, r#"macro @foo(&rest,$a) { x }"#, context);
    assert!(result.is_err());
    check(
        &display_errors(&result.unwrap_err()),
        expect_test::expect![[r#"
            expected `)`, or `macro_param`, found `&rest`
            @11..16: expected `)`, or `macro_param`
        "#]],
    );

    let result = parse_and_collect_errors!(parser, r#"macro @foo($a,,$b) { x }"#, context);
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
    let src = r#"predicate test { @foo(a * 3; int; <= =>); }"#;
    let mut contract = Contract::default();
    let mut context = context!(contract, Vec::<String>::new(), Vec::new());
    let result = parse_and_collect_errors!(yp::PintParser::new(), src, context);

    assert!(result.is_ok());
    assert!(context.contract.preds.iter().count() == 1);
    let (pred_key, pred) = context.contract.preds.iter().next().unwrap();
    assert!(pred.name == "::test");

    assert!(context.macro_calls.get(&pred_key).unwrap().len() == 1);

    check(
        &context
            .macro_calls
            .get(&pred_key)
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
fn select_exprs() {
    let expr = (yp::TestDelegateParser::new(), "expr");

    check(
        &run_parser!(expr, "c ? 1 : 0"),
        expect_test::expect!["(::c ? 1 : 0)"],
    );
    check(
        &run_parser!(expr, "c ? ( c ? 1 : 0 ) :  2 "),
        expect_test::expect!["(::c ? (::c ? 1 : 0) : 2)"],
    );

    check(
        &run_parser!(
            expr,
            "c ? x ? { 1, 1 }.0 : a in b as int : a[5] ? b && 5 : __this_address()"
        ),
        expect_test::expect![
            "(::c ? (::x ? {1, 1}.0 : ::a in ::b as int) : (::a[5] ? (::b && 5) : __this_address()))"
        ],
    );
}

#[test]
fn array_type() {
    let type_ = (yp::TestDelegateParser::new(), "type");

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
        &run_parser!(type_, r#"string[__this_address()][ 7 ][true ?  1 : 2]"#),
        expect_test::expect!["string[(true ? 1 : 2)][7][__this_address()]"],
    );

    check(
        &run_parser!(type_, r#"real[N][9][M][3]"#),
        expect_test::expect!["real[3][::M][9][::N]"],
    );

    check(
        &run_parser!(type_, r#"{int, { real, string }}[N][9]"#),
        expect_test::expect!["{int, {real, string}}[9][::N]"],
    );

    // This should fail in type checking
    check(
        &run_parser!(
            (yp::PintParser::new(), ""),
            r#"predicate test { var a: int[]; }"#
        ),
        expect_test::expect![[r#"

            predicate ::test {
                var ::a: int[];
            }"#]],
    );
}

#[test]
fn array_expressions() {
    let expr = (yp::TestDelegateParser::new(), "expr");

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
        &run_parser!(expr, r#"[[__this_address(), 2], [ true ? 1 : 2, t.0]]"#),
        expect_test::expect!["[[__this_address(), 2], [(true ? 1 : 2), ::t.0]]"],
    );
}

#[test]
fn array_element_accesses() {
    let expr = (yp::TestDelegateParser::new(), "expr");

    check(
        &run_parser!(expr, r#"a[5]"#),
        expect_test::expect!["::a[5]"],
    );

    check(
        &run_parser!(expr, r#"a[N][5][t.0]"#),
        expect_test::expect!["::a[::N][5][::t.0]"],
    );

    check(
        &run_parser!(expr, r#"{ a, b }[N][__this_address()][M][4]"#),
        expect_test::expect!["{::a, ::b}[::N][__this_address()][::M][4]"],
    );

    check(
        &run_parser!(expr, r#"__this_address()[ M ][true ? 1 : 3]"#),
        expect_test::expect!["__this_address()[::M][(true ? 1 : 3)]"],
    );

    check(
        &run_parser!(
            (yp::PintParser::new(), ""),
            r#"predicate test { var x = a[]; }"#
        ),
        expect_test::expect![[r#"
            missing array or map index
            @25..28: missing array or map element index
        "#]],
    );

    check(
        &run_parser!(expr, r#"a[MyEnum::Variant1]"#),
        expect_test::expect!["::a[::MyEnum::Variant1]"],
    );
}

#[test]
fn tuple_expressions() {
    let expr = (yp::TestDelegateParser::new(), "expr");

    check(&run_parser!(expr, r#"{ 0 }"#), expect_test::expect!["{0}"]);

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
        &run_parser!(expr, r#"{ 42, c ?  2 : 3, __this_address() }"#),
        expect_test::expect!["{42, (::c ? 2 : 3), __this_address()}"],
    );

    check(
        &run_parser!(expr, r#"{ x:  42 , y: c ?  2 : 3, z: __this_address() }"#),
        expect_test::expect!["{x: 42, y: (::c ? 2 : 3), z: __this_address()}"],
    );
}

#[test]
fn tuple_field_accesses() {
    let expr = (yp::TestDelegateParser::new(), "expr");

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
        &run_parser!(expr, r#"__this_address().0.1"#),
        expect_test::expect!["__this_address().0.1"],
    );

    check(
        &run_parser!(expr, r#"__this_address().a.b.0.1"#),
        expect_test::expect!["__this_address().a.b.0.1"],
    );

    check(
        &run_parser!(expr, r#"{0, 0} .0"#),
        expect_test::expect!["{0, 0}.0"],
    );

    check(
        &run_parser!(expr, r#" {0, 0} .a"#),
        expect_test::expect!["{0, 0}.a"],
    );

    check(
        &run_parser!(expr, r#"(true ? {0, 0} : {0, 0}).0"#),
        expect_test::expect!["(true ? {0, 0} : {0, 0}).0"],
    );

    check(
        &run_parser!(expr, r#"(true ? {0, 0}  : {0, 0}).x"#),
        expect_test::expect!["(true ? {0, 0} : {0, 0}).x"],
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

    check(
        &run_parser!(expr, "{1_1, 2_000}.x"),
        expect_test::expect!["{11, 2000}.x"],
    );

    check(
        &run_parser!(expr, "{1_100.4e3, 2_0e3}.x"),
        expect_test::expect!["{1.1004e6, 2e4}.x"],
    );

    check(
        &run_parser!(expr, "a[5][3].foo.2[4].1"),
        expect_test::expect!["::a[5][3].foo.2[4].1"],
    );

    check(
        &run_parser!(expr, "a.foo[5][3].1"),
        expect_test::expect!["::a.foo[5][3].1"],
    );

    let pint = (yp::PintParser::new(), "");

    check(
        &run_parser!(pint, "predicate test { var x = t.0xa; }"),
        expect_test::expect![[r#"
            invalid integer `0xa` as tuple index
            @27..30: invalid integer as tuple index
        "#]],
    );

    check(
        &run_parser!(
            pint,
            "predicate test { var x = t.111111111111111111111111111; }"
        ),
        expect_test::expect![[r#"
            invalid integer `111111111111111111111111111` as tuple index
            @27..54: invalid integer as tuple index
        "#]],
    );

    check(
        &run_parser!(
            pint,
            "predicate test { var x = t.111111111111111111111111111.2; }"
        ),
        expect_test::expect![[r#"
            invalid integer `111111111111111111111111111` as tuple index
            @27..54: invalid integer as tuple index
        "#]],
    );

    check(
        &run_parser!(
            pint,
            "predicate test { var x = t.2.111111111111111111111111111; }"
        ),
        expect_test::expect![[r#"
            invalid integer `111111111111111111111111111` as tuple index
            @29..56: invalid integer as tuple index
        "#]],
    );

    check(
        &run_parser!(
            pint,
            "var x = t.222222222222222222222.111111111111111111111111111;"
        ),
        expect_test::expect![[r#"
            expected `::`, `an identifier`, `const`, `enum`, `interface`, `macro`, `macro_name`, `predicate`, `storage`, `type`, or `use`, found `var`
            @0..3: expected `::`, `an identifier`, `const`, `enum`, `interface`, `macro`, `macro_name`, `predicate`, `storage`, `type`, or `use`
        "#]],
    );

    check(
        &run_parser!(pint, "predicate test { var x = t.1e5; }"),
        expect_test::expect![[r#"
            invalid value `1e5` as tuple index
            @27..30: invalid value as tuple index
        "#]],
    );

    check(
        &run_parser!(pint, "predicate test { var bad_tuple:{} = {}; }"),
        expect_test::expect![[r#"
            empty tuple types are not allowed
            @31..33: empty tuple type found
            empty tuple expressions are not allowed
            @36..38: empty tuple expression found
        "#]],
    );
}

#[test]
fn cond_exprs() {
    let expr = (yp::TestDelegateParser::new(), "expr");

    check(
        &run_parser!(expr, r#"cond { else => a, }"#),
        expect_test::expect!["::a"],
    );

    check(
        &run_parser!(expr, r#"cond { else =>  a  }"#),
        expect_test::expect!["::a"],
    );

    check(
        &run_parser!(expr, r#"cond { a => b, else => c }"#),
        expect_test::expect!["(::a ? ::b : ::c)"],
    );

    check(
        &run_parser!(expr, r#"cond { a =>  b , else => c, }"#),
        expect_test::expect!["(::a ? ::b : ::c)"],
    );

    check(
        &run_parser!(expr, r#"cond { a => b,  true  => d, else => f, }"#),
        expect_test::expect!["(::a ? ::b : (true ? ::d : ::f))"],
    );

    check(
        &run_parser!(expr, r#"cond { a => b, }"#),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `[`, `a boolean`, `a literal`, `an identifier`, `cond`, `else`, `exists`, `forall`, `intrinsic_name`, `macro_name`, or `{`, found `}`
            @26..27: expected `!`, `(`, `+`, `-`, `::`, `[`, `a boolean`, `a literal`, `an identifier`, `cond`, `else`, `exists`, `forall`, `intrinsic_name`, `macro_name`, or `{`
        "#]],
    );

    check(
        &run_parser!(expr, r#"cond { else => a, b => c }"#),
        expect_test::expect![[r#"
            expected `}`, found `b`
            @29..30: expected `}`
        "#]],
    );
}

#[test]
fn casting() {
    let expr = (yp::TestDelegateParser::new(), "expr");
    let pint = (yp::PintParser::new(), "");

    check(
        &run_parser!(expr, r#"(5 as int) as real as int"#),
        expect_test::expect!["5 as int as real as int"],
    );

    check(
        &run_parser!(expr, r#"t.0.1 as real * a[5][3] as int"#),
        expect_test::expect!["(::t.0.1 as real * ::a[5][3] as int)"],
    );

    check(
        &run_parser!(
            pint,
            r#"predicate test { var x = __this_address() as real as { int, real }; }"#
        ),
        expect_test::expect![[r#"

            predicate ::test {
                var ::x;
                constraint (::x == __this_address() as real as {int, real});
            }"#]],
    );

    check(
        &run_parser!(pint, r#"predicate test { var x = 5 as"#),
        expect_test::expect![[r#"
            expected `(`, `::`, `a type`, `an identifier`, or `{`, found `end of file`
            @29..29: expected `(`, `::`, `a type`, `an identifier`, or `{`
        "#]],
    )
}

#[test]
fn in_expr() {
    let expr = (yp::TestDelegateParser::new(), "expr");

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
        &run_parser!(expr, r#"[1] in __this_address() in [[1]]"#),
        expect_test::expect!["[1] in __this_address() in [[1]]"],
    );

    check(
        &run_parser!(
            (yp::PintParser::new(), ""),
            r#"predicate test { var x = 5 in"#
        ),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `[`, `a boolean`, `a literal`, `an identifier`, `cond`, `exists`, `forall`, `intrinsic_name`, `macro_name`, or `{`, found `end of file`
            @29..29: expected `!`, `(`, `+`, `-`, `::`, `[`, `a boolean`, `a literal`, `an identifier`, `cond`, `exists`, `forall`, `intrinsic_name`, `macro_name`, or `{`
        "#]],
    );
}

#[test]
fn forall_expr() {
    let pint = (yp::PintParser::new(), "");

    check(
        &run_parser!(
            pint,
            r#"predicate test { constraint forall i in 0..3 { true }; }"#
        ),
        expect_test::expect![[r#"

            predicate ::test {
                constraint forall i in 0..3, { true };
            }"#]],
    );

    check(
        &run_parser!(
            pint,
            r#"predicate test { constraint forall i in 0..3, j in i..k { true }; }"#
        ),
        expect_test::expect![[r#"

            predicate ::test {
                constraint forall i in 0..3, j in ::i..::k, { true };
            }"#]],
    );

    check(
        &run_parser!(
            pint,
            r#"predicate test { constraint forall i in 0..3 where i > 4 { true }; }"#
        ),
        expect_test::expect![[r#"

            predicate ::test {
                constraint forall i in 0..3, where (::i > 4) { true };
            }"#]],
    );

    check(
        &run_parser!(
            pint,
            r#"predicate test {
                constraint forall i in 0..3, j in 0..3 where i > 2, j < 3, i != j && true { A[i] > A[j] };
            }"#
        ),
        expect_test::expect![[r#"

            predicate ::test {
                constraint forall i in 0..3, j in 0..3, where (::i > 2), (::j < 3), ((::i != ::j) && true) { (::A[::i] > ::A[::j]) };
            }"#]],
    );

    check(
        &run_parser!(pint, r#"predicate test { constraint forall { true }; }"#),
        expect_test::expect![[r#"
            expected `an identifier`, found `{`
            @35..36: expected `an identifier`
        "#]],
    );

    check(
        &run_parser!(
            pint,
            r#"predicate test { constraint forall range { true }; }"#
        ),
        expect_test::expect![[r#"
            expected `in`, found `{`
            @41..42: expected `in`
        "#]],
    );

    check(
        &run_parser!(
            pint,
            r#"predicate test { constraint forall i in 0..3 { constraint x; true }; }"#
        ),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `[`, `a boolean`, `a literal`, `an identifier`, `cond`, `exists`, `forall`, `intrinsic_name`, `macro_name`, or `{`, found `constraint`
            @47..57: expected `!`, `(`, `+`, `-`, `::`, `[`, `a boolean`, `a literal`, `an identifier`, `cond`, `exists`, `forall`, `intrinsic_name`, `macro_name`, or `{`
        "#]],
    );
}

#[test]
fn exists_expr() {
    let pint = (yp::PintParser::new(), "");
    check(
        &run_parser!(
            pint,
            r#"predicate test { constraint exists i in 0..3 { true }; }"#
        ),
        expect_test::expect![[r#"

            predicate ::test {
                constraint exists i in 0..3, { true };
            }"#]],
    );

    check(
        &run_parser!(
            pint,
            r#"predicate test { constraint exists i in 0..3, j in i..k { true }; }"#
        ),
        expect_test::expect![[r#"

            predicate ::test {
                constraint exists i in 0..3, j in ::i..::k, { true };
            }"#]],
    );

    check(
        &run_parser!(
            pint,
            r#"predicate test { constraint exists i in 0..3 where i > 4 { true }; }"#
        ),
        expect_test::expect![[r#"

            predicate ::test {
                constraint exists i in 0..3, where (::i > 4) { true };
            }"#]],
    );

    check(
        &run_parser!(
            pint,
            r#"predicate test {
                constraint exists i in 0..3, j in 0..3 where i > 2, j < 3, i != j && true { A[i] > A[j] };
            }"#
        ),
        expect_test::expect![[r#"

            predicate ::test {
                constraint exists i in 0..3, j in 0..3, where (::i > 2), (::j < 3), ((::i != ::j) && true) { (::A[::i] > ::A[::j]) };
            }"#]],
    );

    check(
        &run_parser!(pint, r#"predicate test { constraint exists { true }; }"#),
        expect_test::expect![[r#"
            expected `an identifier`, found `{`
            @35..36: expected `an identifier`
        "#]],
    );

    check(
        &run_parser!(
            pint,
            r#"predicate test { constraint exists range { true }; }"#
        ),
        expect_test::expect![[r#"
            expected `in`, found `{`
            @41..42: expected `in`
        "#]],
    );

    check(
        &run_parser!(
            pint,
            r#"predicate test { constraint exists i in 0..3 { constraint x; true }; }"#
        ),
        expect_test::expect![[r#"
            expected `!`, `(`, `+`, `-`, `::`, `[`, `a boolean`, `a literal`, `an identifier`, `cond`, `exists`, `forall`, `intrinsic_name`, `macro_name`, or `{`, found `constraint`
            @47..57: expected `!`, `(`, `+`, `-`, `::`, `[`, `a boolean`, `a literal`, `an identifier`, `cond`, `exists`, `forall`, `intrinsic_name`, `macro_name`, or `{`
        "#]],
    );
}

#[test]
fn intrinsic_call() {
    let expr = (yp::TestDelegateParser::new(), "expr");

    check(
        &run_parser!(expr, r#"__this_address()"#),
        expect_test::expect!["__this_address()"],
    );

    check(
        &run_parser!(expr, r#"foo(x)"#),
        expect_test::expect![[r#"
            expected `!=`, `%`, `&&`, `'`, `*`, `+`, `-`, `.`, `/`, `::`, `<`, `<=`, `==`, `>`, `>=`, `?`, `[`, `as`, `in`, or `||`, found `(`
            @14..15: expected `!=`, `%`, `&&`, `'`, `*`, `+`, `-`, `.`, `/`, `::`, `<`, `<=`, `==`, `>`, `>=`, `?`, `[`, `as`, `in`, or `||`
        "#]],
    );

    check(
        &run_parser!(expr, r#"foo(1,)"#),
        expect_test::expect![[r#"
            expected `!=`, `%`, `&&`, `'`, `*`, `+`, `-`, `.`, `/`, `::`, `<`, `<=`, `==`, `>`, `>=`, `?`, `[`, `as`, `in`, or `||`, found `(`
            @14..15: expected `!=`, `%`, `&&`, `'`, `*`, `+`, `-`, `.`, `/`, `::`, `<`, `<=`, `==`, `>`, `>=`, `?`, `[`, `as`, `in`, or `||`
        "#]],
    );

    check(
        &run_parser!(expr, r#"foo(1, 2,)"#),
        expect_test::expect![[r#"
            expected `!=`, `%`, `&&`, `'`, `*`, `+`, `-`, `.`, `/`, `::`, `<`, `<=`, `==`, `>`, `>=`, `?`, `[`, `as`, `in`, or `||`, found `(`
            @14..15: expected `!=`, `%`, `&&`, `'`, `*`, `+`, `-`, `.`, `/`, `::`, `<`, `<=`, `==`, `>`, `>=`, `?`, `[`, `as`, `in`, or `||`
        "#]],
    );

    check(
        &run_parser!(expr, r#"foo(1, 2, { 3, x }.1, [y, __vec_len()])"#),
        expect_test::expect![[r#"
            expected `!=`, `%`, `&&`, `'`, `*`, `+`, `-`, `.`, `/`, `::`, `<`, `<=`, `==`, `>`, `>=`, `?`, `[`, `as`, `in`, or `||`, found `(`
            @14..15: expected `!=`, `%`, `&&`, `'`, `*`, `+`, `-`, `.`, `/`, `::`, `<`, `<=`, `==`, `>`, `>=`, `?`, `[`, `as`, `in`, or `||`
        "#]],
    );

    check(
        &run_parser!(
            (yp::PintParser::new(), ""),
            r#"predicate test { var x = foo(a*3, c); }"#
        ),
        expect_test::expect![[r#"
            expected `!=`, `%`, `&&`, `'`, `*`, `+`, `-`, `.`, `..`, `/`, `;`, `<`, `<=`, `==`, `>`, `>=`, `?`, `[`, `as`, `in`, or `||`, found `(`
            @28..29: expected `!=`, `%`, `&&`, `'`, `*`, `+`, `-`, `.`, `..`, `/`, `;`, `<`, `<=`, `==`, `>`, `>=`, `?`, `[`, `as`, `in`, or `||`
        "#]],
    );

    check(
        &run_parser!(
            (yp::TestDelegateParser::new(), "expr"),
            "__this_address(-a, b+c)"
        ),
        expect_test::expect!["__this_address(-::a, (::b + ::c))"],
    );
}

#[test]
fn basic_contract() {
    let src = r#"
predicate test {
    var low_val: real = 1.23;
    var high_val = 4.56;        // Implicit type.

    // Here's the constraints.
    constraint mid > low_val * 2.0;
    constraint mid < high_val;
}
"#;

    check(
        &run_parser!((yp::PintParser::new(), ""), src),
        expect_test::expect![[r#"

            predicate ::test {
                var ::low_val: real;
                var ::high_val;
                constraint (::low_val == 1.23e0);
                constraint (::high_val == 4.56e0);
                constraint (::mid > (::low_val * 2e0));
                constraint (::mid < ::high_val);
            }"#]],
    );
}

#[test]
fn predicate_decls() {
    let src = r#"
predicate Foo { }
predicate Bar {
    var x: int;
    constraint x == 1;
}
enum MyEnum = A | B;
type MyType = MyEnum;
predicate Baz {
}
"#;

    check(
        &run_parser!((yp::PintParser::new(), ""), src),
        expect_test::expect![[r#"
            enum ::MyEnum = A | B;
            type ::MyType = ::MyEnum;

            predicate ::Foo {
            }

            predicate ::Bar {
                var ::x: int;
                constraint (::x == 1);
            }

            predicate ::Baz {
            }"#]],
    );
}

#[test]
fn out_of_order_decls() {
    let src = r#"predicate test {
        constraint low < high;
        var high = 2.0;
        var low = 1.0;
    }"#;

    check(
        &run_parser!((yp::PintParser::new(), ""), src),
        expect_test::expect![[r#"

            predicate ::test {
                var ::high;
                var ::low;
                constraint (::low < ::high);
                constraint (::high == 2e0);
                constraint (::low == 1e0);
            }"#]],
    );
}

#[test]
fn keywords_as_identifiers_errors() {
    // TODO: Ideally, we emit a special error here. Instead, we currently get a generic "expected..
    // found" error.
    for keyword in KEYWORDS {
        let src = format!("predicate test {{ var {keyword} = 5; }}").to_string();
        assert_eq!(
            &run_parser!((yp::PintParser::new(), ""), &src),
            &format!(
                "expected `an identifier`, found `{keyword}`\n@21..{}: expected `an identifier`\n",
                21 + format!("{keyword}").len() // End of the error span)
            ),
            "Check \"identifier as keyword\" error for  keyword \"{}\"",
            keyword
        );
    }
}

#[test]
fn big_ints() {
    let pint = (yp::PintParser::new(), "");

    check(
        &run_parser!(
            pint,
            "predicate test { var blah = 1234567890123456789012345678901234567890; }"
        ),
        expect_test::expect![[r#"
            integer literal is too large
            @28..68: integer literal is too large
            value exceeds limit of `9,223,372,036,854,775,807`
        "#]],
    );

    check(
        &run_parser!(pint, "predicate test { var blah = 0xfeedbadfd2adeadc; }"),
        // Confirmed by using the Python REPL to convert from hex to dec...
        expect_test::expect![[r#"

            predicate ::test {
                var ::blah;
                constraint (::blah == -77200148120343844);
            }"#]],
    );

    check(
        &run_parser!(
            pint,
            "var blah = 0xfeedbadfd2adeadcafed00dbabefacefeedbadf00d2adeadcafed00dbabeface;"
        ),
        expect_test::expect![[r#"
            expected `::`, `an identifier`, `const`, `enum`, `interface`, `macro`, `macro_name`, `predicate`, `storage`, `type`, or `use`, found `var`
            @0..3: expected `::`, `an identifier`, `const`, `enum`, `interface`, `macro`, `macro_name`, `predicate`, `storage`, `type`, or `use`
        "#]],
    );

    check(
        &run_parser!(
            (yp::TestDelegateParser::new(), "expr"),
            "0b1101001010010101010101010101010100110100110101010101010101010101 + \
             0b0100100101011010101010100101010101001010010010100101001010010010"
        ),
        expect_test::expect!["(-3272615729767819947 + 5285724395968025234)"],
    );
}

#[test]
fn error_recovery() {
    let src = r#"
        predicate test {
            var untyped;
            var clash = 5;
            var clash = 5;
            var clash = 5;
            var empty_tuple: {} = {};
            var empty_array: int[] = [];
            var empty_index = a[];
            var bad_integer_index = t.0x5;
            var bad_real_index = t.1e5;
            var parse_error
        "#;

    check(
        &run_parser!((yp::PintParser::new(), ""), src),
        expect_test::expect![[r#"
            type annotation or initializer needed for variable `untyped`
            @38..49: type annotation or initializer needed
            consider giving `untyped` an explicit type or an initializer
            symbol `clash` has already been declared
            @67..72: previous declaration of the symbol `clash` here
            @94..99: `clash` redeclared here
            `clash` must be declared or imported only once in this scope
            symbol `clash` has already been declared
            @67..72: previous declaration of the symbol `clash` here
            @121..126: `clash` redeclared here
            `clash` must be declared or imported only once in this scope
            empty tuple types are not allowed
            @161..163: empty tuple type found
            empty tuple expressions are not allowed
            @166..168: empty tuple expression found
            missing array or map index
            @241..244: missing array or map element index
            invalid integer `0x5` as tuple index
            @284..287: invalid integer as tuple index
            invalid value `1e5` as tuple index
            @324..327: invalid value as tuple index
            expected `:`, `;`, or `=`, found `end of file`
            @356..356: expected `:`, `;`, or `=`
        "#]],
    );
}
