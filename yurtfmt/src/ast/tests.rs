#[cfg(test)]
use crate::formatter::{Format, FormattedCode};

#[cfg(test)]
use crate::parser::*;

#[cfg(test)]
use chumsky::{prelude::*, Stream};

#[cfg(test)]
fn check(actual: &str, expect: expect_test::Expect) {
    expect.assert_eq(actual);
}

#[cfg(test)]
macro_rules! run_formatter {
    ($parser: expr, $source: expr) => {{
        let (toks, errs) = crate::lexer::lex($source);
        if !errs.is_empty() {
            format!(
                "{}",
                // Print each lexing error on one line
                errs.iter().fold(String::new(), |acc, error| {
                    format!("{}{}\n", acc, error)
                })
            )
        } else {
            let token_stream = Stream::from_iter($source.len()..$source.len(), toks.into_iter());
            match $parser.parse(token_stream) {
                Ok(ast) => {
                    let mut formatted_code = FormattedCode::new();
                    match ast.format(&mut formatted_code) {
                        Ok(_) => formatted_code.as_str().to_string(),
                        Err(error) => format!("{}", error),
                    }
                }
                Err(errors) => format!(
                    "{}",
                    // Print each parsing error on one line
                    errors.iter().fold(String::new(), |acc, error| {
                        format!("{}{}\n", acc, error)
                    })
                ),
            }
        }
    }};
}

#[test]
fn errors() {
    check(
        &run_formatter!(yurt_program(), r#"@@@"#),
        expect_test::expect![[r#"
            invalid token
            invalid token
            invalid token
        "#]],
    );

    check(
        &run_formatter!(yurt_program(), r#"let x = 5"#),
        expect_test::expect![[r#"
            Error formatting starting at location 9 and ending at location 9
        "#]],
    );
}

#[test]
fn let_decls() {
    check(
        &run_formatter!(
            yurt_program(),
            r#"
    let   x   = 5;

   let y     =     7.777;

    let   bool_var   = true;

  let       bool_var   =false;

    let  str_var =     "sample";

    let str1 = "abc \
        def";

    let str2 = "abc
        def";

 let  real_var  =    8.8888E+5;

     let hex_var =   0xFF;

let bin_var  =0b1010;

let         bigint_var=1234567890123456789012345678901234567890;
"#
        ),
        expect_test::expect![[r#"
                let x = 5;
                let y = 7.777;
                let bool_var = true;
                let bool_var = false;
                let str_var = "sample";
                let str1 = "abc \
                        def";
                let str2 = "abc
                        def";
                let real_var = 8.8888E+5;
                let hex_var = 0xFF;
                let bin_var = 0b1010;
                let bigint_var = 1234567890123456789012345678901234567890;
            "#]],
    );

    check(
        &run_formatter!(
            yurt_program(),
            r#"
    let  x  :  int   =   5;

   let   y:  real  =  7.777;

    let   bool_var : bool    = true;

 let str_var  : string  =    "this sample has spaces";

     let real_var:   real = 8.8888E+5;

  let   hex_var:int =   0xFF;

let bin_var :  int=0b1010;

     let         bigint_var:        int=1234567890123456789012345678901234567890;
"#
        ),
        expect_test::expect![[r#"
                let x: int = 5;
                let y: real = 7.777;
                let bool_var: bool = true;
                let str_var: string = "this sample has spaces";
                let real_var: real = 8.8888E+5;
                let hex_var: int = 0xFF;
                let bin_var: int = 0b1010;
                let bigint_var: int = 1234567890123456789012345678901234567890;
            "#]],
    );

    check(
        &run_formatter!(
            yurt_program(),
            r#"
    let     x ;

    let  bool_var   ;
        "#
        ),
        expect_test::expect![[r#"
                let x;
                let bool_var;
            "#]],
    );

    check(
        &run_formatter!(
            yurt_program(),
            r#"
            let   t 
            : {  int , real
            , string };
            "#
        ),
        expect_test::expect![[r#"
            let t: { int, real, string };
        "#]],
    );
}

#[test]
fn solve_decls() {
    check(
        &run_formatter!(
            yurt_program(),
            r#"
                 solve    satisfy   ;

          solve    minimize   ;

solve  maximize ;

               solve minimize   foo;

   solve   maximize foo   ;

                    solve   minimize x    +y   ;

        solve   maximize y-    x   ;
  "#
        ),
        expect_test::expect![[r#"
                    solve satisfy;
                    solve minimize;
                    solve maximize;
                    solve minimize foo;
                    solve maximize foo;
                    solve minimize x + y;
                    solve maximize y - x;
                "#]],
    );
}

#[test]
fn constraint_decls() {
    // TODO: add binary expr constraint when supported (ex. x + y)
    check(
        &run_formatter!(
            yurt_program(),
            r#"
            constraint         blah;
    constraint foo      ;
        "#
        ),
        expect_test::expect![[r#"
        constraint blah;
        constraint foo;
        "#]],
    );
}

#[test]
fn paths() {
    use crate::parser;
    check(
        &run_formatter!(parser::path(), r#"::     foo    ::   bar   "#),
        expect_test::expect![[r#"::foo::bar"#]],
    );
    check(
        &run_formatter!(path(), "foo    ::   \n   bar"),
        expect_test::expect![[r#"foo::bar"#]],
    );
    check(
        &run_formatter!(path(), "_foo_  \n :: \n  _bar"),
        expect_test::expect![[r#"_foo_::_bar"#]],
    );
    check(
        &run_formatter!(path(), "_  ::   _"),
        expect_test::expect![[r#"_::_"#]],
    );
    check(
        &run_formatter!(path(), "t2::   \n_3t::  t4_   :: t"),
        expect_test::expect![[r#"t2::_3t::t4_::t"#]],
    );
    check(
        &run_formatter!(path(), ":: foo   ::bar "),
        expect_test::expect![[r#"::foo::bar"#]],
    );
}

#[test]
fn use_statements() {
    check(
        &run_formatter!(
            use_statement(),
            "

        use  x ;  "
        ),
        expect_test::expect!["use x;"],
    );
    check(
        &run_formatter!(
            use_statement(),
            "
                use   a::b::c;"
        ),
        expect_test::expect![[r#"use a::b::c;"#]],
    );
    check(
        &run_formatter!(
            use_statement(),
            "   use {  c, d,e,
    g};  "
        ),
        expect_test::expect!["use {c, d, e, g};"],
    );
    check(
        &run_formatter!(
            use_statement(),
            "
                use   q::r   as  x ;
                "
        ),
        expect_test::expect!["use q::r as x;"],
    );
    check(
        &run_formatter!(use_statement(), "  use   q::r as x   ;  "),
        expect_test::expect!["use q::r as x;"],
    );
    check(
        &run_formatter!(
            use_statement(),
            "   use a::b::{
            self,
            c,
            d::e
            };  "
        ),
        expect_test::expect!["use a::b::{self, c, d::e};"],
    );
    check(
        &run_formatter!(
            use_statement(),
            " use a::b::{
            self   as ab , c,
            d::{
            e, f::g} 
            };"
        ),
        expect_test::expect!["use a::b::{self as ab, c, d::{e, f::g}};"],
    );
}

#[test]
fn unary_op_exprs() {
    check(
        &run_formatter!(expr(), "   !a   "),
        expect_test::expect![[r#"!a"#]],
    );
    check(
        &run_formatter!(expr(), "   +a   "),
        expect_test::expect![[r#"+a"#]],
    );
    check(
        &run_formatter!(expr(), " -   a"),
        expect_test::expect![[r#"-a"#]],
    );
    check(
        &run_formatter!(expr(), "+7"),
        expect_test::expect![[r#"+7"#]],
    );
    check(
        &run_formatter!(expr(), "+    3.4"),
        expect_test::expect![[r#"+3.4"#]],
    );
    check(
        &run_formatter!(expr(), "-  1.0"),
        expect_test::expect![[r#"-1.0"#]],
    );
    check(
        &run_formatter!(expr(), "! - - !  -+  -1"),
        expect_test::expect![[r#"!--!-+-1"#]],
    );
    check(
        &run_formatter!(expr(), "!- -! - +  -1 "),
        expect_test::expect![[r#"!--!-+-1"#]],
    );
}

#[test]
fn binary_op_exprs() {
    check(
        &run_formatter!(expr(), "   a   *    2.0   "),
        expect_test::expect![[r#"a * 2.0"#]],
    );
    check(
        &run_formatter!(expr(), " a /     2.0 "),
        expect_test::expect![[r#"a / 2.0"#]],
    );
    check(
        &run_formatter!(expr(), "a   %   2.0"),
        expect_test::expect![[r#"a % 2.0"#]],
    );
    check(
        &run_formatter!(expr(), "  a   +  2.0 "),
        expect_test::expect![[r#"a + 2.0"#]],
    );
    check(
        &run_formatter!(expr(), " a   - 2.0   "),
        expect_test::expect![[r#"a - 2.0"#]],
    );
    check(
        &run_formatter!(expr(), "a+   2.0"),
        expect_test::expect![[r#"a + 2.0"#]],
    );
    check(
        &run_formatter!(expr(), " a- 2.0   "),
        expect_test::expect![[r#"a - 2.0"#]],
    );
    check(
        &run_formatter!(expr(), "   a <  2.0   "),
        expect_test::expect![[r#"a < 2.0"#]],
    );
    check(
        &run_formatter!(expr(), " a   >    2.0 "),
        expect_test::expect![[r#"a > 2.0"#]],
    );
    check(
        &run_formatter!(expr(), "a   <= 2.0   "),
        expect_test::expect![[r#"a <= 2.0"#]],
    );
    check(
        &run_formatter!(expr(), "   a  >=  2.0  "),
        expect_test::expect![[r#"a >= 2.0"#]],
    );
    check(
        &run_formatter!(expr(), "  a   ==  2.0   "),
        expect_test::expect![[r#"a == 2.0"#]],
    );
    check(
        &run_formatter!(expr(), " a  != 2.0 "),
        expect_test::expect![[r#"a != 2.0"#]],
    );
    check(
        &run_formatter!(expr(), "a  &&  b  "),
        expect_test::expect![[r#"a && b"#]],
    );
    check(
        &run_formatter!(expr(), "   a ||  b  "),
        expect_test::expect![[r#"a || b"#]],
    );
    check(
        &run_formatter!(expr(), "   a     ||  b &&     c  ||      d      &&     ! e"),
        expect_test::expect![[r#"a || b && c || d && !e"#]],
    );
}

#[test]
fn custom_types() {
    check(
        &run_formatter!(
            yurt_program(),
            r#"
                type MyTuple =  { 
                    x:  int , y:    real, z: 
                string };
            "#
        ),
        expect_test::expect![[r#"
            type MyTuple = { x: int, y: real, z: string };
        "#]],
    );
    check(
        &run_formatter!(
            yurt_program(),
            r#"
                type   MyTuple 
            = { real, 
                bool, z
            :   string };
            "#
        ),
        expect_test::expect![[r#"
            type MyTuple = { real, bool, z: string };
        "#]],
    );
}

#[test]
fn func_decl() {
    check(
        &run_formatter!(
            yurt_program(),
            r#"
            fn foo(
                        x: real,
        y: real
            )   ->       real {
                let x: int = 2;
    y + 1
            }
            "#
        ),
        expect_test::expect![[r#"
            fn foo(x: real, y: real) -> real {
                let x: int = 2;
                y + 1
            }"#]],
    );
}

#[test]
fn interface_test() {
    check(
        &run_formatter!(
            interface_decl(),
            "interface IERC20 {
            fn totalSupply() -> int;
            fn balanceOf(account: int) -> int;
            fn allowance(owner: int, spender: int) -> int;
        } "
        ),
        expect_test::expect![[r#"
            interface IERC20 {
                fn totalSupply() -> int;
                fn balanceOf(account: int) -> int;
                fn allowance(owner: int, spender: int) -> int;
            }
        "#]],
    );
    check(
        &run_formatter!(interface_decl(), "interface IERC20 {}"),
        expect_test::expect![[r#"
            interface IERC20 {}
        "#]],
    );
}

#[test]
fn contract_decl() {
    check(
        &run_formatter!(yurt_program(), "contract MyToken(0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48) implements IERC20, Ownable {
            fn foo() -> int;
            fn bar() -> int;
        }"),
        expect_test::expect![[r#"
            contract MyToken(0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48) implements IERC20, Ownable {
                fn foo() -> int;
                fn bar() -> int;
            }
        "#]],
    );
    check(
        &run_formatter!(
            yurt_program(),
            "contract MyToken(foo) implements IERC20, Ownable {}"
        ),
        expect_test::expect![[r#"
            contract MyToken(foo) implements IERC20, Ownable {}
        "#]],
    );
    check(
        &run_formatter!(yurt_program(), "contract MyToken(foo) {}"),
        expect_test::expect![[r#"
            contract MyToken(foo) {}
        "#]],
    );
}
