#[cfg(test)]
use crate::formatter::Format;

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
                    let mut formatted_code = String::default();
                    match ast.format(&mut formatted_code) {
                        Ok(_) => formatted_code,
                        Err(error) => format!("{}", error),
                    }
                }
                Err(errors) => format!(
                    "{}",
                    // Print each parsing.
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
            "type MyTuple = { x: int, y: real, z: string };"
        ),
        expect_test::expect!["type MyTuple = { x: int, y: real, z: string };"],
    );
}
