#[cfg(test)]
use crate::formatter::{Format, FormattedCode};
use crate::lexer::Token;

#[cfg(test)]
use crate::parser::*;

#[cfg(test)]
use chumsky::{prelude::*, Stream};

#[cfg(test)]
use expect_test::{expect, Expect};

#[cfg(test)]
fn check(actual: &str, expect: Expect) {
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
            // Preserve only newlines following semicolons from token stream
            let mut tokens_iter = toks.into_iter().peekable();
            let mut toks_without_newlines = Vec::new();
            let mut prev_token = None;

            while let Some(token) = tokens_iter.next() {
                let token_clone = token.clone();
                match token {
                    (Token::Newline, _) if matches!(prev_token, Some((Token::Semi, _))) => {
                        if matches!(tokens_iter.peek(), Some((Token::Newline, _))) {
                            toks_without_newlines.push(token);
                        }
                    }
                    (Token::Newline, _) => {}
                    _ => toks_without_newlines.push(token),
                }
                prev_token = Some(token_clone);
            }
            let token_stream = Stream::from_iter(
                $source.len()..$source.len(),
                toks_without_newlines.into_iter(),
            );
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
        &run_formatter!(pint_program(), r#"@@@"#),
        expect![[r#"
            invalid token
            invalid token
            invalid token
        "#]],
    );

    check(
        &run_formatter!(pint_program(), r#"let x = 5"#),
        expect![[r#"
            Error formatting starting at location 9 and ending at location 9
        "#]],
    );
}

#[test]
fn let_decls() {
    check(
        &run_formatter!(
            pint_program(),
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

let x = y in        1..2;
"#
        ),
        expect![[r#"
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

                let x = y in 1..2;
            "#]],
    );

    check(
        &run_formatter!(
            pint_program(),
            r#"
    let  x  :  int   =   5;

   let   y:  real  =  7.777;

    let   bool_var : bool    = true;

 let str_var  : string  =    "this sample has spaces";

     let real_var:   real = 8.8888E+5;

  let   hex_var:int =   0xFF;

let bin_var :  int=0b1010;

     let         bigint_var:        int=1234567890123456789012345678901234567890;

     let x      :int =          y in 1.. 2    ;
"#
        ),
        expect![[r#"
                let x: int = 5;

                let y: real = 7.777;

                let bool_var: bool = true;

                let str_var: string = "this sample has spaces";

                let real_var: real = 8.8888E+5;

                let hex_var: int = 0xFF;

                let bin_var: int = 0b1010;

                let bigint_var: int = 1234567890123456789012345678901234567890;

                let x: int = y in 1..2;
            "#]],
    );

    check(
        &run_formatter!(
            pint_program(),
            r#"
    let     x ;
    let  bool_var   ;
        "#
        ),
        expect![[r#"
                let x;
                let bool_var;
            "#]],
    );

    check(
        &run_formatter!(
            pint_program(),
            r#"
            let   t
            : {  int , real
            , string };
            "#
        ),
        expect![[r#"
            let t: { int, real, string };
        "#]],
    );
}

#[test]
fn solve_decls() {
    check(
        &run_formatter!(
            pint_program(),
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
        expect![[r#"
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
    check(
        &run_formatter!(
            pint_program(),
            r#"
            constraint         blah;
    constraint a    + b    <=c      ;
            constraint
                !   foo;
constraint a::    b::d       ;
        "#
        ),
        expect![[r#"
        constraint blah;
        constraint a + b <= c;
        constraint !foo;
        constraint a::b::d;
        "#]],
    );
}

#[test]
fn paths() {
    use crate::parser;
    check(
        &run_formatter!(parser::path(), r#"::     foo    ::   bar   "#),
        expect![[r#"::foo::bar"#]],
    );
    check(
        &run_formatter!(path(), "foo    ::   \n   bar"),
        expect![[r#"foo::bar"#]],
    );
    check(
        &run_formatter!(path(), "_foo_  \n :: \n  _bar"),
        expect![[r#"_foo_::_bar"#]],
    );
    check(&run_formatter!(path(), "_  ::   _"), expect![[r#"_::_"#]]);
    check(
        &run_formatter!(path(), "t2::   \n_3t::  t4_   :: t"),
        expect![[r#"t2::_3t::t4_::t"#]],
    );
    check(
        &run_formatter!(path(), ":: foo   ::bar "),
        expect![[r#"::foo::bar"#]],
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
        expect!["use x;"],
    );
    check(
        &run_formatter!(
            use_statement(),
            "
                use   a::b::c;"
        ),
        expect![[r#"use a::b::c;"#]],
    );
    check(
        &run_formatter!(
            use_statement(),
            "   use {  c, d,e,
    g};  "
        ),
        expect!["use {c, d, e, g};"],
    );
    check(
        &run_formatter!(
            use_statement(),
            "
                use   q::r   as  x ;
                "
        ),
        expect!["use q::r as x;"],
    );
    check(
        &run_formatter!(use_statement(), "  use   q::r as x   ;  "),
        expect!["use q::r as x;"],
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
        expect!["use a::b::{self, c, d::e};"],
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
        expect!["use a::b::{self as ab, c, d::{e, f::g}};"],
    );
}

#[test]
fn unary_op_exprs() {
    check(&run_formatter!(expr(), "   !a   "), expect![[r#"!a"#]]);
    check(&run_formatter!(expr(), "   +a   "), expect![[r#"+a"#]]);
    check(&run_formatter!(expr(), " -   a"), expect![[r#"-a"#]]);
    check(&run_formatter!(expr(), "+7"), expect![[r#"+7"#]]);
    check(&run_formatter!(expr(), "+    3.4"), expect![[r#"+3.4"#]]);
    check(&run_formatter!(expr(), "-  1.0"), expect![[r#"-1.0"#]]);
    check(
        &run_formatter!(expr(), "! - - !  -+  -1"),
        expect![[r#"!--!-+-1"#]],
    );
    check(
        &run_formatter!(expr(), "!- -! - +  -1 "),
        expect![[r#"!--!-+-1"#]],
    );
}

#[test]
fn binary_op_exprs() {
    check(
        &run_formatter!(expr(), "   a   *    2.0   "),
        expect![[r#"a * 2.0"#]],
    );
    check(
        &run_formatter!(expr(), " a /     2.0 "),
        expect![[r#"a / 2.0"#]],
    );
    check(
        &run_formatter!(expr(), "a   %   2.0"),
        expect![[r#"a % 2.0"#]],
    );
    check(
        &run_formatter!(expr(), "  a   +  2.0 "),
        expect![[r#"a + 2.0"#]],
    );
    check(
        &run_formatter!(expr(), " a   - 2.0   "),
        expect![[r#"a - 2.0"#]],
    );
    check(&run_formatter!(expr(), "a+   2.0"), expect![[r#"a + 2.0"#]]);
    check(
        &run_formatter!(expr(), " a- 2.0   "),
        expect![[r#"a - 2.0"#]],
    );
    check(
        &run_formatter!(expr(), "   a <  2.0   "),
        expect![[r#"a < 2.0"#]],
    );
    check(
        &run_formatter!(expr(), " a   >    2.0 "),
        expect![[r#"a > 2.0"#]],
    );
    check(
        &run_formatter!(expr(), "a   <= 2.0   "),
        expect![[r#"a <= 2.0"#]],
    );
    check(
        &run_formatter!(expr(), "   a  >=  2.0  "),
        expect![[r#"a >= 2.0"#]],
    );
    check(
        &run_formatter!(expr(), "  a   ==  2.0   "),
        expect![[r#"a == 2.0"#]],
    );
    check(
        &run_formatter!(expr(), " a  != 2.0 "),
        expect![[r#"a != 2.0"#]],
    );
    check(
        &run_formatter!(expr(), "a  &&  b  "),
        expect![[r#"a && b"#]],
    );
    check(
        &run_formatter!(expr(), "   a ||  b  "),
        expect![[r#"a || b"#]],
    );
    check(
        &run_formatter!(expr(), "   a     ||  b &&     c  ||      d      &&     ! e"),
        expect![[r#"a || b && c || d && !e"#]],
    );
}

#[test]
fn custom_types() {
    check(
        &run_formatter!(
            pint_program(),
            r#"
                type MyTuple =  {
                    x:  int , y:    real, z:
                string };
            "#
        ),
        expect![[r#"
            type MyTuple = { x: int, y: real, z: string };
        "#]],
    );
    check(
        &run_formatter!(
            pint_program(),
            r#"
                type   MyTuple
            = { real,
                bool, z
            :   string };
            "#
        ),
        expect![[r#"
            type MyTuple = { real, bool, z: string };
        "#]],
    );
}

#[test]
fn func_decl() {
    check(
        &run_formatter!(
            pint_program(),
            r#"
            fn foo(
                        x: real,
        y: real
            )   ->       real {
    y + 1
            }"#
        ),
        expect![[r#"
        fn foo(x: real, y: real) -> real {
            y + 1
        }
        
        "#]],
    );
}

#[test]
fn state_decl() {
    check(
        &run_formatter!(
            pint_program(),
            "  state x:   int   =
            MyPath::getBalance;

                state y=  CryptoExchange::convertToEth;
       state z :  int
       =   totalSupply + mintedTokens;
            state  w =
            -  5.5  ;

            state v   :  bool = !isWhitelisted;

            state u
            = Token::Metadata::uri;

    state tx: int = CryptoUtils::hashTransaction;
            "
        ),
        expect![[r#"
            state x: int = MyPath::getBalance;

            state y = CryptoExchange::convertToEth;
            state z: int = totalSupply + mintedTokens;
            state w = -5.5;

            state v: bool = !isWhitelisted;

            state u = Token::Metadata::uri;

            state tx: int = CryptoUtils::hashTransaction;
          "#]],
    );
}

#[test]
fn enum_decl() {
    check(
        &run_formatter!(pint_program(), "   enum   Colour=Red|Green|Blue  ;  "),
        expect![[r#"enum Colour = Red | Green | Blue;"#]],
    );
    check(
        &run_formatter!(
            pint_program(),
            "enum       MyEnum =Variant1
        ;"
        ),
        expect![[r#"enum MyEnum = Variant1;"#]],
    );
    check(
        &run_formatter!(
            pint_program(),
            "enum
         Weather
             =Sunny
    |
     Rainy;"
        ),
        expect![[r#"enum Weather = Sunny | Rainy;"#]],
    );
}

#[test]
fn array_types() {
    check(
        &run_formatter!(type_(expr()), "int[   10   ]"),
        expect![[r#"int[10]"#]],
    );
    check(
        &run_formatter!(
            type_(expr()),
            "string[
    Day
    ]"
        ),
        expect![[r#"string[Day]"#]],
    );
    check(
        &run_formatter!(
            type_(expr()),
            "bool [  10  ] [
    Colour
    ]"
        ),
        expect![[r#"bool[10][Colour]"#]],
    );
    check(
        &run_formatter!(type_(expr()), "real [3] [ 4 ][  5]"),
        expect![[r#"real[3][4][5]"#]],
    );
    check(
        &run_formatter!(
            type_(expr()),
            "

    real [   0   ] [ 2 ]"
        ),
        expect![[r#"real[0][2]"#]],
    );
    check(
        &run_formatter!(
            type_(expr()),
            "string[
    N
    ][
    Colour
    ]"
        ),
        expect![[r#"string[N][Colour]"#]],
    );
    check(
        &run_formatter!(type_(expr()), "string[  N   ][Colour  ]"),
        expect![[r#"string[N][Colour]"#]],
    );
    check(
        &run_formatter!(type_(expr()), "bool[N][Colour]    "),
        expect![[r#"bool[N][Colour]"#]],
    );
}

#[test]
fn call_expressions() {
    check(
        &run_formatter!(expr(), "foo        ()"),
        expect![[r#"foo()"#]],
    );
    check(
        &run_formatter!(
            expr(),
            "foo( 5,
            2 )"
        ),
        expect![[r#"foo(5, 2)"#]],
    );
    check(
        &run_formatter!(expr(), "foo(    5  ,  10    ,20)"),
        expect![[r#"foo(5, 10, 20)"#]],
    );
    check(
        &run_formatter!(
            expr(),
            "foo(
5,
    10,
            20
)"
        ),
        expect![[r#"foo(5, 10, 20)"#]],
    );
    check(
        &run_formatter!(
            expr(),
            "foo(   5 ,
    10  ,
     20   )"
        ),
        expect![[r#"foo(5, 10, 20)"#]],
    );
    check(
        &run_formatter!(expr(), "foo(bar(5,6), 2)"),
        expect![[r#"foo(bar(5, 6), 2)"#]],
    );
    check(
        &run_formatter!(
            expr(),
            "

foo  (  5,2  )"
        ),
        expect![[r#"foo(5, 2)"#]],
    );
    check(
        &run_formatter!(expr(), "foo(5, 2,)"),
        expect![[r#"foo(5, 2)"#]],
    );
    check(
        &run_formatter!(expr(), "           foo    (5, 2)"),
        expect![[r#"foo(5, 2)"#]],
    );
}

#[test]
fn in_expressions() {
    check(
        &run_formatter!(expr(), "42    in y"),
        expect![[r#"42 in y"#]],
    );
    check(
        &run_formatter!(
            expr(),
            "
!x in y"
        ),
        expect![[r#"!x in y"#]],
    );
    check(
        &run_formatter!(
            expr(),
            "x in   y
&&    z in w"
        ),
        expect![[r#"x in y && z in w"#]],
    );
    check(
        &run_formatter!(
            expr(),
            "!x   in
y ||    42 in   z"
        ),
        expect![[r#"!x in y || 42 in z"#]],
    );
}

#[test]
fn range_expressions() {
    check(
        &run_formatter!(range(expr()), "1..     2"),
        expect![[r#"1..2"#]],
    );
    check(
        &run_formatter!(range(expr()), "1+2     ..3+4"),
        expect!["1 + 2..3 + 4"],
    );
    check(
        &run_formatter!(
            range(expr()),
            "1.1
                    ..
        2.2e3"
        ),
        expect!["1.1..2.2e3"],
    );
}

#[test]
fn casting() {
    check(
        &run_formatter!(expr(), "5 as      int"),
        expect![[r#"5 as int"#]],
    );
    check(
        &run_formatter!(expr(), "5    as int     as real as    int"),
        expect![[r#"5 as int as real as int"#]],
    );
    check(
        &run_formatter!(
            expr(),
            "5 as { x: int,
            y: real,    z: string }"
        ),
        expect![[r#"5 as { x: int, y: real, z: string }"#]],
    );
    check(
        &run_formatter!(expr(), "a[ 5   ]      [       3    ]   as real"),
        expect![[r#"a[5][3] as real"#]],
    );
}

#[test]
fn if_exprs() {
    check(
        &run_formatter!(
            expr(),
            "if
        1 ==
        2 {         3 }
        else {
            4 }"
        ),
        expect![[r#"
        if 1 == 2 {
            3
        } else {
            4
        }"#]],
    );
}

#[test]
fn cond_exprs() {
    check(
        &run_formatter!(cond_expr(expr()), "cond {   else => {  a }  }"),
        expect![[r#"
        cond {
            else => {
                a
            },
        }"#]],
    );
    check(
        &run_formatter!(
            cond_expr(expr()),
            "cond
            {
    else => a
    , }"
        ),
        expect![[r#"
        cond {
            else => a,
        }"#]],
    );
    check(
        &run_formatter!(
            cond_expr(expr()),
            "cond {

              a =>
               b,
        else => c }"
        ),
        expect![[r#"
        cond {
            a => b,
            else => c,
        }"#]],
    );
    check(
        &run_formatter!(
            cond_expr(expr()),
            "cond {
                a           =>{
                    b  },else => c,  }"
        ),
        expect![[r#"
        cond {
            a => {
                b
            },
            else => c,
        }"#]],
    );
    check(
        &run_formatter!(cond_expr(expr()), "cond {a => b,{true}=>d,else=>f,}"),
        expect![[r#"
        cond {
            a => b,
            {
                true
            } => d,
            else => f,
        }"#]],
    );
}

#[test]
fn tuple_expressions() {
    check(
        &run_formatter!(expr(), "{x:   0}"),
        expect_test::expect!["{ x: 0 }"],
    );
    check(
        &run_formatter!(
            expr(),
            "{0,
         }"
        ),
        expect_test::expect!["{ 0, }"],
    );
    check(
        &run_formatter!(expr(), "{x: 0,     }"),
        expect_test::expect!["{ x: 0 }"],
    );
    check(
        &run_formatter!(
            expr(),
            "{  0, 1.0,
             \"foo\" }"
        ),
        expect_test::expect![[r#"{ 0, 1.0, "foo" }"#]],
    );
    check(
        &run_formatter!(
            expr(),
            "{x: 0,
             y:   1.0, z: \"foo\"}"
        ),
        expect_test::expect![[r#"{ x: 0, y: 1.0, z: "foo" }"#]],
    );
    check(
        &run_formatter!(
            expr(),
            "{0, {1.0,
             \"bar\"}, \"foo\"}"
        ),
        expect_test::expect![[r#"{ 0, { 1.0, "bar" }, "foo" }"#]],
    );
    check(
        &run_formatter!(
            expr(),
            "{x:        0,
                {y:1.0, \"bar\"}, z:\"foo\"
            }"
        ),
        expect_test::expect![[r#"{ x: 0, { y: 1.0, "bar" }, z: "foo" }"#]],
    );
    check(
        &run_formatter!(
            expr(),
            "{ { 42, },
         if c { 2 }      else{3}, foo() }"
        ),
        expect_test::expect![
            r#"
        { { 42, }, if c {
            2
        } else {
            3
        }, foo() }"#
        ],
    );
    check(
        &run_formatter!(
            expr(),
            "{ x: { 42, }, y: if c { 2 }
            else { 3 }, z: foo() }"
        ),
        expect_test::expect![
            r#"
        { x: { 42, }, y: if c {
            2
        } else {
            3
        }, z: foo() }"#
        ],
    );
}

#[test]
fn tuple_field_accesses() {
    check(
        &run_formatter!(
            expr(),
            "t.
    1"
        ),
        expect_test::expect!["t.1"],
    );
    check(
        &run_formatter!(
            expr(),
            "t.
        1
        .1"
        ),
        expect_test::expect!["t.1.1"],
    );
    check(
        &run_formatter!(
            expr(),
            "t.a
        .
        1"
        ),
        expect_test::expect!["t.a.1"],
    );
    check(
        &run_formatter!(
            expr(),
            "t  .   0 + t   .   9999999 + t
        .x"
        ),
        expect_test::expect!["t.0 + t.9999999 + t.x"],
    );
    check(
        &run_formatter!(
            expr(),
            "{0,
            1}.0"
        ),
        expect_test::expect!["{ 0, 1 }.0"],
    );
    check(
        &run_formatter!(
            expr(),
            "{
            0
            , 1
        }.x"
        ),
        expect_test::expect!["{ 0, 1 }.x"],
    );
    check(
        &run_formatter!(
            expr(),
            "t.0
        .0"
        ),
        expect_test::expect!["t.0.0"],
    );
    check(
        &run_formatter!(
            expr(),
            "t.x
        .y"
        ),
        expect_test::expect!["t.x.y"],
    );
    check(
        &run_formatter!(
            expr(),
            "t      .1 .2.2.
         3 .     13 . 1.1"
        ),
        expect_test::expect!["t.1.2.2.3.13.1.1"],
    );
    check(
        &run_formatter!(
            expr(),
            "t
         .x .1.2.
          w .
           t. 3.4"
        ),
        expect_test::expect!["t.x.1.2.w.t.3.4"],
    );
    check(
        &run_formatter!(expr(), "foo()          .0.1"),
        expect_test::expect!["foo().0.1"],
    );
    check(
        &run_formatter!(expr(), "foo().a.   b.0.    1"),
        expect_test::expect!["foo().a.b.0.1"],
    );
    check(
        &run_formatter!(
            expr(),
            "{ {0,
            0}, }.
            0"
        ),
        expect_test::expect!["{ { 0, 0 }, }.0"],
    );
    check(
        &run_formatter!(
            expr(),
            "{ {0,
            0}, }.a"
        ),
        expect_test::expect!["{ { 0, 0 }, }.a"],
    );
    check(
        &run_formatter!(expr(), "if true { {0, 0} } else { {0, 0} }.0"),
        expect_test::expect![
            r#"
        if true {
            { 0, 0 }
        } else {
            { 0, 0 }
        }.0"#
        ],
    );
    check(
        &run_formatter!(
            expr(),
            "if true {       {0, 0} } else{{0,        0}
          }.x"
        ),
        expect_test::expect![
            r#"
        if true {
            { 0, 0 }
        } else {
            { 0, 0 }
        }.x"#
        ],
    );
    check(
        &run_formatter!(
            expr(),
            "1
        +
        2
        .3"
        ),
        expect_test::expect!["1 + 2.3"],
    );
    check(
        &run_formatter!(
            expr(),
            "1
        + 2                 .a"
        ),
        expect_test::expect!["1 + 2.a"],
    );
}

#[test]
fn array_expressions() {
    check(
        &run_formatter!(expr(), r#"[    5]"#),
        expect_test::expect!["[5]"],
    );
    check(
        &run_formatter!(
            expr(),
            r#"[5
        ,]"#
        ),
        expect_test::expect!["[5]"],
    );
    check(
        &run_formatter!(expr(), r#"[    5   , 4    ]"#),
        expect_test::expect!["[5, 4]"],
    );
    check(
        &run_formatter!(expr(), r#"[[1],]"#),
        expect_test::expect!["[[1]]"],
    );
    check(
        &run_formatter!(
            expr(),
            r#"[[1,
        2],
        3]"#
        ),
        expect_test::expect!["[[1, 2], 3]"],
    );
    check(
        &run_formatter!(
            expr(),
            r#"[
            [1,
             2],
              [3,
               4]
               ]"#
        ),
        expect_test::expect!["[[1, 2], [3, 4]]"],
    );
    check(
        &run_formatter!(
            expr(),
            r#"[
                [   foo (  ) ,  2] ,    [if  true
                {1}else{2}  ,      t    .0]]"#
        ),
        expect_test::expect![
            r#"
        [[foo(), 2], [if true {
            1
        } else {
            2
        }, t.0]]"#
        ],
    );
}

#[test]
fn array_element_accesses() {
    check(
        &run_formatter!(
            expr(),
            r#"a
        [   5   ]"#
        ),
        expect_test::expect!["a[5]"],
    );
    check(
        &run_formatter!(
            expr(),
            r#"a    [   N       ][
            5][t.
            0]"#
        ),
        expect_test::expect!["a[N][5][t.0]"],
    );
    check(
        &run_formatter!(
            expr(),
            r#"{ a }
        [N] [   foo()][     M   ][  4
        ]"#
        ),
        expect_test::expect![[r#"
            {
                a
            }[N][foo()][M][4]"#]],
    );
    check(
        &run_formatter!(
            expr(),
            r#"foo(

        )[
            {
                M
            }][
                if true { 1 } else { 3 }]"#
        ),
        expect_test::expect![[r#"
            foo()[{
                M
            }][if true {
                1
            } else {
                3
            }]"#]],
    );
    check(
        &run_formatter!(expr(), r#"a    [   MyEnum  ::  Variant1    ]"#),
        expect_test::expect!["a[MyEnum::Variant1]"],
    );
}

#[test]
fn single_line_comments() {
    check(
        &run_formatter!(pint_program(), r#"// Hello"#),
        expect_test::expect!["// Hello\n"],
    );
    check(
        &run_formatter!(
            pint_program(),
            r#"//Hello
            //World"#
        ),
        expect_test::expect!["//Hello\n//World\n"],
    );
    check(
        &run_formatter!(
            pint_program(),
            r#"// Hello
        // World
        // !"#
        ),
        expect_test::expect!["// Hello\n// World\n// !\n"],
    );
    check(
        &run_formatter!(
            pint_program(),
            r#"//       Hello
            // World
            // !
            // "#
        ),
        expect_test::expect!["//       Hello\n// World\n// !\n// \n"],
    );
    check(
        &run_formatter!(
            pint_program(),
            r#"// This comment is allowed
            constraint h1 + h2 + h3 + h4 + h5 + h6 + h7 + h8
                     + h9 + h10 + h11 + h12 + h13 + h14 + h15
                     + h16 + h17 + h18 == 72;"#
        ),
        expect_test::expect!["// This comment is allowed\nconstraint h1 + h2 + h3 + h4 + h5 + h6 + h7 + h8 + h9 + h10 + h11 + h12 + h13 + h14 + h15 + h16 + h17 + h18 == 72;\n"],
    );
    check(
        &run_formatter!(
            pint_program(),
            r#"constraint h1 + h2 + h3 + h4 + h5 + h6 + h7 + h8 // this is not supported for now
            + h9 + h10 + h11 + h12 + h13 + h14 + h15
            + h16 + h17 + h18 == 72;"#
        ),
        expect_test::expect![
            r#"
            Error formatting starting at location 49 and ending at location 82
            "#
        ],
    );
}

#[test]
fn blank_lines() {
    // Blank lines are preserved only after a semicolon token
    check(
        &run_formatter!(
            pint_program(),
            r#"
            let x = 5;
        "#
        ),
        expect_test::expect![
            r#"
            let x = 5;
        "#
        ],
    );
    // Only one blank line is preserved if more than one is present
    check(
        &run_formatter!(
            pint_program(),
            r#"
            let x = 5;



            let y = 3;
        "#
        ),
        expect_test::expect![
            r#"
            let x = 5;

            let y = 3;
        "#
        ],
    );
    // Blank lines are not added if they were not present
    check(
        &run_formatter!(
            pint_program(),
            r#"
            let x = 5;
            let y = 3;
        "#
        ),
        expect_test::expect![
            r#"
            let x = 5;
            let y = 3;
        "#
        ],
    );
    check(
        &run_formatter!(
            pint_program(),
            r#"
            let x = 5;
            let y = 3;




            let z = 1;
        "#
        ),
        expect_test::expect![
            r#"
            let x = 5;
            let y = 3;

            let z = 1;
        "#
        ],
    );
    // Blank lines are not preserved after any token other than a semicolon
    check(
        &run_formatter!(
            pint_program(),
            r#"
                fn some_call
                (
                transaction: string, 
                blockTag: string) 
                -> 
                string;
            
        "#
        ),
        expect_test::expect![[r#"
            Error formatting starting at location 163 and ending at location 164
        "#]],
    );
    check(
        &run_formatter!(
            pint_program(),
            r#"
                
                fn some_call(transaction: string, blockTag: string) -> string;
            




        "#
        ),
        expect_test::expect![[r#"
            Error formatting starting at location 95 and ending at location 96
        "#]],
    );
}
