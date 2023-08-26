#[cfg(test)]
use crate::parser::*;
use crate::{
    error::FormatterError,
    formatter::{Format, FormattedCode},
    lexer::Token,
};
#[cfg(test)]
use chumsky::{prelude::*, Stream};
use std::fmt::{self, Write};

pub(super) type Ast<'sc> = Vec<Decl<'sc>>;

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Decl<'sc> {
    Value {
        let_token: Token<'sc>,
        name: String,
        colon_token_and_ty: Option<(Token<'sc>, Type)>,
        eq_token_and_init: Option<(Token<'sc>, Expr)>,
    },
}

impl<'sc> Format for Decl<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        match self {
            Self::Value {
                let_token,
                name,
                colon_token_and_ty,
                eq_token_and_init,
            } => {
                write!(formatted_code, "{} {}", let_token, name)?;

                if let Some((colon_token, ty)) = colon_token_and_ty {
                    write!(formatted_code, " {} {}", colon_token, ty)?;
                }

                if let Some((eq_token, init)) = eq_token_and_init {
                    write!(formatted_code, " {} ", eq_token)?;
                    init.format(formatted_code)?;
                }
            }
        }

        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Expr {
    Immediate(Immediate),
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Immediate {
    Number(String),
    Bool(String),
    String(String),
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Type {
    Primitive(String),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Primitive(primitive_ty) => write!(f, "{primitive_ty}"),
        }
    }
}

impl Format for Immediate {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        match self {
            Self::Number(val) => write!(formatted_code, "{}", val)?,
            Self::Bool(val) => write!(formatted_code, "{}", val)?,
            Self::String(val) => write!(formatted_code, "\"{}\"", val)?,
        }

        Ok(())
    }
}

impl Format for Expr {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        match self {
            Self::Immediate(immediate) => immediate.format(formatted_code)?,
        }

        Ok(())
    }
}

impl<'sc> Format for Ast<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        for node in self {
            node.format(formatted_code)?;
            writeln!(formatted_code, "{}", Token::Semi)?;
        }

        Ok(())
    }
}

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
                        Err(error) => format!("{error}"),
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
            found end of input but expected ";"
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

    let  str_var =     "sample";

 let  real_var  =    8.8888E+5;

     let hex_var =   0xFF;

let bin_var  =0b1010;
"#
        ),
        expect_test::expect![[r#"
                let x = 5;
                let y = 7.777;
                let bool_var = true;
                let str_var = "sample";
                let real_var = 8.8888E+5;
                let hex_var = 0xFF;
                let bin_var = 0b1010;
            "#]],
    );

    check(
        &run_formatter!(
            yurt_program(),
            r#"
    let  x  :  int   =   5;

   let   y:  real  =  7.777;

    let   bool_var : bool    = true;

 let str_var  : string  =    "sample";

     let real_var:   real = 8.8888E+5;

  let   hex_var:int =   0xFF;

let bin_var :  int=0b1010;
"#
        ),
        expect_test::expect![[r#"
                let x : int = 5;
                let y : real = 7.777;
                let bool_var : bool = true;
                let str_var : string = "sample";
                let real_var : real = 8.8888E+5;
                let hex_var : int = 0xFF;
                let bin_var : int = 0b1010;
            "#]],
    );

    check(
        &run_formatter!(
            yurt_program(),
            r#"
    let     x ;

    let y;

    let  bool_var   ;

    let    str_var;

    let real_var  ;

    let   hex_var ;

let   bin_var  ;
        "#
        ),
        expect_test::expect![[r#"
                let x;
                let y;
                let bool_var;
                let str_var;
                let real_var;
                let hex_var;
                let bin_var;
            "#]],
    );
}
