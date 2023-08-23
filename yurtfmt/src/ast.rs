#[cfg(test)]
use crate::parser::*;
use crate::{
    error::FormatterError,
    formatter::{Format, FormattedCode},
    lexer::Token,
};
#[cfg(test)]
use chumsky::{prelude::*, Stream};
use std::fmt::Write;

pub(super) type Ast<'sc> = Vec<Decl<'sc>>;

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Decl<'sc> {
    Value {
        let_token: Token<'sc>,
        name: String,
        eq_token: Token<'sc>,
        init: Expr,
    },
}

impl<'sc> Format for Decl<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        match self {
            Self::Value {
                let_token,
                name,
                eq_token,
                init,
            } => {
                write!(formatted_code, "{} {} {} ", let_token, name, eq_token)?;
                init.format(formatted_code)?;
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
    Real(f64),
}

impl Format for Immediate {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        match self {
            Self::Real(val) => write!(formatted_code, "{}", val)?,
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
let x =    5; 


let     y     =    7.777   ;
"#
        ),
        expect_test::expect![[r#"
            let x = 5;
            let y = 7.777;
        "#]],
    );
}
