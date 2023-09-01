mod tests;

use crate::error::{FormatterError, LexError, Span};
use itertools::{Either, Itertools};
use logos::Logos;
use std::fmt;

#[derive(Clone, Debug, Eq, Hash, Logos, PartialEq)]
#[logos(skip r"[ \t\n\r\f]+")]
#[logos(error = LexError)]
pub(super) enum Token<'sc> {
    #[token("=")]
    Eq,
    #[token(":")]
    Colon,
    #[token(";")]
    Semi,
    #[regex(r"int|bool|string|real", |lex| lex.slice())]
    Primitive(&'sc str),

    #[token("let")]
    Let,

    #[token("maximize")]
    // Probably combine maximize, minimize, satisfy together? Or at least maximize and minimize
    Maximize,
    #[token("minimize")]
    Minimize,
    #[token("solve")]
    Solve,
    #[token("satisfy")]
    Satisfy,

    #[regex(r"[A-Za-z_][A-Za-z_0-9]*", |lex| lex.slice())]
    Ident(&'sc str),
    #[regex(
        r#"(?x)
        true|false
        |[0-9]+\.[0-9]+([Ee][-+]?[0-9]+)?|[0-9]+[Ee][-+]?[0-9]+|0x[0-9A-Fa-f]+|0b[0-1]+|[0-9]+
        |"([^"\\]|\\(x[0-9a-fA-F]{2}|[nt"]|\\|\n))*"
        "#,
        |lex| lex.slice(),
        priority = 2
    )]
    Literal(&'sc str),

    #[regex(r"//[^\n\r]*", logos::skip)]
    Comment,
}

pub(super) fn lex(src: &str) -> (Vec<(Token, Span)>, Vec<FormatterError>) {
    Token::lexer(src)
        .spanned()
        .partition_map(|(r, span)| match r {
            Ok(v) => Either::Left((v, span)),
            Err(v) => Either::Right(FormatterError::Lex { span, error: v }),
        })
}

impl<'sc> fmt::Display for Token<'sc> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Eq => write!(f, "="),
            Token::Colon => write!(f, ":"),
            Token::Semi => write!(f, ";"),
            Token::Primitive(ident) => write!(f, "{ident}"),
            Token::Let => write!(f, "let"),
            Token::Maximize => write!(f, "maximize"),
            Token::Minimize => write!(f, "minimize"),
            Token::Solve => write!(f, "solve"),
            Token::Satisfy => write!(f, "satisfy"),
            Token::Ident(ident) => write!(f, "{ident}"),
            Token::Literal(contents) => write!(f, "{contents}"),
            Token::Comment => write!(f, "comment"),
        }
    }
}
