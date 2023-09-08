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
    #[token("::")]
    DoubleColon,
    #[token(";")]
    Semi,
    #[token("!")]
    Bang,
    #[token("*")]
    Star,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("/")]
    Div,
    #[token("%")]
    Mod,
    #[token(">")]
    Gt,
    #[token("<")]
    Lt,
    #[token("<=")]
    LtEq,
    #[token(">=")]
    GtEq,
    #[token("==")]
    EqEq,
    #[token("!=")]
    NotEq,
    #[token("&&")]
    DoubleAmpersand,
    #[token("||")]
    DoublePipe,
    #[regex(r"int|bool|string|real", |lex| lex.slice())]
    Primitive(&'sc str),

    #[token("let")]
    Let,

    #[regex(r"satisfy|minimize|maximize", |lex| lex.slice())]
    Directive(&'sc str),
    #[token("solve")]
    Solve,

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
            Token::DoubleColon => write!(f, "::"),
            Token::Semi => write!(f, ";"),
            Token::Bang => write!(f, "!"),
            Token::Star => write!(f, "*"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Div => write!(f, "/"),
            Token::Mod => write!(f, "%"),
            Token::Gt => write!(f, ">"),
            Token::Lt => write!(f, "<"),
            Token::LtEq => write!(f, "<="),
            Token::GtEq => write!(f, ">="),
            Token::EqEq => write!(f, "=="),
            Token::NotEq => write!(f, "!="),
            Token::DoubleAmpersand => write!(f, "&&"),
            Token::DoublePipe => write!(f, "||"),
            Token::Primitive(ident) => write!(f, "{ident}"),
            Token::Let => write!(f, "let"),
            Token::Directive(contents) => write!(f, "{contents}"),
            Token::Solve => write!(f, "solve"),
            Token::Ident(ident) => write!(f, "{ident}"),
            Token::Literal(contents) => write!(f, "{contents}"),
            Token::Comment => write!(f, "comment"),
        }
    }
}
