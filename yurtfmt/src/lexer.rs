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
    #[token("|")]
    Pipe,
    #[token(":")]
    Colon,
    #[token("::")]
    DoubleColon,
    #[token(";")]
    Semi,
    #[token(",")]
    Comma,
    #[token("!")]
    Bang,
    #[token("{")]
    BraceOpen,
    #[token("}")]
    BraceClose,
    #[token("(")]
    ParenOpen,
    #[token(")")]
    ParenClose,
    #[token("->")]
    Arrow,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[regex(r"/|%|\*|>|<|<=|>=|!=|==|&&|\|\|", |lex| lex.slice())]
    BinaryOp(&'sc str),
    #[regex(r"int|bool|string|real", |lex| lex.slice())]
    Primitive(&'sc str),

    #[token("let")]
    Let,
    #[token("type")]
    Type,
    #[token("constraint")]
    Constraint,
    #[token("fn")]
    Fn,
    #[token("in")]
    In,
    #[token("enum")]
    Enum,
    #[token("contract")]
    Contract,
    #[token("implements")]
    Implements,
    #[token("interface")]
    Interface,
    #[token("state")]
    State,
    #[token("extern")]
    Extern,

    #[regex(r"satisfy|minimize|maximize", |lex| lex.slice())]
    Directive(&'sc str),
    #[token("use")]
    Use,
    #[token("as")]
    As,
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
            Token::Pipe => write!(f, "|"),
            Token::Colon => write!(f, ":"),
            Token::DoubleColon => write!(f, "::"),
            Token::Semi => write!(f, ";"),
            Token::Comma => write!(f, ","),
            Token::Bang => write!(f, "!"),
            Token::BraceOpen => write!(f, "{{"),
            Token::BraceClose => write!(f, "}}"),
            Token::ParenOpen => write!(f, "("),
            Token::ParenClose => write!(f, ")"),
            Token::Arrow => write!(f, "->"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::BinaryOp(op) => write!(f, "{op}"),
            Token::Primitive(ident) => write!(f, "{ident}"),
            Token::Let => write!(f, "let"),
            Token::Type => write!(f, "type"),
            Token::Constraint => write!(f, "constraint"),
            Token::Fn => write!(f, "fn"),
            Token::In => write!(f, "in"),
            Token::Enum => write!(f, "enum"),
            Token::Contract => write!(f, "contract"),
            Token::Implements => write!(f, "implements"),
            Token::Interface => write!(f, "interface"),
            Token::State => write!(f, "state"),
            Token::Extern => write!(f, "extern"),
            Token::Directive(contents) => write!(f, "{contents}"),
            Token::Use => write!(f, "use"),
            Token::As => write!(f, "as"),
            Token::Solve => write!(f, "solve"),
            Token::Ident(ident) => write!(f, "{ident}"),
            Token::Literal(contents) => write!(f, "{contents}"),
            Token::Comment => write!(f, "comment"),
        }
    }
}
