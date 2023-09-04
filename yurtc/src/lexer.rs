use crate::{
    error::{Error, LexError},
    span::Span,
};
use chumsky::prelude::*;
use itertools::{Either, Itertools};
use logos::Logos;
use std::{fmt, path::Path, rc::Rc};

#[cfg(test)]
mod tests;

#[derive(Clone, Debug, Eq, Hash, Logos, PartialEq, Ord, PartialOrd)]
#[logos(skip r"[ \t\n\r\f]+")]
#[logos(error = LexError)]
pub(super) enum Token<'sc> {
    #[token(":")]
    Colon,
    #[token("::")]
    DoubleColon,
    #[token("!")]
    Bang,
    #[token("|")]
    Pipe,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("/")]
    Div,
    #[token("%")]
    Mod,
    #[token("=")]
    Eq,
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
    #[token("'")]
    SingleQuote,

    #[token(";")]
    Semi,
    #[token(",")]
    Comma,
    #[token("*")]
    Star,
    #[token("{")]
    BraceOpen,
    #[token("}")]
    BraceClose,
    #[token("(")]
    ParenOpen,
    #[token(")")]
    ParenClose,
    #[token("[")]
    BracketOpen,
    #[token("]")]
    BracketClose,
    #[token("->")]
    Arrow,
    #[token("=>")]
    HeavyArrow,
    #[token(".")]
    Dot,

    #[token("real")]
    Real,
    #[token("int")]
    Int,
    #[token("bool")]
    Bool,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("string")]
    String,

    #[token("fn")]
    Fn,

    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("cond")]
    Cond,

    #[token("let")]
    Let,
    #[token("state")]
    State,
    #[token("enum")]
    Enum,
    #[token("type")]
    Type,
    #[token("constraint")]
    Constraint,
    #[token("maximize")]
    Maximize,
    #[token("minimize")]
    Minimize,
    #[token("solve")]
    Solve,
    #[token("satisfy")]
    Satisfy,

    #[token("use")]
    Use,
    #[token("as")]
    As,

    #[token("interface")]
    Interface,
    #[token("contract")]
    Contract,
    #[token("implements")]
    Implements,
    #[token("extern")]
    Extern,

    #[token("in")]
    In,

    #[regex(r"[A-Za-z_][A-Za-z_0-9]*", |lex| lex.slice())]
    Ident(&'sc str),
    #[regex(r"[0-9]+\.[0-9]+([Ee][-+]?[0-9]+)?|[0-9]+[Ee][-+]?[0-9]+", |lex| lex.slice())]
    RealLiteral(&'sc str),
    #[regex(r"0x[0-9A-Fa-f]+|0b[0-1]+|[0-9]+", |lex| lex.slice())]
    IntLiteral(&'sc str),
    #[regex(
        r#""([^"\\]|\\(x[0-9a-fA-F]{2}|[nt"]|\\|\n))*""#,
        |lex| {
            StringLiteralChar::lexer(lex.slice())
                .map(|c| c.map(char::from))
                .collect::<Result<String, _>>()
                .unwrap()
        }
    )]
    StringLiteral(String),

    #[regex(r"//[^\n\r]*", logos::skip)]
    Comment,
}

pub(super) static KEYWORDS: &[Token] = &[
    Token::Real,
    Token::Int,
    Token::Bool,
    Token::True,
    Token::False,
    Token::String,
    Token::Fn,
    Token::If,
    Token::Else,
    Token::Cond,
    Token::Let,
    Token::State,
    Token::Constraint,
    Token::Maximize,
    Token::Minimize,
    Token::Solve,
    Token::Satisfy,
    Token::Use,
    Token::As,
    Token::Enum,
    Token::Interface,
    Token::Contract,
    Token::Implements,
    Token::Extern,
    Token::In,
    Token::Type,
];

impl<'sc> fmt::Display for Token<'sc> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Colon => write!(f, ":"),
            Token::DoubleColon => write!(f, "::"),
            Token::Bang => write!(f, "!"),
            Token::Pipe => write!(f, "|"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Div => write!(f, "/"),
            Token::Mod => write!(f, "%"),
            Token::Eq => write!(f, "="),
            Token::Gt => write!(f, ">"),
            Token::Lt => write!(f, "<"),
            Token::LtEq => write!(f, "<="),
            Token::GtEq => write!(f, ">="),
            Token::EqEq => write!(f, "=="),
            Token::NotEq => write!(f, "!="),
            Token::DoubleAmpersand => write!(f, "&&"),
            Token::DoublePipe => write!(f, "||"),
            Token::SingleQuote => write!(f, "'"),
            Token::Semi => write!(f, ";"),
            Token::Comma => write!(f, ","),
            Token::Star => write!(f, "*"),
            Token::BraceOpen => write!(f, "{{"),
            Token::BraceClose => write!(f, "}}"),
            Token::ParenOpen => write!(f, "("),
            Token::ParenClose => write!(f, ")"),
            Token::BracketOpen => write!(f, "["),
            Token::BracketClose => write!(f, "]"),
            Token::Arrow => write!(f, "->"),
            Token::HeavyArrow => write!(f, "=>"),
            Token::Dot => write!(f, "."),
            Token::Real => write!(f, "real"),
            Token::Int => write!(f, "int"),
            Token::Bool => write!(f, "bool"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::String => write!(f, "string"),
            Token::Fn => write!(f, "fn"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Cond => write!(f, "cond"),
            Token::Let => write!(f, "let"),
            Token::State => write!(f, "state"),
            Token::Enum => write!(f, "enum"),
            Token::Type => write!(f, "type"),
            Token::Constraint => write!(f, "constraint"),
            Token::Maximize => write!(f, "maximize"),
            Token::Minimize => write!(f, "minimize"),
            Token::Solve => write!(f, "solve"),
            Token::Satisfy => write!(f, "satisfy"),
            Token::Use => write!(f, "use"),
            Token::As => write!(f, "as"),
            Token::Interface => write!(f, "interface"),
            Token::Contract => write!(f, "contract"),
            Token::Implements => write!(f, "implements"),
            Token::Extern => write!(f, "extern"),
            Token::In => write!(f, "in"),
            Token::Ident(ident) => write!(f, "{ident}"),
            Token::RealLiteral(ident) => write!(f, "{ident}"),
            Token::IntLiteral(ident) => write!(f, "{ident}"),
            Token::StringLiteral(contents) => write!(f, "{contents}"),
            Token::Comment => write!(f, "comment"),
        }
    }
}

/// Lex a stream of characters. Return a list of discovered tokens and a list of errors encountered
/// along the way.
pub(super) fn lex(src: &str, filepath: Rc<Path>) -> (Vec<(Token, Span)>, Vec<Error>) {
    Token::lexer(src).spanned().partition_map(|(r, span)| {
        let span = Span::new(Rc::clone(&filepath), span);
        match r {
            Ok(v) => Either::Left((v, span)),
            Err(v) => Either::Right(Error::Lex { span, error: v }),
        }
    })
}

#[derive(Clone, Debug, Eq, Hash, Logos, PartialEq, Ord, PartialOrd)]
#[logos(error = LexError)]
enum StringLiteralChar {
    // The lex.slice() is the whole matched '\xDD'.  It's easy to create an invalid character this
    // way as far as Rust is concerned, so if it fails we currently return 0.  Supporting UTF8
    // properly or treating Yurt strings as `[u8]` instead of `String` is a TODO issue.
    #[regex(r"\\x[0-9a-fA-F]{2}",
        |lex| {
            char::from_u32(
                lex.slice()
                .chars()
                .skip(2)
                .fold(0, |n, c| n * 16 + c.to_digit(16).unwrap()),
            )
            .unwrap_or('\x00')
        }
    )]
    HexEscape(char),

    #[token(r"\n", |_| '\n')]
    Newline(char),

    #[token(r"\t", |_| '\t')]
    Tab(char),

    #[token(r#"\""#, |_| '\"')]
    DoubleQuote(char),

    #[token(r"\\", |_| '\\')]
    Backslash(char),

    #[regex(r"\\\n[ \t]*", logos::skip)]
    JoinNewline,

    #[token(r#"""#, logos::skip)]
    Delimiter,

    #[regex(r#"[^"\\]"#, |lex| lex.slice().chars().next().unwrap())]
    Any(char),
}

impl From<StringLiteralChar> for char {
    fn from(value: StringLiteralChar) -> Self {
        match value {
            StringLiteralChar::HexEscape(c)
            | StringLiteralChar::Newline(c)
            | StringLiteralChar::Tab(c)
            | StringLiteralChar::DoubleQuote(c)
            | StringLiteralChar::Backslash(c)
            | StringLiteralChar::Any(c) => c,

            StringLiteralChar::JoinNewline | StringLiteralChar::Delimiter => {
                unreachable!("Should be skipped by the tokenizer.")
            }
        }
    }
}
