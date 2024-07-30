mod tests;

use crate::error::{FormatterError, LexError, Span};
use itertools::{Either, Itertools};
use logos::{Logos, SpannedIter};
use std::fmt;

#[derive(Clone, Debug, Eq, Hash, Logos, PartialEq)]
#[logos(skip r"[ \t\r\f]+")]
#[logos(error = LexError)]
pub enum Token {
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
    #[token("[")]
    BracketOpen,
    #[token("]")]
    BracketClose,
    #[token("(")]
    ParenOpen,
    #[token(")")]
    ParenClose,
    #[token("->")]
    Arrow,
    #[token("=>")]
    HeavyArrow,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token(".")]
    Dot,
    #[token("..")]
    TwoDots,
    #[regex(r"/|%|\*|>|<|<=|>=|!=|==|&&|\|\|", |lex| lex.slice().to_string())]
    BinaryOp(String),
    #[regex(r"int|bool|string|real", |lex| lex.slice().to_string())]
    Primitive(String),

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
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("cond")]
    Cond,
    #[token("enum")]
    Enum,
    #[token("state")]
    State,

    #[regex(r"satisfy|minimize|maximize", |lex| lex.slice().to_string())]
    Directive(String),
    #[token("use")]
    Use,
    #[token("as")]
    As,
    #[token("solve")]
    Solve,

    #[regex(r"[A-Za-z_][A-Za-z_0-9]*", |lex| lex.slice().to_string(), priority = 1)]
    Ident(String),
    #[regex(
        r#"(?x)
        true|false
        |[0-9]+\.[0-9]+([Ee][-+]?[0-9]+)?|[0-9]+[Ee][-+]?[0-9]+|0x[0-9A-Fa-f]+|0b[0-1]+|[0-9]+
        |"([^"\\]|\\(x[0-9a-fA-F]{2}|[nt"]|\\|\n))*"
        "#,
        |lex| lex.slice().to_string(),
        priority = 2
    )]
    Literal(String),

    #[regex(r"//.*\n?", |lex| lex.slice().to_string())]
    Comment(String),

    #[regex(r"\n|\\n")]
    Newline,
}

pub(super) fn lex(src: &str) -> (Vec<(Token, Span)>, Vec<FormatterError>) {
    Token::lexer(src)
        .spanned()
        .partition_map(|(r, span)| match r {
            Ok(v) => Either::Left((v, span)),
            Err(v) => Either::Right(FormatterError::Lex { span, error: v }),
        })
}

impl fmt::Display for Token {
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
            Token::BracketOpen => write!(f, "["),
            Token::BracketClose => write!(f, "]"),
            Token::ParenOpen => write!(f, "("),
            Token::ParenClose => write!(f, ")"),
            Token::Arrow => write!(f, "->"),
            Token::HeavyArrow => write!(f, "=>"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Dot => write!(f, "."),
            Token::TwoDots => write!(f, ".."),
            Token::BinaryOp(op) => write!(f, "{op}"),
            Token::Primitive(ident) => write!(f, "{ident}"),
            Token::Let => write!(f, "let"),
            Token::Type => write!(f, "type"),
            Token::Constraint => write!(f, "constraint"),
            Token::Fn => write!(f, "fn"),
            Token::In => write!(f, "in"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Cond => write!(f, "cond"),
            Token::Enum => write!(f, "enum"),
            Token::State => write!(f, "state"),
            Token::Directive(contents) => write!(f, "{contents}"),
            Token::Use => write!(f, "use"),
            Token::As => write!(f, "as"),
            Token::Solve => write!(f, "solve"),
            Token::Ident(ident) => write!(f, "{ident}"),
            Token::Literal(contents) => write!(f, "{contents}"),
            Token::Comment(contents) => write!(f, "{contents}"),
            Token::Newline => write!(f, r#"\n"#),
        }
    }
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub struct Lexer<'sc> {
    token_stream: SpannedIter<'sc, Token>,
}

impl<'sc> Lexer<'sc> {
    pub fn new(input: &'sc str) -> Self {
        /* TODO: find a way to remove the newlines unless they're following semicolons
        Maybe handle it via the ast instead of running a pass over it
        Although it's simpler to just remove during this pass instead of having more
        optional stuff in the ast

        // Preserve only newlines following semicolons from token stream
        let raw_token_stream = Token::lexer(input).spanned();
        let mut tokens_iter = raw_token_stream.into_iter().peekable();
        let mut new_token_stream = SpannedIter::;
        let mut tokens_without_newlines = Vec::new();
        let mut prev_token = None;

        while let Some(token) = tokens_iter.next() {
            let token_clone = token.clone();
            match token {
                (Ok(Token::Newline), _) if matches!(prev_token, Some((Ok(Token::Semi), _))) => {
                    if matches!(tokens_iter.peek(), Some((Ok(Token::Newline), _))) {
                        tokens_without_newlines.push(token);
                    }
                }
                (Ok(Token::Newline), _) => {}
                _ => tokens_without_newlines.push(token),
            }
            prev_token = Some(token_clone);
        }

        // Provide a token stream
        // let eoi_span = source.len()..source.len();
        // let token_stream = Stream::from_iter(eoi_span.clone(), tokens_without_newlines.into_iter());

        let stream = SpannedIter::from(tokens_without_newlines); // need to convert back to token stream and it should run fine?
        */
        Self {
            token_stream: Token::lexer(input).spanned(),
        }
    }
}

impl<'sc> Iterator for Lexer<'sc> {
    type Item = Spanned<Token, usize, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.token_stream
            .next()
            .map(|(token, span)| Ok((span.start, token?, span.end)))
    }
}
