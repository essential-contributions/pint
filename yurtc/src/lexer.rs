use crate::error::{CompileError, LexError, Span};
use itertools::{Either, Itertools};
use logos::Logos;
use std::fmt;

#[derive(Clone, Debug, Eq, Hash, Logos, PartialEq, Ord, PartialOrd)]
#[logos(skip r"[ \t\n\r\f]+")]
#[logos(error = LexError)]
pub(super) enum Token<'sc> {
    #[token(":")]
    Colon,
    #[token("!")]
    Bang,
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
    #[token("->")]
    Arrow,

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

    #[token("var")]
    Var,
    #[token("let")]
    Let,
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

    #[regex(r"[A-Za-z_][A-Za-z_0-9]*", |lex| lex.slice())]
    Ident(&'sc str),
    #[regex(r"[+-]?[0-9]+\.[0-9]+([Ee][-+]?[0-9]+)?|[0-9]+[Ee][-+]?[0-9]+", |lex| lex.slice())]
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
    Token::Var,
    Token::Let,
    Token::Constraint,
    Token::Maximize,
    Token::Minimize,
    Token::Solve,
    Token::Satisfy,
];

impl<'sc> fmt::Display for Token<'sc> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Colon => write!(f, ":"),
            Token::Bang => write!(f, "!"),
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
            Token::Semi => write!(f, ";"),
            Token::Comma => write!(f, ","),
            Token::Star => write!(f, "*"),
            Token::BraceOpen => write!(f, "{{"),
            Token::BraceClose => write!(f, "}}"),
            Token::ParenOpen => write!(f, "("),
            Token::ParenClose => write!(f, ")"),
            Token::Arrow => write!(f, "->"),
            Token::Real => write!(f, "real"),
            Token::Int => write!(f, "int"),
            Token::Bool => write!(f, "bool"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::String => write!(f, "string"),
            Token::Fn => write!(f, "fn"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Let => write!(f, "let"),
            Token::Var => write!(f, "var"),
            Token::Constraint => write!(f, "constraint"),
            Token::Maximize => write!(f, "maximize"),
            Token::Minimize => write!(f, "minimize"),
            Token::Solve => write!(f, "solve"),
            Token::Satisfy => write!(f, "satisfy"),
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
pub(super) fn lex(src: &str) -> (Vec<(Token, Span)>, Vec<CompileError>) {
    Token::lexer(src)
        .spanned()
        .partition_map(|(r, span)| match r {
            Ok(v) => Either::Left((v, span)),
            Err(v) => Either::Right(CompileError::Lex { span, error: v }),
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

#[cfg(test)]
fn check(actual: &str, expect: expect_test::Expect) {
    expect.assert_eq(actual);
}

#[cfg(test)]
fn lex_one_success(src: &str) -> Token<'_> {
    // Tokenise src, assume success and that we produce a single token.
    let (toks, errs) = lex(src);
    assert!(errs.is_empty(), "Testing for success only.");
    assert_eq!(toks.len(), 1, "Testing for single token only.");
    toks[0].0.clone()
}

#[cfg(test)]
fn lex_one_error(src: &str) -> CompileError {
    // Tokenise src, assume a single error.
    let (_, errs) = lex(src);
    assert_eq!(errs.len(), 1, "Testing for single error only.");
    errs[0].clone()
}

#[test]
fn reals() {
    assert_eq!(lex_one_success("1.05"), Token::RealLiteral("1.05"));
    assert_eq!(lex_one_success("1.0"), Token::RealLiteral("1.0"));
    assert_eq!(lex_one_success("2.5e-4"), Token::RealLiteral("2.5e-4"));
    assert_eq!(lex_one_success("1.3E5"), Token::RealLiteral("1.3E5"));
    assert_eq!(lex_one_success("0.34"), Token::RealLiteral("0.34"));
    assert_eq!(lex_one_success("-0.34"), Token::RealLiteral("-0.34"));
    check(
        &format!("{:?}", lex_one_error(".34")),
        expect_test::expect![[r#"Lex { span: 0..1, error: InvalidToken }"#]],
    );
    check(
        &format!("{:?}", lex_one_error("12.")),
        expect_test::expect!["Lex { span: 2..3, error: InvalidToken }"],
    );
}

#[test]
fn ints() {
    assert_eq!(lex_one_success("1"), Token::IntLiteral("1"));
    assert_eq!(lex_one_success("0030"), Token::IntLiteral("0030"));
    assert_eq!(lex_one_success("0x333"), Token::IntLiteral("0x333"));
    assert_eq!(lex_one_success("0b1010"), Token::IntLiteral("0b1010"));
}

#[test]
fn bools() {
    assert_eq!(lex_one_success("true"), Token::True);
    assert_eq!(lex_one_success("false"), Token::False);
    assert_ne!(lex_one_success("false"), Token::True);
    assert_ne!(lex_one_success("true"), Token::False);
}

#[test]
fn strings() {
    assert_eq!(
        lex_one_success(r#""Hello, world!""#),
        Token::StringLiteral("Hello, world!".to_string())
    );
    assert_eq!(
        lex_one_success(
            r#"
            "first line \
            second line \
            third line"
            "#
        ),
        Token::StringLiteral("first line second line third line".to_string())
    );
    assert_eq!(
        lex_one_success("\"Hello, world!\n\""),
        Token::StringLiteral("Hello, world!\n".to_string())
    );
    assert_eq!(
        lex_one_success("\"Hello, world!\t\""),
        Token::StringLiteral("Hello, world!\t".to_string())
    );
    assert_eq!(
        lex_one_success(r#""Hello, \" world\"!""#),
        Token::StringLiteral("Hello, \" world\"!".to_string())
    );
    assert_eq!(
        lex_one_success("\"Hello, \\\\ world!\""),
        Token::StringLiteral("Hello, \\ world!".to_string())
    );
    assert_eq!(
        lex_one_success("\"x\\x41\\x2b\\x7ab\""),
        Token::StringLiteral("xA+zb".to_string())
    );
    assert_eq!(
        lex_one_success("\"aha\\x0a\\x0d\\x09\""),
        Token::StringLiteral("aha\n\r\t".to_string())
    );
}

#[test]
fn variables() {
    assert_eq!(lex_one_success("var"), Token::Var);
    assert_eq!(lex_one_success("let"), Token::Let);
}

#[test]
fn operators() {
    assert_eq!(lex_one_success("!"), Token::Bang);
    assert_eq!(lex_one_success("+"), Token::Plus);
    assert_eq!(lex_one_success("-"), Token::Minus);
    assert_eq!(lex_one_success("*"), Token::Star);
    assert_eq!(lex_one_success("/"), Token::Div);
    assert_eq!(lex_one_success("%"), Token::Mod);
    assert_eq!(lex_one_success(">"), Token::Gt);
    assert_eq!(lex_one_success("<"), Token::Lt);
    assert_eq!(lex_one_success("<="), Token::LtEq);
    assert_eq!(lex_one_success(">="), Token::GtEq);
    assert_eq!(lex_one_success("=="), Token::EqEq);
    assert_eq!(lex_one_success("!="), Token::NotEq);
}

#[test]
fn func() {
    assert_eq!(lex_one_success("fn"), Token::Fn);
}

#[test]
fn if_and_else() {
    assert_eq!(lex_one_success("if"), Token::If);
    assert_eq!(lex_one_success("else"), Token::Else);
}

#[test]
fn with_error() {
    let src = r#"
let low_val: int = 5.0;
constraint mid > low_val # 2;
constraint mid < low_val @ 2;
solve minimize mid;
"#;

    let (tokens, errors) = lex(src);

    // Check errors
    assert_eq!(errors.len(), 2);
    assert!(matches!(
        (&errors[0], &errors[1]),
        (
            CompileError::Lex {
                error: LexError::InvalidToken,
                ..
            },
            CompileError::Lex {
                error: LexError::InvalidToken,
                ..
            }
        )
    ));

    // Check tokens
    use Token::*;
    assert_eq!(tokens.len(), 23);
    assert!(matches!(tokens[0].0, Let));
    assert!(matches!(tokens[1].0, Ident("low_val")));
    assert!(matches!(tokens[2].0, Colon));
    assert!(matches!(tokens[3].0, Int));
    assert!(matches!(tokens[4].0, Eq));
    assert!(matches!(tokens[5].0, RealLiteral("5.0")));
    assert!(matches!(tokens[6].0, Semi));

    assert!(matches!(tokens[7].0, Constraint));
    assert!(matches!(tokens[8].0, Ident("mid")));
    assert!(matches!(tokens[9].0, Gt));
    assert!(matches!(tokens[10].0, Ident("low_val")));
    assert!(matches!(tokens[11].0, IntLiteral("2")));
    assert!(matches!(tokens[12].0, Semi));

    assert!(matches!(tokens[13].0, Constraint));
    assert!(matches!(tokens[14].0, Ident("mid")));
    assert!(matches!(tokens[15].0, Lt));
    assert!(matches!(tokens[16].0, Ident("low_val")));
    assert!(matches!(tokens[17].0, IntLiteral("2")));
    assert!(matches!(tokens[18].0, Semi));

    assert!(matches!(tokens[19].0, Solve));
    assert!(matches!(tokens[20].0, Minimize));
    assert!(matches!(tokens[21].0, Ident("mid")));
    assert!(matches!(tokens[22].0, Semi));
}
