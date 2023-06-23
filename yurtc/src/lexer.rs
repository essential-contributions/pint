use crate::error::{CompileError, LexError, Span};
use itertools::{Either, Itertools};
use logos::Logos;
use std::fmt;

#[derive(Clone, Debug, Eq, Hash, Logos, PartialEq)]
#[logos(skip r"[ \t\n\r\f]+")]
#[logos(error = LexError)]
pub(super) enum Token<'sc> {
    #[token(":")]
    Colon,
    #[token("=")]
    Eq,
    #[token(">")]
    Gt,
    #[token("<")]
    Lt,
    #[token(";")]
    Semi,
    #[token("*")]
    Star,

    #[token("real")]
    Real,

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
    #[regex(r"-?[0-9]+(\.[0-9]+)?", |lex| lex.slice())]
    Number(&'sc str),

    #[regex(r"//[^\n\r]*", logos::skip)]
    Comment,
}

impl<'sc> fmt::Display for Token<'sc> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Colon => write!(f, ":"),
            Token::Eq => write!(f, "="),
            Token::Gt => write!(f, ">"),
            Token::Lt => write!(f, "<"),
            Token::Semi => write!(f, ";"),
            Token::Star => write!(f, "*"),
            Token::Real => write!(f, "real"),
            Token::Let => write!(f, "let"),
            Token::Constraint => write!(f, "constraint"),
            Token::Maximize => write!(f, "maximize"),
            Token::Minimize => write!(f, "minimize"),
            Token::Solve => write!(f, "solve"),
            Token::Satisfy => write!(f, "satisfy"),
            Token::Ident(ident) => write!(f, "{ident}"),
            Token::Number(ident) => write!(f, "{ident}"),
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
    assert_eq!(lex_one_success("12"), Token::Number("12"));
    assert_eq!(lex_one_success("0012"), Token::Number("0012"));
    assert_eq!(lex_one_success("12.34"), Token::Number("12.34"));
    assert_eq!(lex_one_success("0.34"), Token::Number("0.34"));
    assert_eq!(lex_one_success("-0.34"), Token::Number("-0.34"));
    assert_eq!(
        format!("{:?}", lex_one_error(".34")),
        r#"Lex { span: 0..1, error: InvalidToken }"#
    );
    assert_eq!(
        format!("{:?}", lex_one_error("12.")),
        r#"Lex { span: 2..3, error: InvalidToken }"#
    );
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
    assert!(matches!(tokens[3].0, Ident("int")));
    assert!(matches!(tokens[4].0, Eq));
    assert!(matches!(tokens[5].0, Number("5.0")));
    assert!(matches!(tokens[6].0, Semi));

    assert!(matches!(tokens[7].0, Constraint));
    assert!(matches!(tokens[8].0, Ident("mid")));
    assert!(matches!(tokens[9].0, Gt));
    assert!(matches!(tokens[10].0, Ident("low_val")));
    assert!(matches!(tokens[11].0, Number("2")));
    assert!(matches!(tokens[12].0, Semi));

    assert!(matches!(tokens[13].0, Constraint));
    assert!(matches!(tokens[14].0, Ident("mid")));
    assert!(matches!(tokens[15].0, Lt));
    assert!(matches!(tokens[16].0, Ident("low_val")));
    assert!(matches!(tokens[17].0, Number("2")));
    assert!(matches!(tokens[18].0, Semi));

    assert!(matches!(tokens[19].0, Solve));
    assert!(matches!(tokens[20].0, Minimize));
    assert!(matches!(tokens[21].0, Ident("mid")));
    assert!(matches!(tokens[22].0, Semi));
}
