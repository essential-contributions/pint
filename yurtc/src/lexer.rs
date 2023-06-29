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
    #[regex(r"[0-9]+\.[0-9]+([Ee][-+]?[0-9]+)?|[0-9]+[Ee][-+]?[0-9]+", |lex| lex.slice())]
    RealNumber(&'sc str),
    #[regex(r"0x[0-9A-Fa-f]+|0b[0-1]+|[0-9]+", |lex| lex.slice())]
    Integer(&'sc str),
    #[regex(
        r#""([^"\\]|\\(x[0-9a-fA-F]{2}|n|t|"|\\|\n[\t ]*))*""#,
        process_string_literal
    )]
    StringLiteral(String),

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
            Token::Fn => write!(f, "Fn"),
            Token::Let => write!(f, "let"),
            Token::Var => write!(f, "var"),
            Token::Constraint => write!(f, "constraint"),
            Token::Maximize => write!(f, "maximize"),
            Token::Minimize => write!(f, "minimize"),
            Token::Solve => write!(f, "solve"),
            Token::Satisfy => write!(f, "satisfy"),
            Token::Ident(ident) => write!(f, "{ident}"),
            Token::RealNumber(ident) => write!(f, "{ident}"),
            Token::Integer(ident) => write!(f, "{ident}"),
            Token::StringLiteral(contents) => write!(f, "{}", contents),
            Token::Comment => write!(f, "comment"),
        }
    }
}

#[cfg(test)]
fn check(actual: &str, expect: expect_test::Expect) {
    expect.assert_eq(actual);
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

fn process_string_literal<'sc>(lex: &mut logos::Lexer<'sc, Token<'sc>>) -> String {
    let raw_string = lex.slice().to_string();
    let mut final_string = String::new();
    let mut chars = raw_string.chars().peekable();

    while let Some(c) = chars.next() {
        match c {
            '\\' => {
                if let Some(&next_char) = chars.peek() {
                    match next_char {
                        'n' => {
                            final_string.push('\n');
                            chars.next();
                        }
                        't' => {
                            final_string.push('\t');
                            chars.next();
                        }
                        '\\' => {
                            final_string.push('\\');
                            chars.next();
                        }
                        '"' => {
                            final_string.push('"');
                            chars.next();
                        }
                        '\n' => {
                            chars.next();
                            while let Some(&next_char) = chars.peek() {
                                if next_char.is_whitespace() {
                                    chars.next();
                                } else {
                                    break;
                                }
                            }
                        }
                        _ => final_string.push(c),
                    }
                }
            }
            '"' => {}
            _ => final_string.push(c),
        }
    }
    final_string
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
    assert_eq!(lex_one_success("1.05"), Token::RealNumber("1.05"));
    assert_eq!(lex_one_success("1.0"), Token::RealNumber("1.0"));
    assert_eq!(lex_one_success("2.5e-4"), Token::RealNumber("2.5e-4"));
    assert_eq!(lex_one_success("1.3E5"), Token::RealNumber("1.3E5"));
    assert_eq!(lex_one_success("0.34"), Token::RealNumber("0.34"));
    check(
        &format!("{:?}", lex_one_error("-0.34")),
        expect_test::expect![[r#"Lex { span: 0..1, error: InvalidToken }"#]],
    );
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
    assert_eq!(lex_one_success("1"), Token::Integer("1"));
    assert_eq!(lex_one_success("0030"), Token::Integer("0030"));
    assert_eq!(lex_one_success("0x333"), Token::Integer("0x333"));
    assert_eq!(lex_one_success("0b1010"), Token::Integer("0b1010"));
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
}

#[test]
fn variables() {
    assert_eq!(lex_one_success("var"), Token::Var);
    assert_eq!(lex_one_success("let"), Token::Let);
}

#[test]
fn func() {
    assert_eq!(lex_one_success("fn"), Token::Fn);
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
    assert!(matches!(tokens[5].0, RealNumber("5.0")));
    assert!(matches!(tokens[6].0, Semi));

    assert!(matches!(tokens[7].0, Constraint));
    assert!(matches!(tokens[8].0, Ident("mid")));
    assert!(matches!(tokens[9].0, Gt));
    assert!(matches!(tokens[10].0, Ident("low_val")));
    assert!(matches!(tokens[11].0, Integer("2")));
    assert!(matches!(tokens[12].0, Semi));

    assert!(matches!(tokens[13].0, Constraint));
    assert!(matches!(tokens[14].0, Ident("mid")));
    assert!(matches!(tokens[15].0, Lt));
    assert!(matches!(tokens[16].0, Ident("low_val")));
    assert!(matches!(tokens[17].0, Integer("2")));
    assert!(matches!(tokens[18].0, Semi));

    assert!(matches!(tokens[19].0, Solve));
    assert!(matches!(tokens[20].0, Minimize));
    assert!(matches!(tokens[21].0, Ident("mid")));
    assert!(matches!(tokens[22].0, Semi));
}
