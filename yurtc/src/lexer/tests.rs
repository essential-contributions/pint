use crate::error::{CompileError, LexError};
use crate::lexer::*;

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

#[test]
fn control_tokens() {
    assert_eq!(lex_one_success(":"), Token::Colon);
    assert_eq!(lex_one_success("::"), Token::DoubleColon);
    assert_eq!(lex_one_success(";"), Token::Semi);
    assert_eq!(lex_one_success(","), Token::Comma);
    assert_eq!(lex_one_success("{"), Token::BraceOpen);
    assert_eq!(lex_one_success("}"), Token::BraceClose);
    assert_eq!(lex_one_success("("), Token::ParenOpen);
    assert_eq!(lex_one_success(")"), Token::ParenClose);
    assert_eq!(lex_one_success("->"), Token::Arrow);
    assert_eq!(lex_one_success("=>"), Token::HeavyArrow);
    assert_eq!(lex_one_success("."), Token::Dot);
}

#[test]
fn reals() {
    assert_eq!(lex_one_success("1.05"), Token::RealLiteral("1.05"));
    assert_eq!(lex_one_success("1.0"), Token::RealLiteral("1.0"));
    assert_eq!(lex_one_success("2.5e-4"), Token::RealLiteral("2.5e-4"));
    assert_eq!(lex_one_success("1.3E5"), Token::RealLiteral("1.3E5"));
    assert_eq!(lex_one_success("0.34"), Token::RealLiteral("0.34"));
    check(
        &format!("{:?}", lex(".34")),
        expect_test::expect![[r#"([(Dot, 0..1), (IntLiteral("34"), 1..3)], [])"#]],
    );
    check(
        &format!("{:?}", lex("12.")),
        expect_test::expect![[r#"([(IntLiteral("12"), 0..2), (Dot, 2..3)], [])"#]],
    );
}

#[test]
fn ints() {
    assert_eq!(lex_one_success("1"), Token::IntLiteral("1"));
    assert_eq!(lex_one_success("0030"), Token::IntLiteral("0030"));
    assert_eq!(lex_one_success("0x333"), Token::IntLiteral("0x333"));
    assert_eq!(lex_one_success("0b1010"), Token::IntLiteral("0b1010"));

    assert_eq!(
        lex_one_success("1111111111222222222233333333334444444444"),
        Token::IntLiteral("1111111111222222222233333333334444444444")
    );
    assert_eq!(
        lex_one_success("0xaaaaaaaaaabbbbbbbbbbccccccccccdddddddddd"),
        Token::IntLiteral("0xaaaaaaaaaabbbbbbbbbbccccccccccdddddddddd")
    );
    assert_eq!(
        lex_one_success(
            "0b11111111110000000000111111111100000000001111111111000000000011111111110000000000"
        ),
        Token::IntLiteral(
            "0b11111111110000000000111111111100000000001111111111000000000011111111110000000000"
        )
    );
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
    assert_eq!(lex_one_success("let"), Token::Let);
}

#[test]
fn operators() {
    assert_eq!(lex_one_success("!"), Token::Bang);
    assert_eq!(lex_one_success("|"), Token::Pipe);
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
    assert_eq!(lex_one_success("&&"), Token::DoubleAmpersand);
    assert_eq!(lex_one_success("||"), Token::DoublePipe);
}

#[test]
fn func() {
    assert_eq!(lex_one_success("fn"), Token::Fn);
}

#[test]
fn if_else_cond() {
    assert_eq!(lex_one_success("if"), Token::If);
    assert_eq!(lex_one_success("else"), Token::Else);
    assert_eq!(lex_one_success("cond"), Token::Cond);
}

#[test]
fn r#use() {
    assert_eq!(lex_one_success("use"), Token::Use);
}

#[test]
fn r#as() {
    assert_eq!(lex_one_success("as"), Token::As);
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
