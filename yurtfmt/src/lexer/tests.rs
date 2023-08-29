#[cfg(test)]
use crate::lexer::*;

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
    assert_eq!(lex_one_success(";"), Token::Semi);
    assert_eq!(lex_one_success("="), Token::Eq);
    assert_eq!(lex_one_success("let"), Token::Let);
}

#[test]
fn primitives() {
    assert_eq!(lex_one_success("real"), Token::Primitive("real"));
    assert_eq!(lex_one_success("int"), Token::Primitive("int"));
    assert_eq!(lex_one_success("string"), Token::Primitive("string"));
    assert_eq!(lex_one_success("bool"), Token::Primitive("bool"));
}

#[test]
fn idents() {
    assert_eq!(lex_one_success("identifier"), Token::Ident("identifier"));
    assert_eq!(lex_one_success("_privateVar"), Token::Ident("_privateVar"));
    assert_eq!(lex_one_success("var123"), Token::Ident("var123"));
}

#[test]
fn bool_literals() {
    assert_eq!(lex_one_success("true"), Token::BoolLiteral("true"));
    assert_eq!(lex_one_success("false"), Token::BoolLiteral("false"));
}

#[test]
fn number_literals() {
    assert_eq!(lex_one_success("42"), Token::NumberLiteral("42"));
    assert_eq!(lex_one_success("3.14"), Token::NumberLiteral("3.14"));
    assert_eq!(lex_one_success("5.67E+3"), Token::NumberLiteral("5.67E+3"));
    assert_eq!(lex_one_success("1.23e-4"), Token::NumberLiteral("1.23e-4"));
    assert_eq!(lex_one_success("0x1A3F"), Token::NumberLiteral("0x1A3F"));
    assert_eq!(lex_one_success("0b1010"), Token::NumberLiteral("0b1010"));
}

#[test]
fn string_literals() {
    assert_eq!(
        lex_one_success(r#""Hello""#),
        Token::StringLiteral(r#""Hello""#.to_string())
    );
    assert_eq!(
        lex_one_success(r#""This is a \"quote\".""#),
        Token::StringLiteral(r#""This is a \"quote\".""#.to_string())
    );
    assert_eq!(
        lex_one_success(r#""New\nLine""#),
        Token::StringLiteral(r#""New\nLine""#.to_string())
    );
}
