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
