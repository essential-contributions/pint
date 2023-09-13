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
    assert_eq!(lex_one_success("::"), Token::DoubleColon);
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
    assert_eq!(lex_one_success("true"), Token::Literal("true"));
    assert_eq!(lex_one_success("false"), Token::Literal("false"));
}

#[test]
fn number_literals() {
    assert_eq!(lex_one_success("42"), Token::Literal("42"));
    assert_eq!(lex_one_success("3.14"), Token::Literal("3.14"));
    assert_eq!(lex_one_success("5.67E+3"), Token::Literal("5.67E+3"));
    assert_eq!(lex_one_success("1.23e-4"), Token::Literal("1.23e-4"));
    assert_eq!(lex_one_success("0x1A3F"), Token::Literal("0x1A3F"));
    assert_eq!(lex_one_success("0b1010"), Token::Literal("0b1010"));
}

#[test]
fn string_literals() {
    assert_eq!(lex_one_success(r#""Hello""#), Token::Literal(r#""Hello""#));
    assert_eq!(
        lex_one_success(r#""This is a \"quote\".""#),
        Token::Literal(r#""This is a \"quote\".""#)
    );
    assert_eq!(
        lex_one_success(r#""New\nLine""#),
        Token::Literal(r#""New\nLine""#)
    );
}

#[test]
fn solve_tokens() {
    assert_eq!(lex_one_success("solve"), Token::Solve);
    assert_eq!(lex_one_success("maximize"), Token::Directive("maximize"));
    assert_eq!(lex_one_success("minimize"), Token::Directive("minimize"));
    assert_eq!(lex_one_success("satisfy"), Token::Directive("satisfy"));
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
    assert_eq!(lex_one_success("&&"), Token::DoubleAmpersand);
    assert_eq!(lex_one_success("||"), Token::DoublePipe);
}

#[test]
fn constraint() {
    assert_eq!(lex_one_success("constraint"), Token::Constraint);
}
