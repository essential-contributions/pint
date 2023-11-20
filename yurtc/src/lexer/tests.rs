use crate::lexer::*;
use std::{path::Path, rc::Rc};

#[cfg(test)]
fn check(actual: &str, expect: expect_test::Expect) {
    expect.assert_eq(actual);
}

#[cfg(test)]
fn lex_one_success(src: &str) -> Token<'_> {
    // Tokenise src, assume success and that we produce a single token.
    let (toks, errs) = lex(src, Rc::from(Path::new("test")));
    assert!(errs.is_empty(), "Testing for success only.");
    assert_eq!(toks.len(), 1, "Testing for single token only.");
    toks[0].0.clone()
}

#[cfg(test)]
pub(super) fn lex(
    src: &str,
    filepath: Rc<std::path::Path>,
) -> (Vec<(Token, Span)>, Vec<ParseError>) {
    use itertools::Itertools;
    Token::lexer(src).spanned().partition_map(|(r, span)| {
        let span = Span::new(Rc::clone(&filepath), span);
        match r {
            Ok(v) => itertools::Either::Left((v, span)),
            Err(_) => itertools::Either::Right(ParseError::Lex { span }),
        }
    })
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
    assert_eq!(lex_one_success("["), Token::BracketOpen);
    assert_eq!(lex_one_success("]"), Token::BracketClose);
    assert_eq!(lex_one_success("->"), Token::Arrow);
    assert_eq!(lex_one_success("=>"), Token::HeavyArrow);
    assert_eq!(lex_one_success("."), Token::Dot);
    assert_eq!(lex_one_success(".."), Token::TwoDots);
}

#[test]
fn reals() {
    assert_eq!(lex_one_success("1.05"), Token::RealLiteral("1.05"));
    assert_eq!(lex_one_success("1.0"), Token::RealLiteral("1.0"));
    assert_eq!(lex_one_success("2.5e-4"), Token::RealLiteral("2.5e-4"));
    assert_eq!(lex_one_success("1.3E5"), Token::RealLiteral("1.3E5"));
    assert_eq!(lex_one_success("0.34"), Token::RealLiteral("0.34"));
    check(
        &format!("{:?}", lex(".34", Rc::from(Path::new("test")))),
        expect_test::expect![[r#"([(Dot, "test":0..1), (IntLiteral("34"), "test":1..3)], [])"#]],
    );
    check(
        &format!("{:?}", lex("12.", Rc::from(Path::new("test")))),
        expect_test::expect![[r#"([(IntLiteral("12"), "test":0..2), (Dot, "test":2..3)], [])"#]],
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
    assert_eq!(lex_one_success("state"), Token::State);
}

#[test]
fn r#types() {
    assert_eq!(lex_one_success("enum"), Token::Enum);
    assert_eq!(lex_one_success("type"), Token::Type);
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
fn r#in() {
    assert_eq!(lex_one_success("in"), Token::In);
}

#[test]
fn blockchain_items() {
    assert_eq!(lex_one_success("interface"), Token::Interface);
    assert_eq!(lex_one_success("contract"), Token::Contract);
    assert_eq!(lex_one_success("implements"), Token::Implements);
    assert_eq!(lex_one_success("extern"), Token::Extern);
}

#[test]
fn with_error() {
    let src = r#"
let low_val: int = 5.0;
constraint mid > low_val # 2;
constraint mid < low_val @ 2;
solve minimize mid;
"#;

    let (tokens, errors) = lex(src, Rc::from(Path::new("test")));

    // Check errors
    assert_eq!(errors.len(), 2);
    assert!(matches!(
        (&errors[0], &errors[1]),
        (ParseError::Lex { .. }, ParseError::Lex { .. })
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

#[test]
fn macros_success() {
    use Token::*;

    let path = Rc::from(Path::new("test"));

    // Simple success cases.
    let mut toks = Lexer::new("macro @name()", &Rc::clone(&path));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Macro, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroName("@name"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenClose, _)));
    assert!(toks.next().is_none());

    let mut toks = Lexer::new("macro @name_x11($a)", &Rc::clone(&path));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Macro, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroName("@name_x11"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroParam("$a"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenClose, _)));
    assert!(toks.next().is_none());

    let mut toks = Lexer::new("macro @name($Z, $9)", &Rc::clone(&path));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Macro, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroName("@name"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroParam("$Z"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Comma, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroParam("$9"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenClose, _)));
    assert!(toks.next().is_none());

    let mut toks = Lexer::new("macro @name($Z, $9,)", &Rc::clone(&path));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Macro, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroName("@name"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroParam("$Z"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Comma, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroParam("$9"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Comma, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenClose, _)));
    assert!(toks.next().is_none());

    let mut toks = Lexer::new("macro @name() {} constraint", &Rc::clone(&path));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Macro, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroName("@name"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenClose, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroBody(_), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Constraint, _)));
    assert!(toks.next().is_none());

    let mut toks = Lexer::new(r#"macro @name() { let it "be" 88 ; }"#, &Rc::clone(&path));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Macro, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroName("@name"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenClose, _)));
    let body = toks.next().unwrap().unwrap();
    assert!(toks.next().is_none());

    assert!(matches!(body, (_, MacroBody(_), _)));
    if let MacroBody(body_toks) = body.1 {
        assert_eq!(body_toks.len(), 7);
        assert!(matches!(body_toks[0], BraceOpen));
        assert!(matches!(body_toks[1], Let));
        assert!(matches!(body_toks[2], Ident("it")));
        assert!(matches!(body_toks[3], StringLiteral(_)));
        assert!(matches!(body_toks[4], IntLiteral("88")));
        assert!(matches!(body_toks[5], Semi));
        assert!(matches!(body_toks[6], BraceClose));
    } else {
        unreachable!()
    }

    // Nested braces.
    let mut toks = Lexer::new("macro @name() { a { b}{{}c}{ d }} let", &Rc::clone(&path));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Macro, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroName("@name"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenClose, _)));
    let body = toks.next().unwrap().unwrap();
    assert!(matches!(body, (_, MacroBody(_), _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Let, _)));
    assert!(toks.next().is_none());

    if let MacroBody(body_toks) = body.1 {
        assert_eq!(body_toks.len(), 14);
        assert!(matches!(body_toks[0], BraceOpen));
        assert!(matches!(body_toks[1], Ident("a")));
        assert!(matches!(body_toks[2], BraceOpen));
        assert!(matches!(body_toks[3], Ident("b")));
        assert!(matches!(body_toks[4], BraceClose));
        assert!(matches!(body_toks[5], BraceOpen));
        assert!(matches!(body_toks[6], BraceOpen));
        assert!(matches!(body_toks[7], BraceClose));
        assert!(matches!(body_toks[8], Ident("c")));
        assert!(matches!(body_toks[9], BraceClose));
        assert!(matches!(body_toks[10], BraceOpen));
        assert!(matches!(body_toks[11], Ident("d")));
        assert!(matches!(body_toks[12], BraceClose));
        assert!(matches!(body_toks[13], BraceClose));
    } else {
        unreachable!()
    }
}

#[test]
fn macros_badly_formed() {
    use Token::*;

    let path = Rc::from(Path::new("test"));

    // Macro name has no `@`, should not parse the body.
    let mut toks = Lexer::new("macro bad() { 11 }", &Rc::clone(&path));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Macro, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, Ident("bad"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenClose, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, BraceOpen, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, IntLiteral("11"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, BraceClose, _)));
    assert!(toks.next().is_none());

    // Macro params have no `$`, should not parse the body.
    let mut toks = Lexer::new("macro @name(bad, param) { 22 }", &Rc::clone(&path));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Macro, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroName("@name"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, Ident("bad"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Comma, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, Ident("param"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenClose, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, BraceOpen, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, IntLiteral("22"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, BraceClose, _)));
    assert!(toks.next().is_none());

    let mut toks = Lexer::new("macro @name($good, bad) { 33 }", &Rc::clone(&path));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Macro, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroName("@name"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroParam("$good"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Comma, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, Ident("bad"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenClose, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, BraceOpen, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, IntLiteral("33"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, BraceClose, _)));
    assert!(toks.next().is_none());

    // Badly nested braces, should backtrack.
    let mut toks = Lexer::new("macro @name() { { } let", &Rc::clone(&path));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Macro, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroName("@name"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenClose, _)));
    // Not a macro body...
    assert!(matches!(toks.next().unwrap().unwrap(), (_, BraceOpen, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, BraceOpen, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, BraceClose, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Let, _)));
    assert!(toks.next().is_none());
}

#[test]
fn macros_call_success() {
    use Token::*;

    let path = Rc::from(Path::new("test"));

    let mut toks = Lexer::new("@name()", &Rc::clone(&path));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroName("@name"), _)
    ));
    let args = toks.next().unwrap().unwrap();
    assert!(matches!(args, (_, MacroCallArgs(_), _)));
    assert!(toks.next().is_none());
    if let MacroCallArgs(args) = args.1 {
        assert!(args.is_empty());
    } else {
        unreachable!()
    }

    let mut toks = Lexer::new("11 + @name()", &Rc::clone(&path));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, IntLiteral("11"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Plus, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroName("@name"), _)
    ));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroCallArgs(_), _)
    ));
    assert!(toks.next().is_none());

    let mut toks = Lexer::new("@name(int)", &Rc::clone(&path));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroName("@name"), _)
    ));
    let args = toks.next().unwrap().unwrap();
    assert!(matches!(args, (_, MacroCallArgs(_), _)));
    assert!(toks.next().is_none());
    if let MacroCallArgs(args) = args.1 {
        assert_eq!(args.len(), 1);
        assert_eq!(args[0].len(), 1);
        assert!(matches!(args[0][0], Int));
    } else {
        unreachable!()
    }

    let mut toks = Lexer::new("@name(int; foo) || true", &Rc::clone(&path));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroName("@name"), _)
    ));
    let args = toks.next().unwrap().unwrap();
    assert!(matches!(args, (_, MacroCallArgs(_), _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, DoublePipe, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, True, _)));
    assert!(toks.next().is_none());
    if let MacroCallArgs(args) = args.1 {
        assert_eq!(args.len(), 2);
        assert_eq!(args[0].len(), 1);
        assert!(matches!(args[0][0], Int));
        assert_eq!(args[1].len(), 1);
        assert!(matches!(args[1][0], Ident(_)));
    } else {
        unreachable!()
    }

    let mut toks = Lexer::new("@name(1 + 2; [1, 2];)", &Rc::clone(&path));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroName("@name"), _)
    ));
    let args = toks.next().unwrap().unwrap();
    assert!(matches!(args, (_, MacroCallArgs(_), _)));
    assert!(toks.next().is_none());
    if let MacroCallArgs(args) = args.1 {
        assert_eq!(args.len(), 2);
        assert_eq!(args[0].len(), 3);
        assert!(matches!(args[0][0], IntLiteral("1")));
        assert!(matches!(args[0][1], Plus));
        assert!(matches!(args[0][2], IntLiteral("2")));
        assert_eq!(args[1].len(), 5);
        assert!(matches!(args[1][0], BracketOpen));
        assert!(matches!(args[1][1], IntLiteral("1")));
        assert!(matches!(args[1][2], Comma));
        assert!(matches!(args[1][3], IntLiteral("2")));
        assert!(matches!(args[1][4], BracketClose));
    } else {
        unreachable!()
    }

    let mut toks = Lexer::new("@name(let i: int = 0;)", &Rc::clone(&path));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroName("@name"), _)
    ));
    let args = toks.next().unwrap().unwrap();
    assert!(matches!(args, (_, MacroCallArgs(_), _)));
    assert!(toks.next().is_none());
    if let MacroCallArgs(args) = args.1 {
        assert_eq!(args.len(), 1);
        assert_eq!(args[0].len(), 6);
        assert!(matches!(args[0][0], Let));
        assert!(matches!(args[0][1], Ident(_)));
        assert!(matches!(args[0][2], Colon));
        assert!(matches!(args[0][3], Int));
        assert!(matches!(args[0][4], Eq));
        assert!(matches!(args[0][5], IntLiteral("0")));
    } else {
        unreachable!()
    }

    let mut toks = Lexer::new("@name(1,2,3,4,5,6,7,8)", &Rc::clone(&path));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroName("@name"), _)
    ));
    let args = toks.next().unwrap().unwrap();
    assert!(matches!(args, (_, MacroCallArgs(_), _)));
    assert!(toks.next().is_none());
    if let MacroCallArgs(args) = args.1 {
        assert_eq!(args.len(), 1);
        assert_eq!(args[0].len(), 15);
        assert!(matches!(args[0][0], IntLiteral("1")));
        assert!(matches!(args[0][1], Comma));
        assert!(matches!(args[0][2], IntLiteral("2")));
        assert!(matches!(args[0][3], Comma));
        // etc.
        assert!(matches!(args[0][11], Comma));
        assert!(matches!(args[0][12], IntLiteral("7")));
        assert!(matches!(args[0][13], Comma));
        assert!(matches!(args[0][14], IntLiteral("8")));
    } else {
        unreachable!()
    }
}

#[test]
fn macros_call_badly_formed() {
    use Token::*;

    let path = Rc::from(Path::new("test"));

    // Macro name has no `@`.
    let mut toks = Lexer::new("1 + name() + 2", &Rc::clone(&path));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, IntLiteral("1"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Plus, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Ident(_), _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenClose, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Plus, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, IntLiteral("2"), _)
    ));
    assert!(toks.next().is_none());

    // No closing paren, should backtrack.
    let mut toks = Lexer::new("@name(one; two", &Rc::clone(&path));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroName("@name"), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Ident(_), _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Semi, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Ident(_), _)));
    assert!(toks.next().is_none());
}
