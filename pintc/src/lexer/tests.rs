use crate::lexer::*;
use std::{path::Path, rc::Rc};

#[cfg(test)]
fn check(actual: &str, expect: expect_test::Expect) {
    expect.assert_eq(actual);
}

#[cfg(test)]
fn lex_one_success(src: &str) -> Token {
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
    assert_eq!(
        lex_one_success("1.05"),
        Token::RealLiteral("1.05".to_owned())
    );
    assert_eq!(lex_one_success("1.0"), Token::RealLiteral("1.0".to_owned()));
    assert_eq!(
        lex_one_success("2.5e-4"),
        Token::RealLiteral("2.5e-4".to_owned())
    );
    assert_eq!(
        lex_one_success("1.3E5"),
        Token::RealLiteral("1.3E5".to_owned())
    );
    assert_eq!(
        lex_one_success("0.34"),
        Token::RealLiteral("0.34".to_owned())
    );
    assert_eq!(
        lex_one_success("0.000_034"),
        Token::RealLiteral("0.000_034".to_owned())
    );
    assert_eq!(
        lex_one_success("1.23_e4"),
        Token::RealLiteral("1.23_e4".to_owned())
    );
    assert_eq!(
        lex_one_success("1.23e4_"),
        Token::RealLiteral("1.23e4_".to_owned())
    );
    assert_eq!(
        lex_one_success("1_000.34"),
        Token::RealLiteral("1_000.34".to_owned())
    );
    assert_eq!(
        lex_one_success("1.23e1_000"),
        Token::RealLiteral("1.23e1_000".to_owned())
    );
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
    assert_eq!(lex_one_success("1"), Token::IntLiteral("1".to_owned()));
    assert_eq!(
        lex_one_success("0030"),
        Token::IntLiteral("0030".to_owned())
    );
    assert_eq!(
        lex_one_success("0x333"),
        Token::IntLiteral("0x333".to_owned())
    );
    assert_eq!(
        lex_one_success("0b1010"),
        Token::IntLiteral("0b1010".to_owned())
    );
    assert_eq!(
        lex_one_success("1111111111222222222233333333334444444444"),
        Token::IntLiteral("1111111111222222222233333333334444444444".to_owned())
    );
    assert_eq!(
        lex_one_success("0xaaaaaaaaaabbbbbbbbbbccccccccccdddddddddd"),
        Token::IntLiteral("0xaaaaaaaaaabbbbbbbbbbccccccccccdddddddddd".to_owned())
    );
    assert_eq!(
        lex_one_success(
            "0b11111111110000000000111111111100000000001111111111000000000011111111110000000000"
        ),
        Token::IntLiteral(
            "0b11111111110000000000111111111100000000001111111111000000000011111111110000000000"
                .to_owned()
        )
    );
    assert_eq!(
        lex_one_success("1_000"),
        Token::IntLiteral("1_000".to_owned())
    );
    assert_eq!(
        lex_one_success("1_000_000"),
        Token::IntLiteral("1_000_000".to_owned())
    );
    assert_eq!(
        lex_one_success("1_2_3_4_5"),
        Token::IntLiteral("1_2_3_4_5".to_owned())
    );
    assert_eq!(
        lex_one_success("0x1_2_3_4_5"),
        Token::IntLiteral("0x1_2_3_4_5".to_owned())
    );
    assert_eq!(
        lex_one_success("0b1_0_1_0"),
        Token::IntLiteral("0b1_0_1_0".to_owned())
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
    assert_eq!(lex_one_success("var"), Token::Var);
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
fn r#pub() {
    assert_eq!(lex_one_success("pub"), Token::Pub);
}

#[test]
fn self_tok() {
    assert_eq!(lex_one_success("self"), Token::SelfTok);
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
    assert_eq!(lex_one_success("state"), Token::State);
    assert_eq!(lex_one_success("intent"), Token::Intent);
    assert_eq!(lex_one_success("storage"), Token::Storage);
    assert_eq!(lex_one_success("interface"), Token::Interface);
}

#[test]
fn generators() {
    assert_eq!(lex_one_success("forall"), Token::ForAll);
    assert_eq!(lex_one_success("exists"), Token::Exists);
    assert_eq!(lex_one_success("where"), Token::Where);
}

#[test]
fn with_error() {
    let src = r#"
var low_val: int = 5.0;
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
    assert!(matches!(tokens[0].0, Var));
    assert_eq!(tokens[1].0, Ident(("low_val".to_owned(), false)));
    assert!(matches!(tokens[2].0, Colon));
    assert!(matches!(tokens[3].0, Int));
    assert!(matches!(tokens[4].0, Eq));
    assert_eq!(tokens[5].0, RealLiteral("5.0".to_owned()));
    assert!(matches!(tokens[6].0, Semi));

    assert!(matches!(tokens[7].0, Constraint));
    assert_eq!(tokens[8].0, Ident(("mid".to_owned(), false)));
    assert!(matches!(tokens[9].0, Gt));
    assert_eq!(tokens[10].0, Ident(("low_val".to_owned(), false)));
    assert_eq!(tokens[11].0, IntLiteral("2".to_owned()));
    assert!(matches!(tokens[12].0, Semi));

    assert!(matches!(tokens[13].0, Constraint));
    assert_eq!(tokens[14].0, Ident(("mid".to_owned(), false)));
    assert!(matches!(tokens[15].0, Lt));
    assert_eq!(tokens[16].0, Ident(("low_val".to_owned(), false)));
    assert_eq!(tokens[17].0, IntLiteral("2".to_owned()));
    assert!(matches!(tokens[18].0, Semi));

    assert!(matches!(tokens[19].0, Solve));
    assert!(matches!(tokens[20].0, Minimize));
    assert_eq!(tokens[21].0, Ident(("mid".to_owned(), false)));
    assert!(matches!(tokens[22].0, Semi));
}

#[test]
fn macros_success() {
    use Token::*;

    let path = Rc::from(Path::new("test"));

    // Simple success cases.
    let mut toks = Lexer::new("macro @name()", &Rc::clone(&path), &[]);
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Macro, _)));
    assert_eq!(
        toks.next().unwrap().unwrap().1,
        MacroName("@name".to_owned()),
    );
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenClose, _)));
    assert!(toks.next().is_none());

    let mut toks = Lexer::new("macro @name_x11($a)", &Rc::clone(&path), &[]);
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Macro, _)));
    assert_eq!(
        toks.next().unwrap().unwrap().1,
        MacroName("@name_x11".to_owned()),
    );
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert_eq!(toks.next().unwrap().unwrap().1, MacroParam("$a".to_owned()),);
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenClose, _)));
    assert!(toks.next().is_none());

    let mut toks = Lexer::new("macro @name($Z, $9)", &Rc::clone(&path), &[]);
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Macro, _)));
    assert_eq!(
        toks.next().unwrap().unwrap().1,
        MacroName("@name".to_owned()),
    );
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert_eq!(toks.next().unwrap().unwrap().1, MacroParam("$Z".to_owned()),);
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Comma, _)));
    assert_eq!(toks.next().unwrap().unwrap().1, MacroParam("$9".to_owned()),);
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenClose, _)));
    assert!(toks.next().is_none());

    let mut toks = Lexer::new("macro @name($Z, $9,)", &Rc::clone(&path), &[]);
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Macro, _)));
    assert_eq!(
        toks.next().unwrap().unwrap().1,
        MacroName("@name".to_owned()),
    );
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert_eq!(toks.next().unwrap().unwrap().1, MacroParam("$Z".to_owned()),);
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Comma, _)));
    assert_eq!(toks.next().unwrap().unwrap().1, MacroParam("$9".to_owned()),);
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Comma, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenClose, _)));
    assert!(toks.next().is_none());

    let mut toks = Lexer::new("macro @name() {} constraint", &Rc::clone(&path), &[]);
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Macro, _)));
    assert_eq!(
        toks.next().unwrap().unwrap().1,
        MacroName("@name".to_owned()),
    );
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenClose, _)));
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroBody(_), _)
    ));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Constraint, _)));
    assert!(toks.next().is_none());

    let mut toks = Lexer::new(
        r#"macro @name() { var it "be" 88 ; }"#,
        &Rc::clone(&path),
        &[],
    );
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Macro, _)));
    assert_eq!(
        toks.next().unwrap().unwrap().1,
        MacroName("@name".to_owned()),
    );
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenClose, _)));
    let body = toks.next().unwrap().unwrap();
    assert!(toks.next().is_none());

    assert!(matches!(body, (_, MacroBody(_), _)));
    if let MacroBody(body_toks) = body.1 {
        assert_eq!(body_toks.len(), 7);
        assert!(matches!(body_toks[0], (_, BraceOpen, _)));
        assert!(matches!(body_toks[1], (_, Var, _)));
        assert_eq!(body_toks[2].1, Ident(("it".to_owned(), false)));
        assert!(matches!(body_toks[3], (_, StringLiteral(_), _)));
        assert_eq!(body_toks[4].1, IntLiteral("88".to_owned()));
        assert!(matches!(body_toks[5], (_, Semi, _)));
        assert!(matches!(body_toks[6], (_, BraceClose, _)));
    } else {
        unreachable!()
    }

    // Nested braces.
    let mut toks = Lexer::new(
        "macro @name() { a { b}{{}c}{ d }} var",
        &Rc::clone(&path),
        &[],
    );
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Macro, _)));
    assert_eq!(
        toks.next().unwrap().unwrap().1,
        MacroName("@name".to_owned()),
    );
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenClose, _)));
    let body = toks.next().unwrap().unwrap();
    assert!(matches!(body, (_, MacroBody(_), _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Var, _)));
    assert!(toks.next().is_none());

    if let MacroBody(body_toks) = body.1 {
        assert_eq!(body_toks.len(), 14);
        assert!(matches!(body_toks[0], (_, BraceOpen, _)));
        assert_eq!(body_toks[1].1, Ident(("a".to_owned(), false)));
        assert!(matches!(body_toks[2], (_, BraceOpen, _)));
        assert_eq!(body_toks[3].1, Ident(("b".to_owned(), false)));
        assert!(matches!(body_toks[4], (_, BraceClose, _)));
        assert!(matches!(body_toks[5], (_, BraceOpen, _)));
        assert!(matches!(body_toks[6], (_, BraceOpen, _)));
        assert!(matches!(body_toks[7], (_, BraceClose, _)));
        assert_eq!(body_toks[8].1, Ident(("c".to_owned(), false)));
        assert!(matches!(body_toks[9], (_, BraceClose, _)));
        assert!(matches!(body_toks[10], (_, BraceOpen, _)));
        assert_eq!(body_toks[11].1, Ident(("d".to_owned(), false)));
        assert!(matches!(body_toks[12], (_, BraceClose, _)));
        assert!(matches!(body_toks[13], (_, BraceClose, _)));
    } else {
        unreachable!()
    }
}

#[test]
fn macros_badly_formed() {
    use Token::*;

    let path = Rc::from(Path::new("test"));

    // Macro name has no `@`, should not parse the body.
    let mut toks = Lexer::new("macro bad() { 11 }", &Rc::clone(&path), &[]);
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Macro, _)));
    assert_eq!(
        toks.next().unwrap().unwrap().1,
        Ident(("bad".to_owned(), false)),
    );
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenClose, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, BraceOpen, _)));
    assert_eq!(toks.next().unwrap().unwrap().1, IntLiteral("11".to_owned()),);
    assert!(matches!(toks.next().unwrap().unwrap(), (_, BraceClose, _)));
    assert!(toks.next().is_none());

    // Macro params have no `$`, should not parse the body.
    let mut toks = Lexer::new("macro @name(bad, param) { 22 }", &Rc::clone(&path), &[]);
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Macro, _)));
    assert_eq!(
        toks.next().unwrap().unwrap().1,
        MacroName("@name".to_owned()),
    );
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert_eq!(
        toks.next().unwrap().unwrap().1,
        Ident(("bad".to_owned(), false)),
    );
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Comma, _)));
    assert_eq!(
        toks.next().unwrap().unwrap().1,
        Ident(("param".to_owned(), false)),
    );
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenClose, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, BraceOpen, _)));
    assert_eq!(toks.next().unwrap().unwrap().1, IntLiteral("22".to_owned()),);
    assert!(matches!(toks.next().unwrap().unwrap(), (_, BraceClose, _)));
    assert!(toks.next().is_none());

    let mut toks = Lexer::new("macro @name($good, bad) { 33 }", &Rc::clone(&path), &[]);
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Macro, _)));
    assert_eq!(
        toks.next().unwrap().unwrap().1,
        MacroName("@name".to_owned()),
    );
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert_eq!(
        toks.next().unwrap().unwrap().1,
        MacroParam("$good".to_owned()),
    );
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Comma, _)));
    assert_eq!(
        toks.next().unwrap().unwrap().1,
        Ident(("bad".to_owned(), false)),
    );
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenClose, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, BraceOpen, _)));
    assert_eq!(toks.next().unwrap().unwrap().1, IntLiteral("33".to_owned()),);
    assert!(matches!(toks.next().unwrap().unwrap(), (_, BraceClose, _)));
    assert!(toks.next().is_none());

    // Badly nested braces, should backtrack.
    let mut toks = Lexer::new("macro @name() { { } var", &Rc::clone(&path), &[]);
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Macro, _)));
    assert_eq!(
        toks.next().unwrap().unwrap().1,
        MacroName("@name".to_owned()),
    );
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenClose, _)));
    // Not a macro body...
    assert!(matches!(toks.next().unwrap().unwrap(), (_, BraceOpen, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, BraceOpen, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, BraceClose, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Var, _)));
    assert!(toks.next().is_none());
}

#[test]
fn macros_call_success() {
    use Token::*;

    let path = Rc::from(Path::new("test"));

    let mut toks = Lexer::new("@name()", &Rc::clone(&path), &[]);
    assert_eq!(
        toks.next().unwrap().unwrap().1,
        MacroName("@name".to_owned()),
    );
    let args = toks.next().unwrap().unwrap();
    assert!(matches!(args, (_, MacroCallArgs(_), _)));
    assert!(toks.next().is_none());
    if let MacroCallArgs(args) = args.1 {
        assert!(args.is_empty());
    } else {
        unreachable!()
    }

    let mut toks = Lexer::new("11 + @name()", &Rc::clone(&path), &[]);
    assert_eq!(toks.next().unwrap().unwrap().1, IntLiteral("11".to_owned()),);
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Plus, _)));
    assert_eq!(
        toks.next().unwrap().unwrap().1,
        MacroName("@name".to_owned()),
    );
    assert!(matches!(
        toks.next().unwrap().unwrap(),
        (_, MacroCallArgs(_), _)
    ));
    assert!(toks.next().is_none());

    let mut toks = Lexer::new("@name(int)", &Rc::clone(&path), &[]);
    assert_eq!(
        toks.next().unwrap().unwrap().1,
        MacroName("@name".to_owned()),
    );
    let args = toks.next().unwrap().unwrap();
    assert!(matches!(args, (_, MacroCallArgs(_), _)));
    assert!(toks.next().is_none());
    if let MacroCallArgs(args) = args.1 {
        assert_eq!(args.len(), 1);
        assert_eq!(args[0].len(), 1);
        assert!(matches!(args[0][0], (_, Int, _)));
    } else {
        unreachable!()
    }

    let mut toks = Lexer::new("@name(int; foo) || true", &Rc::clone(&path), &[]);
    assert_eq!(
        toks.next().unwrap().unwrap().1,
        MacroName("@name".to_owned()),
    );
    let args = toks.next().unwrap().unwrap();
    assert!(matches!(args, (_, MacroCallArgs(_), _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, DoublePipe, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, True, _)));
    assert!(toks.next().is_none());
    if let MacroCallArgs(args) = args.1 {
        assert_eq!(args.len(), 2);
        assert_eq!(args[0].len(), 1);
        assert!(matches!(args[0][0], (_, Int, _)));
        assert_eq!(args[1].len(), 1);
        assert!(matches!(args[1][0], (_, Ident(_), _)));
    } else {
        unreachable!()
    }

    let mut toks = Lexer::new("@name(1 + 2; [1, 2];)", &Rc::clone(&path), &[]);
    assert_eq!(
        toks.next().unwrap().unwrap().1,
        MacroName("@name".to_owned()),
    );
    let args = toks.next().unwrap().unwrap();
    assert!(matches!(args, (_, MacroCallArgs(_), _)));
    assert!(toks.next().is_none());
    if let MacroCallArgs(args) = args.1 {
        assert_eq!(args.len(), 2);
        assert_eq!(args[0].len(), 3);
        assert_eq!(args[0][0].1, IntLiteral("1".to_owned()));
        assert!(matches!(args[0][1], (_, Plus, _)));
        assert_eq!(args[0][2].1, IntLiteral("2".to_owned()));
        assert_eq!(args[1].len(), 5);
        assert!(matches!(args[1][0], (_, BracketOpen, _)));
        assert_eq!(args[1][1].1, IntLiteral("1".to_owned()));
        assert!(matches!(args[1][2], (_, Comma, _)));
        assert_eq!(args[1][3].1, IntLiteral("2".to_owned()));
        assert!(matches!(args[1][4], (_, BracketClose, _)));
    } else {
        unreachable!()
    }

    let mut toks = Lexer::new("@name(var i: int = 0;)", &Rc::clone(&path), &[]);
    assert_eq!(
        toks.next().unwrap().unwrap().1,
        MacroName("@name".to_owned()),
    );
    let args = toks.next().unwrap().unwrap();
    assert!(matches!(args, (_, MacroCallArgs(_), _)));
    assert!(toks.next().is_none());
    if let MacroCallArgs(args) = args.1 {
        assert_eq!(args.len(), 1);
        assert_eq!(args[0].len(), 6);
        assert!(matches!(args[0][0], (_, Var, _)));
        assert!(matches!(args[0][1], (_, Ident(_), _)));
        assert!(matches!(args[0][2], (_, Colon, _)));
        assert!(matches!(args[0][3], (_, Int, _)));
        assert!(matches!(args[0][4], (_, Eq, _)));
        assert_eq!(args[0][5].1, IntLiteral("0".to_owned()));
    } else {
        unreachable!()
    }

    let mut toks = Lexer::new("@name(1,2,3,4,5,6,7,8)", &Rc::clone(&path), &[]);
    assert_eq!(
        toks.next().unwrap().unwrap().1,
        MacroName("@name".to_owned()),
    );
    let args = toks.next().unwrap().unwrap();
    assert!(matches!(args, (_, MacroCallArgs(_), _)));
    assert!(toks.next().is_none());
    if let MacroCallArgs(args) = args.1 {
        assert_eq!(args.len(), 1);
        assert_eq!(args[0].len(), 15);
        assert_eq!(args[0][0].1, IntLiteral("1".to_owned()));
        assert!(matches!(args[0][1], (_, Comma, _)));
        assert_eq!(args[0][2].1, IntLiteral("2".to_owned()));
        assert!(matches!(args[0][3], (_, Comma, _)));
        // etc.
        assert!(matches!(args[0][11], (_, Comma, _)));
        assert_eq!(args[0][12].1, IntLiteral("7".to_owned()));
        assert!(matches!(args[0][13], (_, Comma, _)));
        assert_eq!(args[0][14].1, IntLiteral("8".to_owned()));
    } else {
        unreachable!()
    }
}

#[test]
fn macros_call_badly_formed() {
    use Token::*;

    let path = Rc::from(Path::new("test"));

    // Macro name has no `@`.
    let mut toks = Lexer::new("1 + name() + 2", &Rc::clone(&path), &[]);
    assert_eq!(toks.next().unwrap().unwrap().1, IntLiteral("1".to_owned()),);
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Plus, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Ident(_), _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenClose, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Plus, _)));
    assert_eq!(toks.next().unwrap().unwrap().1, IntLiteral("2".to_owned()),);
    assert!(toks.next().is_none());

    // No closing paren, should backtrack.
    let mut toks = Lexer::new("@name(one; two", &Rc::clone(&path), &[]);
    assert_eq!(
        toks.next().unwrap().unwrap().1,
        MacroName("@name".to_owned()),
    );
    assert!(matches!(toks.next().unwrap().unwrap(), (_, ParenOpen, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Ident(_), _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Semi, _)));
    assert!(matches!(toks.next().unwrap().unwrap(), (_, Ident(_), _)));
    assert!(toks.next().is_none());
}
