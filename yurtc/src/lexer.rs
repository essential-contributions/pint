use logos::Logos;

type Span = std::ops::Range<usize>;

#[derive(Clone, Debug, Eq, Hash, Logos, PartialEq)]
#[logos(skip r"[ \t\n\r\f]+")]
pub(super) enum Token {
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

    #[regex(r"[A-Za-z_][A-Za-z_0-9]*", |lex| lex.slice().to_owned())]
    Ident(String),
    #[regex(r"-?[0-9]+(\.[0-9]+)?", |lex| lex.slice().to_owned())]
    Number(String),

    #[regex(r"//[^\n\r]*", logos::skip)]
    Comment,
}

pub(super) fn lex(src: &str) -> Result<Vec<(Token, Span)>, ()> {
    Token::lexer(src)
        .spanned()
        .map(|(tok, span)| tok.map(|tok| (tok, span)))
        .collect()
}
