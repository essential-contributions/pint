use crate::{error::ParseError, span::Span};
use logos::Logos;
use std::{fmt, rc::Rc};

#[cfg(test)]
mod tests;

#[derive(Clone, Debug, Eq, Hash, Logos, PartialEq, Ord, PartialOrd)]
#[logos(skip r"[ \t\n\r\f]+")]
#[logos(error = ParseError)]
pub enum Token {
    #[token(":")]
    Colon,
    #[token("::")]
    DoubleColon,
    #[token("!")]
    Bang,
    #[token("|")]
    Pipe,
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
    #[token("&&")]
    DoubleAmpersand,
    #[token("||")]
    DoublePipe,
    #[token("'")]
    SingleQuote,

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
    #[token("[")]
    BracketOpen,
    #[token("]")]
    BracketClose,
    #[token("->")]
    Arrow,
    #[token("=>")]
    HeavyArrow,
    #[token(".")]
    Dot,
    #[token("..")]
    TwoDots,

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

    #[token("macro")]
    Macro,
    #[regex(r"@[A-Za-z_][A-Za-z_0-9]*", |lex| lex.slice().to_string())]
    MacroName(String),
    #[regex(r"\$[A-Za-z_0-9]+", |lex| lex.slice().to_string())]
    MacroParam(String),
    #[regex(r"&[A-Za-z_0-9]+", |lex| lex.slice().to_string())]
    MacroParamPack(String),
    MacroBody(Vec<(usize, Token, usize)>),
    MacroCallArgs(Vec<Vec<(usize, Token, usize)>>),
    MacroTag(Option<u64>),

    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("cond")]
    Cond,

    #[token("let")]
    Let,
    #[token("state")]
    State,
    #[token("enum")]
    Enum,
    #[token("type")]
    Type,
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

    #[token("use")]
    Use,
    #[token("self")]
    SelfTok,
    #[token("as")]
    As,

    #[token("interface")]
    Interface,
    #[token("contract")]
    Contract,
    #[token("implements")]
    Implements,
    #[token("extern")]
    Extern,

    #[token("in")]
    In,

    // Generators
    #[token("forall")]
    ForAll,
    #[token("where")]
    Where,

    // Ident has a flag indicating whether it's in a macro argument.  Is generally false.
    #[regex(r"[A-Za-z_][A-Za-z_0-9]*", |lex| {(lex.slice().to_string(), false)})]
    Ident((String, bool)),
    #[regex(r"[0-9]+\.[0-9]+([Ee][-+]?[0-9]+)?|[0-9]+[Ee][-+]?[0-9]+", |lex| lex.slice().to_string())]
    RealLiteral(String),
    #[regex(r"0x[0-9A-Fa-f]+|0b[0-1]+|[0-9]+", |lex| lex.slice().to_string())]
    IntLiteral(String),
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

#[cfg(test)]
pub(super) static KEYWORDS: &[Token] = &[
    Token::Real,
    Token::Int,
    Token::Bool,
    Token::True,
    Token::False,
    Token::String,
    Token::ForAll,
    Token::Fn,
    Token::If,
    Token::Else,
    Token::Cond,
    Token::Let,
    Token::State,
    Token::Constraint,
    Token::Macro,
    Token::Maximize,
    Token::Minimize,
    Token::Solve,
    Token::Satisfy,
    Token::Use,
    Token::SelfTok,
    Token::As,
    Token::Enum,
    Token::Interface,
    Token::Contract,
    Token::Implements,
    Token::Extern,
    Token::In,
    Token::Type,
    Token::Where,
];

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Colon => write!(f, ":"),
            Token::DoubleColon => write!(f, "::"),
            Token::Bang => write!(f, "!"),
            Token::Pipe => write!(f, "|"),
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
            Token::DoubleAmpersand => write!(f, "&&"),
            Token::DoublePipe => write!(f, "||"),
            Token::SingleQuote => write!(f, "'"),
            Token::Semi => write!(f, ";"),
            Token::Comma => write!(f, ","),
            Token::Star => write!(f, "*"),
            Token::BraceOpen => write!(f, "{{"),
            Token::BraceClose => write!(f, "}}"),
            Token::ParenOpen => write!(f, "("),
            Token::ParenClose => write!(f, ")"),
            Token::BracketOpen => write!(f, "["),
            Token::BracketClose => write!(f, "]"),
            Token::Arrow => write!(f, "->"),
            Token::HeavyArrow => write!(f, "=>"),
            Token::Dot => write!(f, "."),
            Token::TwoDots => write!(f, ".."),
            Token::Real => write!(f, "real"),
            Token::Int => write!(f, "int"),
            Token::Bool => write!(f, "bool"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::String => write!(f, "string"),
            Token::Fn => write!(f, "fn"),
            Token::Macro => write!(f, "macro"),
            Token::MacroName(name) => write!(f, "{name}"),
            Token::MacroParam(arg) | Token::MacroParamPack(arg) => write!(f, "{arg}"),
            Token::MacroBody(body) => write!(
                f,
                "{{ {} }}",
                body.iter()
                    .map(|(_, tok, _)| tok.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Token::MacroCallArgs(params) => write!(
                f,
                "{}",
                params
                    .iter()
                    .map(|param| {
                        param
                            .iter()
                            .fold(String::new(), |s, (_, tok, _)| format!("{s} {tok}"))
                    })
                    .collect::<Vec<_>>()
                    .join("; ")
            ),
            Token::MacroTag(tag) => {
                if let Some(tag) = tag {
                    write!(f, "<{tag}>")
                } else {
                    Ok(())
                }
            }
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Cond => write!(f, "cond"),
            Token::Let => write!(f, "let"),
            Token::State => write!(f, "state"),
            Token::Enum => write!(f, "enum"),
            Token::Type => write!(f, "type"),
            Token::Constraint => write!(f, "constraint"),
            Token::Maximize => write!(f, "maximize"),
            Token::Minimize => write!(f, "minimize"),
            Token::Solve => write!(f, "solve"),
            Token::Satisfy => write!(f, "satisfy"),
            Token::Use => write!(f, "use"),
            Token::SelfTok => write!(f, "self"),
            Token::As => write!(f, "as"),
            Token::Interface => write!(f, "interface"),
            Token::Contract => write!(f, "contract"),
            Token::Implements => write!(f, "implements"),
            Token::Extern => write!(f, "extern"),
            Token::In => write!(f, "in"),
            Token::ForAll => write!(f, "forall"),
            Token::Where => write!(f, "where"),
            Token::Ident((ident, _)) => write!(f, "{ident}"),
            Token::RealLiteral(ident) => write!(f, "{ident}"),
            Token::IntLiteral(ident) => write!(f, "{ident}"),
            Token::StringLiteral(contents) => write!(f, "{contents}"),
            Token::Comment => write!(f, "comment"),
        }
    }
}

pub(super) struct Lexer<'a> {
    token_stream: TokenSource<'a>,
    filepath: Rc<std::path::Path>,
    state: LexerState,
}

impl<'sc> Lexer<'sc> {
    pub(super) fn new(src: &'sc str, filepath: &Rc<std::path::Path>) -> Self {
        Self {
            token_stream: TokenSource::LogosLexer(Token::lexer(src)),
            filepath: filepath.clone(),
            state: LexerState::default(),
        }
    }

    pub(super) fn from_tokens(
        tokens: Vec<(usize, Token, usize)>,
        filepath: &Rc<std::path::Path>,
    ) -> Self {
        Self {
            token_stream: TokenSource::VecToken(VecTokenSourceState::new(tokens)),
            filepath: filepath.clone(),
            state: LexerState::default(),
        }
    }

    fn gather_macro_body(
        &mut self,
        obrace_tok: Token,
        obrace_span: &std::ops::Range<usize>,
    ) -> Result<(usize, Token, usize), ParseError> {
        // Copy the lexer in case we need to backtrack.
        let mut body_token_stream = self.token_stream.clone();
        let mut parsed_tok_count = 0;

        // We've already parsed the `{`.  We need to find the matching `}` while counting and
        // skipping nested `{`/`}` pairs.
        let mut body_toks = vec![(obrace_span.start, Token::BraceOpen, obrace_span.end)];
        let mut nest_depth = 0;
        loop {
            parsed_tok_count += 1;

            let next_tok = body_token_stream.next();
            let next_span = body_token_stream.span();
            macro_rules! push_tok {
                ($tok: expr) => {
                    body_toks.push((next_span.start, $tok, next_span.end))
                };
            }

            match next_tok {
                None => {
                    // Unexpected end of stream.  Just return the `{`.
                    return Ok((obrace_span.start, obrace_tok, obrace_span.end));
                }

                Some(Ok(Token::BraceOpen)) => {
                    push_tok!(Token::BraceOpen);
                    nest_depth += 1;
                }

                Some(Ok(Token::BraceClose)) => {
                    push_tok!(Token::BraceClose);
                    if nest_depth > 0 {
                        // We're leaving a nested pair.
                        nest_depth -= 1;
                    } else {
                        // We've found the end of the block. We can consume all these tokens from
                        // the main token_stream -- we've been counting them in `parsed_tok_count`.
                        // (There's an `advance_by` method currently in nightly which would suit in
                        // the future.  Instead we'll just use `nth` to skip ahead.)
                        let _ = self.token_stream.nth(parsed_tok_count - 1);

                        return Ok((
                            obrace_span.start,
                            Token::MacroBody(body_toks),
                            next_span.end,
                        ));
                    }
                }

                Some(Ok(tok @ Token::MacroName(_))) => {
                    // If we see a macro name in a macro body then we inject an empty tag used by
                    // recursion checking.
                    push_tok!(tok);
                    push_tok!(Token::MacroTag(None));
                }

                Some(Ok(tok)) => {
                    push_tok!(tok);
                }

                Some(Err(_)) => {
                    return Err(ParseError::InvalidToken);
                }
            }
        }
    }

    fn gather_macro_call_args(
        &mut self,
        oparen_tok: Token,
        oparen_span: &std::ops::Range<usize>,
    ) -> Result<(usize, Token, usize), ParseError> {
        // Copy the token stream in case we need to backtrack.  Cloning isn't the most efficient
        // way to do this, especially with TokenSource::VecToken, but it works.
        let mut args_token_stream = self.token_stream.clone();
        let mut parsed_tok_count = 0;

        // We've already parsed the `(`.  Next we need any tokens up to delimiting `;` or
        // terminating `)`.
        let mut all_args: Vec<Vec<(usize, Token, usize)>> = vec![Vec::new()];
        loop {
            parsed_tok_count += 1;
            match args_token_stream.next() {
                None => {
                    // Unexpected end of stream.  Just return the `(`.
                    return Ok((oparen_span.start, oparen_tok, oparen_span.end));
                }

                Some(Ok(Token::Semi)) => {
                    // The end of some arg tokens.
                    all_args.push(Vec::new());
                }

                Some(Ok(Token::ParenClose)) => {
                    // We've successfully parsed the args.  We can consume all these tokens from
                    // the main token_stream -- we've been counting them in `parsed_tok_count`.
                    // (There's an `advance_by` method currently in nightly which would suit in the
                    // future.  Instead we'll just use `nth` to skip ahead.)
                    let _ = self.token_stream.nth(parsed_tok_count - 1);

                    // Special case: if the last arg vec is empty then there are no args or we've
                    // seen a semi after the last arg.  Just remove the empty arg.
                    if all_args
                        .last()
                        .expect("Args vec is always valid.")
                        .is_empty()
                    {
                        all_args.pop();
                    }

                    let cparen_span = args_token_stream.span();
                    return Ok((
                        oparen_span.start,
                        Token::MacroCallArgs(all_args),
                        cparen_span.end,
                    ));
                }

                Some(Ok(Token::Ident((s, _)))) => {
                    // When identifiers are in macro args we set the flag to true. Then while
                    // parsing expanded macro bodies, the `let` decls can implement hygiene.
                    let tok_span = args_token_stream.span();
                    all_args
                        .last_mut()
                        .expect("Args vec is always valid.")
                        .push((tok_span.start, Token::Ident((s, true)), tok_span.end))
                }

                // A regular parameter token.
                Some(Ok(tok)) => {
                    let tok_span = args_token_stream.span();
                    all_args
                        .last_mut()
                        .expect("Args vec is always valid.")
                        .push((tok_span.start, tok, tok_span.end))
                }

                Some(Err(_)) => {
                    return Err(ParseError::InvalidToken);
                }
            }
        }
    }
}

#[derive(Clone)]
enum TokenSource<'a> {
    // Used when we parse from a source string using Logos.
    LogosLexer(logos::Lexer<'a, Token>),

    // Used by macro expansion when parsing macro bodies.
    VecToken(VecTokenSourceState),
}

#[derive(Clone)]
struct VecTokenSourceState {
    toks: Vec<(usize, Token, usize)>,
    index: usize,
    start: usize,
    end: usize,
}

impl VecTokenSourceState {
    fn new(toks: Vec<(usize, Token, usize)>) -> Self {
        VecTokenSourceState {
            toks,
            index: 0,
            start: 0,
            end: 0,
        }
    }
}

impl<'a> TokenSource<'a> {
    fn next(&mut self) -> Option<Result<Token, ParseError>> {
        match self {
            TokenSource::LogosLexer(lex) => lex.next(),
            TokenSource::VecToken(_) => self.nth(0),
        }
    }

    fn nth(&mut self, n: usize) -> Option<Result<Token, ParseError>> {
        match self {
            TokenSource::LogosLexer(lex) => lex.nth(n),
            TokenSource::VecToken(state) => {
                state.toks.get(state.index + n).cloned().map(|(s, t, e)| {
                    state.index += n + 1;
                    state.start = s;
                    state.end = e;

                    Ok(t)
                })
            }
        }
    }

    fn span(&self) -> std::ops::Range<usize> {
        match self {
            TokenSource::LogosLexer(lex) => lex.span(),
            TokenSource::VecToken(state) => state.start..state.end,
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
enum LexerState {
    #[default]
    Normal,

    Macro,
    MacroName,
    MacroParams,

    MacroCall,
}

// Iterator adapter which is converting the token stream into something LALRPOP can work with.
//
// We implement special case macro parsing here as an adapter to the adapter, as we need to wrap up
// macro params and body tokens before passing them to the parser.
impl<'a> Iterator for Lexer<'a> {
    type Item = Result<(usize, Token, usize), ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.token_stream.next().map(|res| {
            let span = self.token_stream.span();
            res.and_then(|tok| match tok {
                // The following states track a macro declaration going from `macro` to `@name` to
                // `($a, $b, ...)` to the `{ ... }` body.
                Token::Macro if self.state == LexerState::Normal => {
                    // Set the state to indicate we've seen the `macro` token.
                    self.state = LexerState::Macro;
                    Ok((span.start, tok, span.end))
                }
                Token::MacroName(_) if self.state == LexerState::Macro => {
                    // Set the state to indicate we've seen the `macro` then `@name` tokens.
                    self.state = LexerState::MacroName;
                    Ok((span.start, tok, span.end))
                }
                Token::ParenOpen if self.state == LexerState::MacroName => {
                    // We're now skipping macro params.
                    self.state = LexerState::MacroParams;
                    Ok((span.start, tok, span.end))
                }
                Token::MacroParam(_)
                | Token::MacroParamPack(_)
                | Token::Comma
                | Token::ParenClose
                    if self.state == LexerState::MacroParams =>
                {
                    // Remain in the macro params state.
                    Ok((span.start, tok, span.end))
                }
                Token::BraceOpen if self.state == LexerState::MacroParams => {
                    // We've skipped the params and now we see an `{`.  We need to gather all the
                    // body tokens up into a MacroBody token.
                    self.state = LexerState::Normal;
                    self.gather_macro_body(tok, &span)
                }

                // The following states track a macro call, going to `@name` (without a preceding
                // `macro`) to the `(a; b; ..)` args.
                Token::MacroName(_) if self.state == LexerState::Normal => {
                    // Set the state to indicate we've seen a `@name` token without a `macro`.
                    self.state = LexerState::MacroCall;
                    Ok((span.start, tok, span.end))
                }
                Token::MacroTag(_) if self.state == LexerState::MacroCall => {
                    // Skip the tag if it exists, don't change the state.
                    Ok((span.start, tok, span.end))
                }
                Token::ParenOpen if self.state == LexerState::MacroCall => {
                    // We need to gather all the args tokens up into a MacroCallArgs token.
                    self.state = LexerState::Normal;
                    self.gather_macro_call_args(tok, &span)
                }

                _ => {
                    // Either we're not in a macro or none of the above matched and we're resetting
                    // back to normal.
                    self.state = LexerState::Normal;
                    Ok((span.start, tok, span.end))
                }
            })
            .map_err(|_| ParseError::Lex {
                span: Span::new(self.filepath.clone(), span.start..span.end),
            })
        })
    }
}

#[derive(Clone, Debug, Eq, Hash, Logos, PartialEq, Ord, PartialOrd)]
#[logos(error = ParseError)]
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
