use chumsky::Stream;
use lalrpop_util::lalrpop_mod;

use crate::{
    error::FormatterError,
    lexer::{self, Token},
};

lalrpop_mod!(#[allow(clippy::ptr_arg, clippy::type_complexity)] pub pintfmt_parser);

pub fn parse_str_to_ast(source: &str) {
    let lexer = lexer::Lexer::new(&source);
    let parser = pintfmt_parser::NewlineParser::new();
    let ast = parser.parse(lexer);

    println!("{:?}", ast);

    // let mut errors = vec![];

    // // Lex the input into tokens and spans. Also collect any lex errors encountered.
    // let (tokens, lex_errors) = lexer::lex(source);
    // errors.extend(lex_errors);

    // // Preserve only newlines following semicolons from token stream
    // let mut tokens_iter = tokens.into_iter().peekable();
    // let mut tokens_without_newlines = Vec::new();
    // let mut prev_token = None;

    // while let Some(token) = tokens_iter.next() {
    //     let token_clone = token.clone();
    //     match token {
    //         (Token::Newline, _) if matches!(prev_token, Some((Token::Semi, _))) => {
    //             if matches!(tokens_iter.peek(), Some((Token::Newline, _))) {
    //                 tokens_without_newlines.push(token);
    //             }
    //         }
    //         (Token::Newline, _) => {}
    //         _ => tokens_without_newlines.push(token),
    //     }
    //     prev_token = Some(token_clone);
    // }

    // // Provide a token stream
    // let eoi_span = source.len()..source.len();
    // let token_stream = Stream::from_iter(eoi_span.clone(), tokens_without_newlines.into_iter());

    // Parse the token stream

    // match pint_program().parse(token_stream) {
    //     Ok(_) if !errors.is_empty() => Err(errors),
    //     Err(parsing_errors) => {
    //         let parsing_errors: Vec<_> = parsing_errors
    //             .iter()
    //             .map(|error| FormatterError::Parse {
    //                 error: error.clone(),
    //             })
    //             .collect();

    //         errors.extend(parsing_errors);
    //         Err(errors)
    //     }
    //     Ok(ast) => Ok(ast),
    // }

    // assert!(pintfmt_parser::TermParser::new().parse("22").is_ok());
    // assert!(pintfmt_parser::TermParser::new().parse("22").is_err());
}
