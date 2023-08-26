use crate::{
    ast,
    error::FormatterError,
    lexer::{self, Token},
};
use chumsky::{prelude::*, Stream};

/// Parse `source` and returns an AST. Upon failure, return a vector of all compile errors
/// encountered.
pub(super) fn parse_str_to_ast(source: &str) -> Result<ast::Ast<'_>, Vec<FormatterError>> {
    let mut errors = vec![];

    // Lex the input into tokens and spans. Also collect any lex errors encountered.
    let (tokens, lex_errors) = lexer::lex(source);
    errors.extend(lex_errors);

    // Provide a token stream
    let eoi_span = source.len()..source.len();
    let token_stream = Stream::from_iter(eoi_span, tokens.into_iter());

    // Parse the token stream
    match yurt_program().parse(token_stream) {
        Ok(_) if !errors.is_empty() => Err(errors),
        Err(parsing_errors) => {
            let parsing_errors: Vec<_> = parsing_errors
                .iter()
                .map(|error| FormatterError::Parse {
                    error: Box::new(error.clone()),
                })
                .collect();

            errors.extend(parsing_errors);
            Err(errors)
        }
        Ok(ast) => Ok(ast),
    }
}

pub(super) fn yurt_program<'sc>(
) -> impl Parser<Token<'sc>, ast::Ast<'sc>, Error = Simple<Token<'sc>>> + Clone {
    value_decl()
        .then_ignore(just(Token::Semi))
        .repeated()
        .then_ignore(end())
        .boxed()
}

fn value_decl<'sc>() -> impl Parser<Token<'sc>, ast::Decl<'sc>, Error = Simple<Token<'sc>>> + Clone
{
    let type_spec = just(Token::Colon).then(type_());
    let init = just(Token::Eq).then(immediate().map(ast::Expr::Immediate));

    just(Token::Let)
        .then(ident())
        .then(type_spec.or_not())
        .then(init.or_not())
        .map(
            |(((let_token, name), colon_token_and_ty), eq_token_and_init)| ast::Decl::Value {
                let_token,
                name,
                colon_token_and_ty,
                eq_token_and_init,
            },
        )
        .boxed()
}

fn ident<'sc>() -> impl Parser<Token<'sc>, String, Error = Simple<Token<'sc>>> + Clone {
    select! { Token::Ident(id) => id.to_owned() }.boxed()
}

fn immediate<'sc>() -> impl Parser<Token<'sc>, ast::Immediate, Error = Simple<Token<'sc>>> + Clone {
    select! {
    Token::NumberLiteral(num_str) => ast::Immediate::Number(num_str.parse().unwrap()),
    Token::BoolLiteral(bool_str) => ast::Immediate::Bool(bool_str.parse().unwrap()),
    Token::StringLiteral(str) => ast::Immediate::String(str.parse().unwrap()) }
    .boxed()
}

fn type_<'sc>() -> impl Parser<Token<'sc>, ast::Type, Error = Simple<Token<'sc>>> + Clone {
    select! { Token::Primitive(type_str) => ast::Type::Primitive(type_str.parse().unwrap()) }
        .boxed()
}
