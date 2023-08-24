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
    just(Token::Let)
        .then(ident())
        .then(just(Token::Eq))
        .then(immediate().map(ast::Expr::Immediate))
        .map(|(((let_token, name), eq_token), init)| ast::Decl::Value {
            let_token,
            name,
            eq_token,
            init,
        })
        .boxed()
}

fn ident<'sc>() -> impl Parser<Token<'sc>, String, Error = Simple<Token<'sc>>> + Clone {
    select! { Token::Ident(id) => id.to_owned() }.boxed()
}

fn type_<'sc>() -> impl Parser<Token<'sc>, ast::Type, Error = ParseError<'sc>> + Clone {
    recursive(|type_| {
        let tuple = (ident().then_ignore(just(Token::Colon)))
            .or_not()
            .then(type_)
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .delimited_by(just(Token::BraceOpen), just(Token::BraceClose))
            .validate(|args, span, emit| {
                if args.is_empty() {
                    emit(ParseError::EmptyTupleType { span })
                }
                args
            })
            .boxed();

        let type_atom = choice((
            just(Token::Real).to(ast::Type::Real),
            just(Token::Int).to(ast::Type::Int),
            just(Token::Bool).to(ast::Type::Bool),
            just(Token::String).to(ast::Type::String),
            tuple.map(ast::Type::Tuple),
        ))
        .boxed();
    })
}

fn immediate<'sc>() -> impl Parser<Token<'sc>, ast::Immediate, Error = Simple<Token<'sc>>> + Clone {
    select! { Token::Number(num_str) => ast::Immediate::Real(num_str.parse().unwrap()) }.boxed()
}
