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
    choice((value_decl(expr()), solve_decl(), fn_decl()))
        .then_ignore(just(Token::Semi))
        .repeated()
        .then_ignore(end())
        .boxed()
}

fn value_decl<'sc>(
    expr: impl Parser<Token<'sc>, ast::Expr, Error = Simple<Token<'sc>>> + Clone + 'sc,
) -> impl Parser<Token<'sc>, ast::Decl<'sc>, Error = Simple<Token<'sc>>> + Clone {
    let type_spec = just(Token::Colon).then(type_());
    let init = just(Token::Eq).then(expr);

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

fn solve_decl<'sc>() -> impl Parser<Token<'sc>, ast::Decl<'sc>, Error = Simple<Token<'sc>>> + Clone
{
    just(Token::Solve)
        .then(directive())
        .then(expr().or_not())
        .map(|((solve_token, directive), expr)| ast::Decl::Solve {
            solve_token,
            directive,
            expr,
        })
        .boxed()
}

fn fn_decl<'sc>() -> impl Parser<Token<'sc>, ast::Decl<'sc>, Error = Simple<Token<'sc>>> + Clone {
    let type_spec = just(Token::Colon).ignore_then(type_());

    let params = ident()
        .then(type_spec)
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .delimited_by(just(Token::ParenOpen), just(Token::ParenClose))
        .boxed();

    let return_type = just(Token::Arrow).ignore_then(type_());

    just(Token::Fn)
        .then(ident())
        .then(params)
        .then(return_type)
        .map(|(((fn_token, name), params), return_type)| ast::Decl::Fn {
            fn_token,
            name,
            fn_sig: Some(params),
            return_type,
        })
}

fn ident<'sc>() -> impl Parser<Token<'sc>, String, Error = Simple<Token<'sc>>> + Clone {
    select! { Token::Ident(id) => id.to_owned() }.boxed()
}

fn immediate<'sc>() -> impl Parser<Token<'sc>, ast::Immediate, Error = Simple<Token<'sc>>> + Clone {
    select! { Token::Literal(str) => ast::Immediate(str.to_string()) }.boxed()
}

fn type_<'sc>() -> impl Parser<Token<'sc>, ast::Type, Error = Simple<Token<'sc>>> + Clone {
    select! { Token::Primitive(type_str) => ast::Type::Primitive(type_str.parse().unwrap()) }
        .boxed()
}

fn expr<'sc>() -> impl Parser<Token<'sc>, ast::Expr, Error = Simple<Token<'sc>>> + Clone {
    recursive(|_| {
        choice((
            immediate().map(ast::Expr::Immediate),
            path().map(ast::Expr::Path),
        ))
    })
    .boxed()
}

fn directive<'sc>() -> impl Parser<Token<'sc>, String, Error = Simple<Token<'sc>>> + Clone {
    select! { Token::Directive(dir) => dir.to_owned() }.boxed()
}

pub(super) fn path<'sc>() -> impl Parser<Token<'sc>, ast::Path, Error = Simple<Token<'sc>>> + Clone
{
    let relative_path = ident().then((just(Token::DoubleColon).ignore_then(ident())).repeated());
    just(Token::DoubleColon)
        .or_not()
        .then(relative_path)
        .map(|(pre_colon, (id, mut path))| {
            path.insert(0, id);
            ast::Path {
                pre_colon: pre_colon.is_some(),
                idents: path,
            }
        })
        .boxed()
}
