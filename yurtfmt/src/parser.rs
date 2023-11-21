use crate::{
    ast,
    error::{FormatterError, ParseError},
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
                    error: error.clone(),
                })
                .collect();

            errors.extend(parsing_errors);
            Err(errors)
        }
        Ok(ast) => Ok(ast),
    }
}

pub(super) fn yurt_program<'sc>(
) -> impl Parser<Token<'sc>, ast::Ast<'sc>, Error = ParseError> + Clone {
    choice((
        use_statement(),
        value_decl(expr()),
        state_decl(),
        solve_decl(),
        fn_decl(),
        constraint_decl(expr()),
        enum_decl(),
        type_decl(),
        interface_decl(),
        contract_decl(),
        extern_decl(),
    ))
    .repeated()
    .then_ignore(end())
    .boxed()
}

pub(super) fn use_statement<'sc>(
) -> impl Parser<Token<'sc>, ast::Decl<'sc>, Error = ParseError> + Clone {
    just(Token::Use)
        .ignore_then(use_tree())
        .then_ignore(just(Token::Semi))
        .map(|use_tree| ast::Decl::Use { use_tree })
}

pub(super) fn use_tree<'sc>() -> impl Parser<Token<'sc>, ast::UseTree, Error = ParseError> + Clone {
    recursive(|use_tree| {
        let name = ident().map(ast::UseTree::Name);

        let path = ident()
            .then_ignore(just(Token::DoubleColon))
            .then(use_tree.clone())
            .map(|(prefix, suffix)| ast::UseTree::Path {
                prefix,
                suffix: Box::new(suffix),
            })
            .boxed();

        let group = use_tree
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .delimited_by(just(Token::BraceOpen), just(Token::BraceClose))
            .map(|imports| ast::UseTree::Group { imports })
            .boxed();

        let alias = ident()
            .then_ignore(just(Token::As))
            .then(ident())
            .map(|(name, alias)| ast::UseTree::Alias { name, alias })
            .boxed();

        choice((path, alias, name, group)).boxed()
    })
}

fn value_decl<'sc>(
    expr: impl Parser<Token<'sc>, ast::Expr<'sc>, Error = ParseError> + Clone + 'sc,
) -> impl Parser<Token<'sc>, ast::Decl<'sc>, Error = ParseError> + Clone {
    let type_spec = just(Token::Colon).ignore_then(type_());
    let init = just(Token::Eq).ignore_then(range(expr.clone()).or(expr));

    just(Token::Let)
        .ignore_then(ident())
        .then(type_spec.or_not())
        .then(init.or_not())
        .then_ignore(just(Token::Semi))
        .map(|((name, ty), init)| ast::Decl::Value { name, ty, init })
        .boxed()
}

fn solve_decl<'sc>() -> impl Parser<Token<'sc>, ast::Decl<'sc>, Error = ParseError> + Clone {
    just(Token::Solve)
        .ignore_then(directive())
        .then(expr().or_not())
        .then_ignore(just(Token::Semi))
        .map(|(directive, expr)| ast::Decl::Solve { directive, expr })
        .boxed()
}

fn type_decl<'sc>() -> impl Parser<Token<'sc>, ast::Decl<'sc>, Error = ParseError> + Clone {
    just(Token::Type)
        .ignore_then(ident())
        .then_ignore(just(Token::Eq))
        .then(type_())
        .then_ignore(just(Token::Semi))
        .map(|(name, ty)| ast::Decl::NewType { name, ty })
        .boxed()
}

fn constraint_decl<'sc>(
    expr: impl Parser<Token<'sc>, ast::Expr<'sc>, Error = ParseError> + Clone + 'sc,
) -> impl Parser<Token<'sc>, ast::Decl<'sc>, Error = ParseError> + Clone {
    just(Token::Constraint)
        .ignore_then(expr)
        .then_ignore(just(Token::Semi))
        .map(|expr| ast::Decl::Constraint { expr })
        .boxed()
}

fn contract_decl<'sc>() -> impl Parser<Token<'sc>, ast::Decl<'sc>, Error = ParseError> + Clone {
    let expr = expr()
        .delimited_by(just(Token::ParenOpen), just(Token::ParenClose))
        .boxed();

    let paths = just(Token::Implements)
        .ignore_then(path().separated_by(just(Token::Comma)))
        .boxed();

    let fn_sigs = (fn_sig().then_ignore(just(Token::Semi)))
        .repeated()
        .delimited_by(just(Token::BraceOpen), just(Token::BraceClose))
        .boxed();

    just(Token::Contract)
        .ignore_then(ident())
        .then(expr)
        .then(paths.or_not())
        .then(fn_sigs)
        .map(|(((name, expr), paths), fn_sigs)| ast::Decl::Contract {
            name,
            expr,
            paths,
            fn_sigs,
        })
        .boxed()
}

fn interface_decl<'sc>() -> impl Parser<Token<'sc>, ast::Decl<'sc>, Error = ParseError> + Clone {
    just(Token::Interface)
        .ignore_then(ident())
        .then_ignore(just(Token::BraceOpen))
        .then((fn_sig().then_ignore(just(Token::Semi))).repeated())
        .then_ignore(just(Token::BraceClose))
        .map(|(name, fn_sigs)| ast::Decl::Interface { name, fn_sigs })
        .boxed()
}

fn state_decl<'sc>() -> impl Parser<Token<'sc>, ast::Decl<'sc>, Error = ParseError> + Clone {
    let type_spec = just(Token::Colon).ignore_then(type_()).boxed();

    just(Token::State)
        .ignore_then(ident())
        .then(type_spec.or_not())
        .then_ignore(just(Token::Eq))
        .then(expr())
        .then_ignore(just(Token::Semi))
        .map(|((name, ty), expr)| ast::Decl::State { name, ty, expr })
}

fn extern_decl<'sc>() -> impl Parser<Token<'sc>, ast::Decl<'sc>, Error = ParseError> + Clone {
    just(Token::Extern)
        .ignore_then(
            (fn_sig().then_ignore(just(Token::Semi)))
                .repeated()
                .delimited_by(just(Token::BraceOpen), just(Token::BraceClose)),
        )
        .map(|fn_sigs| ast::Decl::Extern { fn_sigs })
        .boxed()
}

fn enum_decl<'sc>() -> impl Parser<Token<'sc>, ast::Decl<'sc>, Error = ParseError> + Clone {
    let variants = ident().separated_by(just(Token::Pipe)).boxed();

    just(Token::Enum)
        .ignore_then(ident())
        .then_ignore(just(Token::Eq))
        .then(variants)
        .then_ignore(just(Token::Semi))
        .map(|(name, variants)| ast::Decl::Enum { name, variants })
        .boxed()
}

pub(super) fn fn_decl<'sc>() -> impl Parser<Token<'sc>, ast::Decl<'sc>, Error = ParseError> + Clone
{
    fn_sig()
        .then(code_block_expr(expr()))
        .map(|(fn_sig, body)| ast::Decl::Fn { fn_sig, body })
}

pub(super) fn fn_sig<'sc>() -> impl Parser<Token<'sc>, ast::FnSig<'sc>, Error = ParseError> + Clone
{
    let return_type = just(Token::Arrow).ignore_then(type_()).boxed();

    let type_spec = just(Token::Colon).ignore_then(type_()).boxed();

    let params = ident()
        .then(type_spec)
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .delimited_by(just(Token::ParenOpen), just(Token::ParenClose))
        .boxed();

    just(Token::Fn)
        .ignore_then(ident())
        .then(params)
        .then(return_type)
        .map(|((name, params), return_type)| ast::FnSig {
            name,
            params: Some(params),
            return_type,
        })
}

pub(super) fn code_block_expr<'a, 'sc>(
    expr: impl Parser<Token<'sc>, ast::Expr<'sc>, Error = ParseError> + Clone + 'sc,
) -> impl Parser<Token<'sc>, ast::Block<'sc>, Error = ParseError> + Clone {
    let code_block_body = choice((
        value_decl(expr.clone()),
        // state_decl(expr.clone()), TODO: add when state is supported
        constraint_decl(expr.clone()),
    ))
    .repeated()
    .then(expr)
    .boxed();

    code_block_body
        .delimited_by(just(Token::BraceOpen), just(Token::BraceClose))
        .map(|(statements, expr)| ast::Block {
            statements,
            final_expr: Box::new(expr),
        })
        .boxed()
}

fn ident<'sc>() -> impl Parser<Token<'sc>, String, Error = ParseError> + Clone {
    select! { Token::Ident(id) => id.to_owned() }.boxed()
}

fn immediate<'sc>() -> impl Parser<Token<'sc>, ast::Immediate, Error = ParseError> + Clone {
    select! { Token::Literal(str) => ast::Immediate(str.to_string()) }.boxed()
}

pub(super) fn type_<'sc>() -> impl Parser<Token<'sc>, ast::Type<'sc>, Error = ParseError> + Clone {
    recursive(|type_| {
        let tuple = (ident().then_ignore(just(Token::Colon)))
            .or_not()
            .then(type_.clone())
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .delimited_by(just(Token::BraceOpen), just(Token::BraceClose))
            .map(ast::Type::Tuple)
            .boxed();

        let type_atom = choice((
            select! { Token::Primitive(type_str) => ast::Type::Primitive(type_str.parse().unwrap()) },
            tuple,
        ))
        .boxed();

        let array = type_atom
            .clone()
            .then(
                expr()
                    .delimited_by(just(Token::BracketOpen), just(Token::BracketClose))
                    .repeated()
                    .at_least(1),
            )
            .map(|(ty, ranges)| ast::Type::Array((Box::new(ty), ranges)))
            .boxed();

        choice((array, type_atom))
    })
}

pub(super) fn expr<'sc>() -> impl Parser<Token<'sc>, ast::Expr<'sc>, Error = ParseError> + Clone {
    recursive(|expr| {
        let call = path()
            .then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .delimited_by(just(Token::ParenOpen), just(Token::ParenClose)),
            )
            .map(|(path, args)| ast::Expr::Call(ast::Call { path, args }))
            .boxed();

        let atom = choice((
            unary_op(expr.clone()),
            immediate().map(ast::Expr::Immediate),
            call,
            path().map(ast::Expr::Path),
        ))
        .boxed();

        let in_expr = in_expr(atom, expr.clone()).boxed();
        binary_op(in_expr).boxed()
    })
}

fn directive<'sc>() -> impl Parser<Token<'sc>, String, Error = ParseError> + Clone {
    select! { Token::Directive(dir) => dir.to_owned() }.boxed()
}

pub(super) fn path<'sc>() -> impl Parser<Token<'sc>, ast::Path, Error = ParseError> + Clone {
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

fn unary_op<'sc>(
    expr: impl Parser<Token<'sc>, ast::Expr<'sc>, Error = ParseError> + Clone + 'sc,
) -> impl Parser<Token<'sc>, ast::Expr<'sc>, Error = ParseError> + Clone + 'sc {
    choice((
        just(Token::Plus).to("+"),
        just(Token::Minus).to("-"),
        just(Token::Bang).to("!"),
    ))
    .then(expr)
    .map(|(prefix_op, expr)| {
        ast::Expr::UnaryOp(ast::UnaryOp {
            prefix_op,
            expr: Box::new(expr),
        })
    })
    .boxed()
}

fn in_expr<'sc, P>(
    parser: P,
    expr: impl Parser<Token<'sc>, ast::Expr<'sc>, Error = ParseError> + Clone + 'sc,
) -> impl Parser<Token<'sc>, ast::Expr<'sc>, Error = ParseError> + Clone
where
    P: Parser<Token<'sc>, ast::Expr<'sc>, Error = ParseError> + Clone + 'sc,
{
    parser
        .clone()
        .then(
            just(Token::In)
                .ignore_then(range(expr.clone()).or(expr))
                .repeated(),
        )
        .foldl(|lhs, rhs| {
            ast::Expr::In(ast::In {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            })
        })
        .boxed()
}

pub(super) fn range<'sc, P>(
    parser: P,
) -> impl Parser<Token<'sc>, ast::Expr<'sc>, Error = ParseError> + Clone
where
    P: Parser<Token<'sc>, ast::Expr<'sc>, Error = ParseError> + Clone + 'sc,
{
    parser
        .clone()
        .then(just(Token::TwoDots).ignore_then(parser))
        .map(|(lb, ub)| {
            ast::Expr::Range(ast::Range {
                lb: Box::new(lb),
                ub: Box::new(ub),
            })
        })
        .boxed()
}

fn binary_op<'sc, P>(
    parser: P,
) -> impl Parser<Token<'sc>, ast::Expr<'sc>, Error = ParseError> + Clone
where
    P: Parser<Token<'sc>, ast::Expr<'sc>, Error = ParseError> + Clone + 'sc,
{
    parser
        .clone()
        .then(
            choice((
                just(Token::Plus).to("+"),
                just(Token::Minus).to("-"),
                select! { Token::BinaryOp(op) => op },
            ))
            .then(parser)
            .repeated(),
        )
        .foldl(|lhs, (op, rhs)| {
            ast::Expr::BinaryOp(ast::BinaryOp {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            })
        })
        .boxed()
}
