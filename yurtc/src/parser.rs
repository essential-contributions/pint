use crate::{
    ast,
    error::{print_on_failure, CompileError, ParseError},
    expr,
    lexer::{self, Token, KEYWORDS},
};
use chumsky::{prelude::*, Stream};
use itertools::Either;
use regex::Regex;
use std::{fs::read_to_string, path::Path};

#[cfg(test)]
mod tests;

type Ast = Vec<ast::Decl>;

pub(super) fn parse_path_to_ast(path: &Path, filename: &str) -> anyhow::Result<Ast> {
    parse_str_to_ast(&read_to_string(path)?, filename)
}

/// Parse `source` and returns an AST. Upon failure, print all compile errors and exit.
fn parse_str_to_ast(source: &str, filename: &str) -> anyhow::Result<Ast> {
    match parse_str_to_ast_inner(source) {
        Ok(ast) => Ok(ast),
        Err(errors) => {
            if !cfg!(test) {
                print_on_failure(filename, source, &errors);
            }
            yurtc_bail!(errors.len(), filename)
        }
    }
}

/// Parse `source` and returns an AST. Upon failure, return a vector of all compile errors
/// encountered.
fn parse_str_to_ast_inner(source: &str) -> Result<Ast, Vec<CompileError>> {
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
                .map(|error| CompileError::Parse {
                    error: error.clone(),
                })
                .collect();

            errors.extend(parsing_errors);
            Err(errors)
        }
        Ok(ast) => Ok(ast),
    }
}

fn yurt_program<'sc>() -> impl Parser<Token<'sc>, Ast, Error = ParseError<'sc>> + Clone {
    choice((
        use_statement(),
        let_decl(expr()),
        state_decl(expr()),
        constraint_decl(expr()),
        solve_decl(),
        fn_decl(expr()),
        enum_decl(),
        interface_decl(expr()),
        contract_decl(expr()),
    ))
    .repeated()
    .then_ignore(end())
}

fn use_tree<'sc>() -> impl Parser<Token<'sc>, ast::UseTree, Error = ParseError<'sc>> + Clone {
    recursive(|use_tree| {
        let path = ident()
            .then_ignore(just(Token::DoubleColon))
            .then(use_tree.clone())
            .map(|(prefix, suffix)| ast::UseTree::Path {
                prefix,
                suffix: Box::new(suffix),
            })
            .boxed();

        let glob = just(Token::Star).to(ast::UseTree::Glob);

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

        let name = ident().map(|name| ast::UseTree::Name { name });

        choice((path, alias, name, glob, group)).boxed()
    })
}

fn use_statement<'sc>() -> impl Parser<Token<'sc>, ast::Decl, Error = ParseError<'sc>> + Clone {
    just(Token::Use)
        .ignore_then(just(Token::DoubleColon).or_not())
        .then(use_tree())
        .then_ignore(just(Token::Semi))
        .map(|(double_colon, use_tree)| ast::Decl::Use {
            is_absolute: double_colon.is_some(),
            use_tree,
        })
        .boxed()
}

fn let_decl<'sc>(
    expr: impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone + 'sc,
) -> impl Parser<Token<'sc>, ast::Decl, Error = ParseError<'sc>> + Clone {
    let type_spec = just(Token::Colon).ignore_then(type_(expr.clone()));
    let init = just(Token::Eq).ignore_then(expr);
    just(Token::Let)
        .ignore_then(ident())
        .then(type_spec.or_not())
        .then(init.or_not())
        .then_ignore(just(Token::Semi))
        .validate(|((name, ty), init), span, emit| {
            if ty.is_none() && init.is_none() {
                emit(ParseError::UntypedVariable {
                    span,
                    name: name.clone(),
                })
            }
            ((name, ty), init)
        })
        .map_with_span(|((name, ty), init), span| ast::Decl::Let {
            name,
            ty,
            init,
            span,
        })
        .boxed()
}

fn state_decl<'sc>(
    expr: impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone + 'sc,
) -> impl Parser<Token<'sc>, ast::Decl, Error = ParseError<'sc>> + Clone {
    let type_spec = just(Token::Colon).ignore_then(type_(expr.clone()));
    let init = just(Token::Eq).ignore_then(expr);
    just(Token::State)
        .ignore_then(ident())
        .then(type_spec.or_not())
        .then(init)
        .then_ignore(just(Token::Semi))
        .map_with_span(|((name, ty), init), span| ast::Decl::State {
            name,
            ty,
            init,
            span,
        })
        .boxed()
}

fn enum_decl<'sc>() -> impl Parser<Token<'sc>, ast::Decl, Error = ParseError<'sc>> + Clone {
    just(Token::Enum)
        .ignore_then(ident().map_with_span(|id, span| (id, span)))
        .then_ignore(just(Token::Eq))
        .then(ident().separated_by(just(Token::Pipe)))
        .then_ignore(just(Token::Semi))
        .map(|((name, name_span), variants)| ast::Decl::Enum {
            name,
            variants,
            name_span,
        })
        .boxed()
}

fn constraint_decl<'sc>(
    expr: impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone + 'sc,
) -> impl Parser<Token<'sc>, ast::Decl, Error = ParseError<'sc>> + Clone {
    just(Token::Constraint)
        .ignore_then(expr)
        .then_ignore(just(Token::Semi))
        .map_with_span(|expr, span| ast::Decl::Constraint { expr, span })
        .boxed()
}

fn solve_decl<'sc>() -> impl Parser<Token<'sc>, ast::Decl, Error = ParseError<'sc>> + Clone {
    let solve_satisfy = just(Token::Satisfy).to(ast::SolveFunc::Satisfy);
    let solve_minimize = just(Token::Minimize)
        .ignore_then(ident_path())
        .map(ast::SolveFunc::Minimize);
    let solve_maximize = just(Token::Maximize)
        .ignore_then(ident_path())
        .map(ast::SolveFunc::Maximize);

    just(Token::Solve)
        .ignore_then(choice((solve_satisfy, solve_minimize, solve_maximize)))
        .then_ignore(just(Token::Semi))
        .map_with_span(|directive, span| ast::Decl::Solve { directive, span })
        .boxed()
}

fn fn_decl<'sc>(
    expr: impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone + 'sc,
) -> impl Parser<Token<'sc>, ast::Decl, Error = ParseError<'sc>> + Clone {
    fn_sig(expr.clone())
        .then(code_block_expr(expr))
        .map(|(fn_sig, body)| ast::Decl::Fn { fn_sig, body })
        .boxed()
}

fn fn_sig<'sc>(
    expr: impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone + 'sc,
) -> impl Parser<Token<'sc>, ast::FnSig, Error = ParseError<'sc>> + Clone {
    let type_spec = just(Token::Colon).ignore_then(type_(expr.clone()));

    let params = ident()
        .then(type_spec)
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .delimited_by(just(Token::ParenOpen), just(Token::ParenClose))
        .boxed();

    let return_type = just(Token::Arrow).ignore_then(type_(expr));

    just(Token::Fn)
        .ignore_then(ident())
        .then(params)
        .then(return_type)
        .map_with_span(|((name, params), return_type), span| ast::FnSig {
            name,
            params,
            return_type,
            span,
        })
        .boxed()
}

fn interface_decl<'sc>(
    expr: impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone + 'sc,
) -> impl Parser<Token<'sc>, ast::Decl, Error = ParseError<'sc>> + Clone {
    just(Token::Interface)
        .ignore_then(ident().map_with_span(|id, span| (id, span)))
        .then(
            (fn_sig(expr).then_ignore(just(Token::Semi)))
                .repeated()
                .delimited_by(just(Token::BraceOpen), just(Token::BraceClose)),
        )
        .map(|((name, name_span), functions)| ast::Decl::Interface {
            name,
            functions,
            name_span,
        })
        .boxed()
}

fn contract_decl<'sc>(
    expr: impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone + 'sc,
) -> impl Parser<Token<'sc>, ast::Decl, Error = ParseError<'sc>> + Clone {
    just(Token::Contract)
        .ignore_then(ident().map_with_span(|id, span| (id, span)))
        .then(
            expr.clone()
                .delimited_by(just(Token::ParenOpen), just(Token::ParenClose)),
        )
        .then(
            (just(Token::Implements)
                .ignore_then(ident_path().separated_by(just(Token::Comma)).at_least(1)))
            .or_not(),
        )
        .then(
            (fn_sig(expr).then_ignore(just(Token::Semi)))
                .repeated()
                .delimited_by(just(Token::BraceOpen), just(Token::BraceClose)),
        )
        .map(
            |((((name, name_span), id), interfaces), functions)| ast::Decl::Contract {
                name,
                id,
                interfaces: interfaces.unwrap_or_default(),
                functions,
                name_span,
            },
        )
        .boxed()
}

fn code_block_expr<'sc>(
    expr: impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone + 'sc,
) -> impl Parser<Token<'sc>, ast::Block, Error = ParseError<'sc>> + Clone {
    let code_block_body = choice((
        let_decl(expr.clone()),
        state_decl(expr.clone()),
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

fn prefix_unary_op<'sc>(
    expr: impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone + 'sc,
) -> impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone {
    choice((
        just(Token::Plus).to(expr::UnaryOp::Pos),
        just(Token::Minus).to(expr::UnaryOp::Neg),
        just(Token::Bang).to(expr::UnaryOp::Not),
    ))
    .then(expr)
    .map(|(op, expr)| ast::Expr::UnaryOp {
        op,
        expr: Box::new(expr),
    })
    .boxed()
}

fn if_expr<'sc>(
    expr: impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone + 'sc,
) -> impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone {
    let then_block = code_block_expr(expr.clone());
    let else_block = just(Token::Else).ignore_then(code_block_expr(expr.clone()));

    just(Token::If)
        .ignore_then(expr)
        .then(then_block)
        .then(else_block)
        .map(|((condition, then_block), else_block)| ast::Expr::If {
            condition: Box::new(condition),
            then_block,
            else_block,
        })
        .boxed()
}

fn cond_expr<'sc>(
    expr: impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone + 'sc,
) -> impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone {
    let cond_branch = expr
        .clone()
        .then_ignore(just(Token::HeavyArrow))
        .then(expr.clone())
        .then_ignore(just(Token::Comma))
        .map(|(condition, result)| expr::CondBranch {
            condition: Box::new(condition),
            result: Box::new(result),
        })
        .boxed();

    let else_branch = just(Token::Else)
        .ignore_then(just(Token::HeavyArrow))
        .ignore_then(expr)
        .boxed();

    let body = cond_branch
        .repeated()
        .then(else_branch)
        .then_ignore(just(Token::Comma).or_not())
        .delimited_by(just(Token::BraceOpen), just(Token::BraceClose))
        .boxed();

    just(Token::Cond)
        .ignore_then(body)
        .map(|(branches, else_result)| ast::Expr::Cond {
            branches,
            else_result: Box::new(else_result),
        })
        .boxed()
}

fn expr<'sc>() -> impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone {
    recursive(|expr| {
        let call_args = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .delimited_by(just(Token::ParenOpen), just(Token::ParenClose))
            .boxed();

        let call = ident_path()
            .then(call_args.clone())
            .map(|(name, args)| ast::Expr::Call { name, args })
            .boxed();

        let array_elements = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .delimited_by(just(Token::BracketOpen), just(Token::BracketClose))
            .boxed();

        let array = array_elements
            .validate(|array_elements, span, emit| {
                if array_elements.is_empty() {
                    emit(ParseError::EmptyArrayExpr { span })
                }
                array_elements
            })
            .map(ast::Expr::Array)
            .boxed();

        let tuple_fields = (ident().then_ignore(just(Token::Colon)))
            .or_not()
            .then(expr.clone())
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .delimited_by(just(Token::BraceOpen), just(Token::BraceClose))
            .boxed();

        let tuple = tuple_fields
            .validate(|tuple_fields, span, emit| {
                if tuple_fields.is_empty() {
                    emit(ParseError::EmptyTupleExpr { span })
                }
                tuple_fields
            })
            .map(ast::Expr::Tuple)
            .boxed();

        let parens = expr
            .clone()
            .delimited_by(just(Token::ParenOpen), just(Token::ParenClose))
            .boxed();

        let atom = choice((
            immediate().map(ast::Expr::Immediate),
            prefix_unary_op(expr.clone()),
            code_block_expr(expr.clone()).map(ast::Expr::Block),
            if_expr(expr.clone()),
            cond_expr(expr.clone()),
            call,
            array,
            tuple,
            parens,
            ident_path().map(ast::Expr::Ident),
        ))
        .boxed();

        // This order enforces the procedence rules in Yurt expressions
        let suffix_unary_op = suffix_unary_op(atom);
        let array_element_access = array_element_access(suffix_unary_op, expr.clone());
        let tuple_field_access = tuple_field_access(array_element_access);
        let cast = cast(tuple_field_access, expr.clone());
        let in_expr = in_expr(cast, expr.clone());
        let multiplicative_op = multiplicative_op(in_expr);
        let additive_op = additive_op(multiplicative_op);
        let comparison_op = comparison_op(additive_op);
        let and = and_or_op(
            Token::DoubleAmpersand,
            expr::BinaryOp::LogicalAnd,
            comparison_op,
        );
        let or = and_or_op(Token::DoublePipe, expr::BinaryOp::LogicalOr, and);

        or.boxed()
    })
}

fn suffix_unary_op<'sc, P>(
    parser: P,
) -> impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone
where
    P: Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone + 'sc,
{
    parser
        .then((just(Token::SingleQuote).to(expr::UnaryOp::NextState)).repeated())
        .foldl(|expr, prime| ast::Expr::UnaryOp {
            op: prime,
            expr: Box::new(expr),
        })
        .boxed()
}

fn array_element_access<'sc, P>(
    parser: P,
    expr: impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone + 'sc,
) -> impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone
where
    P: Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone + 'sc,
{
    parser
        .then(
            expr.delimited_by(just(Token::BracketOpen), just(Token::BracketClose))
                .repeated(),
        )
        .map(|(expr, indices)| (indices, expr))
        .foldr(|index, expr| ast::Expr::ArrayElementAccess {
            array: Box::new(expr),
            index: Box::new(index),
        })
        .boxed()
}

fn tuple_field_access<'sc, P>(
    parser: P,
) -> impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone
where
    P: Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone + 'sc,
{
    let indices = filter_map(|span, token| match &token {
        // Field access with an identifier
        Token::Ident(ident) => Ok(vec![Either::Right((*ident).to_owned())]),

        // Field access with an integer
        Token::IntLiteral(num_str) => num_str
            .parse::<usize>()
            .map(|index| vec![Either::Left(index)])
            .map_err(|_| ParseError::InvalidIntegerTupleIndex {
                span,
                index: num_str,
            }),

        // If the next token is of the form `<int>.<int>` which, to the lexer, looks like a real,
        // break it apart manually.
        Token::RealLiteral(num_str) => {
            match Regex::new(r"[0-9]+\.[0-9]+")
                .expect("valid regex")
                .captures(num_str)
            {
                Some(_) => {
                    // Collect the spans for the two integers
                    let dot_index = num_str
                        .chars()
                        .position(|c| c == '.')
                        .expect("guaranteed by regex");
                    let spans = [
                        span.start..span.start + dot_index,
                        span.start + dot_index + 1..span.end,
                    ];

                    // Split at `.` then collect the two indices as `usize`. Report errors as
                    // needed
                    num_str
                        .split('.')
                        .zip(spans.iter())
                        .map(|(index, span)| {
                            index
                                .parse::<usize>()
                                .map_err(|_| ParseError::InvalidIntegerTupleIndex {
                                    span: span.clone(),
                                    index,
                                })
                                .map(Either::Left)
                        })
                        .collect::<Result<Vec<Either<usize, String>>, _>>()
                }
                None => Err(ParseError::InvalidTupleIndex { span, index: token }),
            }
        }
        _ => Err(ParseError::InvalidTupleIndex { span, index: token }),
    })
    .boxed();

    parser
        .then(just(Token::Dot).ignore_then(indices).repeated().flatten())
        .foldl(|expr, field| ast::Expr::TupleFieldAccess {
            tuple: Box::new(expr),
            field,
        })
        .boxed()
}

fn cast<'sc, P>(
    parser: P,
    expr: impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone + 'sc,
) -> impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone
where
    P: Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone + 'sc,
{
    parser
        .then(just(Token::As).ignore_then(type_(expr)).repeated())
        .foldl(|value, ty| ast::Expr::Cast {
            value: Box::new(value),
            ty: Box::new(ty),
        })
        .boxed()
}

fn in_expr<'sc, P>(
    parser: P,
    expr: impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone + 'sc,
) -> impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone
where
    P: Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone + 'sc,
{
    parser
        .then(just(Token::In).ignore_then(expr).repeated())
        .foldl(|value, collection| ast::Expr::In {
            value: Box::new(value),
            collection: Box::new(collection),
        })
        .boxed()
}

fn multiplicative_op<'sc, P>(
    parser: P,
) -> impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone
where
    P: Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone + 'sc,
{
    parser
        .clone()
        .then(
            just(Token::Star)
                .to(expr::BinaryOp::Mul)
                .or(just(Token::Div).to(expr::BinaryOp::Div))
                .or(just(Token::Mod).to(expr::BinaryOp::Mod))
                .then(parser)
                .repeated(),
        )
        .foldl(|lhs, (op, rhs)| ast::Expr::BinaryOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
        .boxed()
}

fn additive_op<'sc, P>(
    parser: P,
) -> impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone
where
    P: Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone + 'sc,
{
    parser
        .clone()
        .then(
            just(Token::Plus)
                .to(expr::BinaryOp::Add)
                .or(just(Token::Minus).to(expr::BinaryOp::Sub))
                .then(parser)
                .repeated(),
        )
        .foldl(|lhs, (op, rhs)| ast::Expr::BinaryOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
        .boxed()
}

fn comparison_op<'sc, P>(
    parser: P,
) -> impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone
where
    P: Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone + 'sc,
{
    parser
        .clone()
        .then(
            choice((
                just(Token::Lt).to(expr::BinaryOp::LessThan),
                just(Token::Gt).to(expr::BinaryOp::GreaterThan),
                just(Token::LtEq).to(expr::BinaryOp::LessThanOrEqual),
                just(Token::GtEq).to(expr::BinaryOp::GreaterThanOrEqual),
                just(Token::EqEq).to(expr::BinaryOp::Equal),
                just(Token::NotEq).to(expr::BinaryOp::NotEqual),
            ))
            .then(parser)
            .repeated(),
        )
        .foldl(|lhs, (op, rhs)| ast::Expr::BinaryOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
        .boxed()
}

fn and_or_op<'sc, P>(
    token: Token<'sc>,
    logical_op: expr::BinaryOp,
    parser: P,
) -> impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone
where
    P: Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone + 'sc,
{
    parser
        .clone()
        .then(just(token).to(logical_op).then(parser).repeated())
        .foldl(|lhs, (op, rhs)| ast::Expr::BinaryOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
        .boxed()
}

fn ident<'sc>() -> impl Parser<Token<'sc>, String, Error = ParseError<'sc>> + Clone {
    filter_map(|span, token| match token {
        // Accept detected identifier. The lexer makes sure that these are not keywords.
        Token::Ident(id) => Ok(id.to_owned()),

        // Tokens that represent keywords are not allowed
        _ if KEYWORDS.contains(&token) => Err(ParseError::KeywordAsIdent {
            span,
            keyword: token,
        }),

        // Other tokens are not allowed either
        _ => Err(ParseError::ExpectedFound {
            span,
            expected: vec![],
            found: Some(token),
        }),
    })
}

fn ident_path<'sc>() -> impl Parser<Token<'sc>, ast::Ident, Error = ParseError<'sc>> + Clone {
    let relative_path = ident().then((just(Token::DoubleColon).ignore_then(ident())).repeated());
    just(Token::DoubleColon)
        .or_not()
        .then(relative_path)
        .map(|(pre_colons, (id, mut path))| {
            path.insert(0, id);
            ast::Ident {
                path,
                is_absolute: pre_colons.is_some(),
            }
        })
        .boxed()
}

fn type_<'sc>(
    expr: impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone + 'sc,
) -> impl Parser<Token<'sc>, ast::Type, Error = ParseError<'sc>> + Clone {
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
            ident_path().map(ast::Type::CustomType),
        ))
        .boxed();

        // Multi-dimensional arrays have their innermost dimension on the far right. Hence, we need
        // a `foldr` here instead of a `foldl`. For example, `int[3][5]` is actually an array of
        // size 3 that contains arrays of size 5 of `int`s.
        type_atom
            .clone()
            .then(
                expr.delimited_by(just(Token::BracketOpen), just(Token::BracketClose))
                    .repeated(),
            )
            .map(|(type_atom, array_range)| (array_range, type_atom))
            .foldr(|range, ty| ast::Type::Array {
                ty: Box::new(ty),
                range,
            })
            .boxed()
    })
}

fn immediate<'sc>() -> impl Parser<Token<'sc>, expr::Immediate, Error = ParseError<'sc>> + Clone {
    let integer_parser = |num_str: &str| {
        use num_traits::Num;
        let (radix, offset) = match num_str.chars().nth(1) {
            Some('b') => (2, 2),
            Some('x') => (16, 2),
            _ => (10, 0),
        };

        // Try to parse as an i64 first
        i64::from_str_radix(&num_str[offset..], radix)
            .map(expr::Immediate::Int)
            .or_else(|_| {
                // Try a big-int if that fails and return an expr::Immedate::BigInt.  The BigInt
                // FromStr::from_str() isn't smart about radices though.
                num_bigint::BigInt::from_str_radix(&num_str[offset..], radix)
                    .map(expr::Immediate::BigInt)
            })
            .unwrap()
    };

    select! {
        Token::RealLiteral(num_str) => expr::Immediate::Real(num_str.parse().unwrap()),
        Token::IntLiteral(num_str) => integer_parser(num_str),
        Token::True => expr::Immediate::Bool(true),
        Token::False => expr::Immediate::Bool(false),
        Token::StringLiteral(str_val) => expr::Immediate::String(str_val),
    }
}
