use crate::{
    ast,
    error::{print_on_failure, CompileError},
    lexer::{self, Token},
};
use chumsky::{prelude::*, Stream};
use std::{fs::read_to_string, path::Path};

type Ast = Vec<ast::Decl>;

pub(super) fn parse_path_to_ast(path: &Path, filename: &str) -> anyhow::Result<Ast> {
    parse_str_to_ast(&read_to_string(path)?, filename)
}

/// Parse `source` and returns an AST. Upon failure, print all compile errors and exit.
fn parse_str_to_ast(source: &str, filename: &str) -> anyhow::Result<Ast> {
    match parse_str_to_ast_inner(source) {
        Ok(ast) => Ok(ast),
        Err(errors) => {
            print_on_failure(filename, source, &errors);
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
    match parser().parse(token_stream) {
        Ok(_) if !errors.is_empty() => Err(errors),
        Err(parsing_errors) => {
            let parsing_errors: Vec<_> = parsing_errors
                .iter()
                .map(|error| CompileError::ParseError {
                    error: error.clone(),
                })
                .collect();

            errors.extend(parsing_errors);
            Err(errors)
        }
        Ok(ast) => Ok(ast),
    }
}

fn parser<'sc>() -> impl Parser<Token<'sc>, Ast, Error = Simple<Token<'sc>>> + Clone {
    choice((value_decl(), constraint_decl(), solve_decl()))
        .then_ignore(just(Token::Semi))
        .repeated()
        .then_ignore(end())
}

fn value_decl<'sc>() -> impl Parser<Token<'sc>, ast::Decl, Error = Simple<Token<'sc>>> + Clone {
    let type_spec = just(Token::Colon).ignore_then(type_());
    just(Token::Let)
        .ignore_then(ident())
        .then(type_spec.or_not())
        .then_ignore(just(Token::Eq))
        .then(expr())
        .map(|((name, ty), init)| ast::Decl::Value { name, ty, init })
}

fn constraint_decl<'sc>() -> impl Parser<Token<'sc>, ast::Decl, Error = Simple<Token<'sc>>> + Clone
{
    just(Token::Constraint)
        .ignore_then(expr())
        .map(ast::Decl::Constraint)
}

fn solve_decl<'sc>() -> impl Parser<Token<'sc>, ast::Decl, Error = Simple<Token<'sc>>> + Clone {
    let solve_satisfy = just(Token::Satisfy).to(ast::SolveFunc::Satisfy);
    let solve_minimize = just(Token::Minimize)
        .ignore_then(ident())
        .map(ast::SolveFunc::Minimize);
    let solve_maximize = just(Token::Maximize)
        .ignore_then(ident())
        .map(ast::SolveFunc::Maximize);

    just(Token::Solve)
        .ignore_then(choice((solve_satisfy, solve_minimize, solve_maximize)))
        .map(ast::Decl::Solve)
}

fn expr<'sc>() -> impl Parser<Token<'sc>, ast::Expr, Error = Simple<Token<'sc>>> + Clone {
    let imm = immediate().map(ast::Expr::Immediate);
    let id = ident().map(ast::Expr::Ident);
    let atom = imm.or(id);

    let multiplicative_op = atom
        .clone()
        .then(
            just(Token::Star)
                .to(ast::BinaryOp::Mul)
                .then(atom)
                .repeated(),
        )
        .foldl(|lhs, (op, rhs)| ast::Expr::BinaryOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        });

    // Comparison op.
    multiplicative_op
        .clone()
        .then(
            just(Token::Gt)
                .to(ast::BinaryOp::GreaterThan)
                .or(just(Token::Lt).to(ast::BinaryOp::LessThan))
                .then(multiplicative_op)
                .repeated(),
        )
        .foldl(|lhs, (op, rhs)| ast::Expr::BinaryOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
}

fn ident<'sc>() -> impl Parser<Token<'sc>, ast::Ident, Error = Simple<Token<'sc>>> + Clone {
    select! { Token::Ident(id) => ast::Ident(id.to_owned()) }
}

fn type_<'sc>() -> impl Parser<Token<'sc>, ast::Type, Error = Simple<Token<'sc>>> + Clone {
    just(Token::Real).to(ast::Type::Real)
}

fn immediate<'sc>() -> impl Parser<Token<'sc>, ast::Immediate, Error = Simple<Token<'sc>>> + Clone {
    select! { Token::Number(num_str) => ast::Immediate::Real(num_str.parse().unwrap()) }
}

// To-do tests:
// - out of order decls.
// - block - constraints { .. }
// - all ops
// - all satisfies
// - all floats
// - hex
// - parens in expressions
// - unary negation, boolean not
//
// - errors (currently Simple, need to switch to Rich)

#[test]
fn parse_00() {
    let src = r#"
let low_val: real = 1.23;
let high_val = 4.56;        // Implicit type.

// Here's the constraints.
constraint mid > low_val * 2;
constraint mid < high_val;

solve minimize mid;
"#;

    // This is still a bit crude for larger ASTs.
    let res = parse_str_to_ast_inner(src);
    assert_eq!(format!("{res:?}"),
        [ r#"Ok(["#
        , r#"Value { name: Ident("low_val"), ty: Some(Real), init: Immediate(Real(1.23)) }, "#
        , r#"Value { name: Ident("high_val"), ty: None, init: Immediate(Real(4.56)) }, "#
        , r#"Constraint(BinaryOp { "#
            , r#"op: GreaterThan, "#
            , r#"lhs: Ident(Ident("mid")), "#
            , r#"rhs: BinaryOp { op: Mul, lhs: Ident(Ident("low_val")), rhs: Immediate(Real(2.0)) } "#
        , r#"}), "#
        , r#"Constraint(BinaryOp { op: LessThan, lhs: Ident(Ident("mid")), rhs: Ident(Ident("high_val")) }), "#
        , r#"Solve(Minimize(Ident("mid")))"#
        , r#"])"#
        ].concat());
}

#[test]
fn parse_with_errors() {
    let src = r#"
let low_val: bad = 1.23;
"#;

    let res = parse_str_to_ast_inner(src);
    assert!(res.is_err());
    let errs = res.unwrap_err();
    assert!(matches!(errs[0], CompileError::ParseError { .. }));
}
