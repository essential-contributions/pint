use std::{fs::read_to_string, path::Path};

use chumsky::{prelude::*, Stream};

use crate::{
    ast,
    lexer::{self, Token},
};

type Ast = Vec<ast::Decl>;

pub(super) fn parse_path_to_ast(path: &Path) -> anyhow::Result<Ast> {
    parse_str_to_ast(&read_to_string(path)?)
}

fn parse_str_to_ast(source: &str) -> anyhow::Result<Ast> {
    // Lex the input into tokens and spans.
    let tokens = lexer::lex(source).map_err(|_| anyhow::anyhow!("TODO lexer errors"))?;

    // Provide a token stream
    let eoi_span = source.len()..source.len();
    let token_stream = Stream::from_iter(eoi_span, tokens.into_iter());

    parser().parse(token_stream).map_err(|errs| {
        for err in errs {
            println!("parse error: {:?}", err)
        }
        anyhow::anyhow!("fixme")
    })
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

    let res = parse_str_to_ast(src);
    assert!(res.is_ok());
    let res = res.unwrap();
    assert_eq!(res.len(), 5);
    assert!(res
        .into_iter()
        .zip([
            ast::Decl::Value {
                name: ast::Ident("low_val".to_owned()),
                ty: Some(ast::Type::Real),
                init: ast::Expr::Immediate(ast::Immediate::Real(1.23))
            },
            ast::Decl::Value {
                name: ast::Ident("high_val".to_owned()),
                ty: None,
                init: ast::Expr::Immediate(ast::Immediate::Real(4.56))
            },
            ast::Decl::Constraint(ast::Expr::BinaryOp {
                op: ast::BinaryOp::GreaterThan,
                lhs: Box::new(ast::Expr::Ident(ast::Ident("mid".to_string()))),
                rhs: Box::new(ast::Expr::BinaryOp {
                    op: ast::BinaryOp::Mul,
                    lhs: Box::new(ast::Expr::Ident(ast::Ident("low_val".to_string()))),
                    rhs: Box::new(ast::Expr::Immediate(ast::Immediate::Real(2.0)))
                })
            }),
            ast::Decl::Constraint(ast::Expr::BinaryOp {
                op: ast::BinaryOp::LessThan,
                lhs: Box::new(ast::Expr::Ident(ast::Ident("mid".to_string()))),
                rhs: Box::new(ast::Expr::Ident(ast::Ident("high_val".to_string()))),
            }),
            ast::Decl::Solve(ast::SolveFunc::Minimize(ast::Ident("mid".to_string()))),
        ])
        .all(|(a, b)| a == b));
}
