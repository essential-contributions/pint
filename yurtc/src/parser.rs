use std::{fs::read_to_string, path::Path};

use chumsky::{prelude::*, Stream};

use crate::{
    ast::*,
    lexer::{self, Token},
};

type Ast = Vec<AstDecl>;

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

fn parser() -> impl Parser<Token, Ast, Error = Simple<Token>> + Clone {
    choice((value_decl(), constraint_decl(), solve_decl()))
        .then_ignore(just(Token::Semi))
        .repeated()
        .then_ignore(end())
}

fn value_decl() -> impl Parser<Token, AstDecl, Error = Simple<Token>> + Clone {
    let type_spec = just(Token::Colon).ignore_then(type_());
    just(Token::Let)
        .ignore_then(ident())
        .then(type_spec.or_not())
        .then_ignore(just(Token::Eq))
        .then(expr())
        .map(|((name, ty), init)| AstDecl::Value { name, ty, init })
}

fn constraint_decl() -> impl Parser<Token, AstDecl, Error = Simple<Token>> + Clone {
    just(Token::Constraint)
        .ignore_then(expr())
        .map(AstDecl::Constraint)
}

fn solve_decl() -> impl Parser<Token, AstDecl, Error = Simple<Token>> + Clone {
    let solve_satisfy = just(Token::Satisfy).to(AstSolveFunc::Satisfy);
    let solve_minimize = just(Token::Minimize)
        .ignore_then(ident())
        .map(AstSolveFunc::Minimize);
    let solve_maximize = just(Token::Maximize)
        .ignore_then(ident())
        .map(AstSolveFunc::Maximize);

    just(Token::Solve)
        .ignore_then(choice((solve_satisfy, solve_minimize, solve_maximize)))
        .map(AstDecl::Solve)
}

fn expr() -> impl Parser<Token, AstExpr, Error = Simple<Token>> + Clone {
    let imm = immediate().map(AstExpr::Immediate);
    let id = ident().map(AstExpr::Ident);
    let atom = imm.or(id);

    let multiplicative_op = atom
        .clone()
        .then(just(Token::Star).to(AstBinaryOp::Mul).then(atom).repeated())
        .foldl(|lhs, (op, rhs)| AstExpr::BinaryOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        });

    // Comparison op.
    multiplicative_op
        .clone()
        .then(
            just(Token::Gt)
                .to(AstBinaryOp::GreaterThan)
                .or(just(Token::Lt).to(AstBinaryOp::LessThan))
                .then(multiplicative_op)
                .repeated(),
        )
        .foldl(|lhs, (op, rhs)| AstExpr::BinaryOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
}

fn ident() -> impl Parser<Token, AstIdent, Error = Simple<Token>> + Clone {
    select! { Token::Ident(id) => AstIdent(id) }
}

fn type_() -> impl Parser<Token, AstType, Error = Simple<Token>> + Clone {
    just(Token::Real).to(AstType::Real)
}

fn immediate() -> impl Parser<Token, AstImmediate, Error = Simple<Token>> + Clone {
    select! { Token::Number(num_str) => AstImmediate::Real(num_str.parse().unwrap()) }
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
            AstDecl::Value {
                name: AstIdent("low_val".to_owned()),
                ty: Some(AstType::Real),
                init: AstExpr::Immediate(AstImmediate::Real(1.23))
            },
            AstDecl::Value {
                name: AstIdent("high_val".to_owned()),
                ty: None,
                init: AstExpr::Immediate(AstImmediate::Real(4.56))
            },
            AstDecl::Constraint(AstExpr::BinaryOp {
                op: AstBinaryOp::GreaterThan,
                lhs: Box::new(AstExpr::Ident(AstIdent("mid".to_string()))),
                rhs: Box::new(AstExpr::BinaryOp {
                    op: AstBinaryOp::Mul,
                    lhs: Box::new(AstExpr::Ident(AstIdent("low_val".to_string()))),
                    rhs: Box::new(AstExpr::Immediate(AstImmediate::Real(2.0)))
                })
            }),
            AstDecl::Constraint(AstExpr::BinaryOp {
                op: AstBinaryOp::LessThan,
                lhs: Box::new(AstExpr::Ident(AstIdent("mid".to_string()))),
                rhs: Box::new(AstExpr::Ident(AstIdent("high_val".to_string()))),
            }),
            AstDecl::Solve(AstSolveFunc::Minimize(AstIdent("mid".to_string()))),
        ])
        .all(|(a, b)| a == b));
}
