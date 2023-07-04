use crate::{
    ast,
    error::{print_on_failure, CompileError, ParseError},
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
        var_decl(expr()),
        let_decl(expr()),
        constraint_decl(expr()),
        solve_decl(),
        fn_decl(expr()),
    ))
    .repeated()
    .then_ignore(end())
}

fn var_decl<'sc>(
    expr: impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone,
) -> impl Parser<Token<'sc>, ast::Decl, Error = ParseError<'sc>> + Clone {
    let type_spec = just(Token::Colon).ignore_then(type_());
    let init = just(Token::Eq).ignore_then(expr);
    just(Token::Var)
        .ignore_then(ident())
        .then(type_spec.or_not())
        .then(init.or_not())
        .then_ignore(just(Token::Semi))
        .map(|((name, ty), init)| ast::Decl::Var(ast::VarStatement { name, ty, init }))
}

fn let_decl<'sc>(
    expr: impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone,
) -> impl Parser<Token<'sc>, ast::Decl, Error = ParseError<'sc>> + Clone {
    let type_spec = just(Token::Colon).ignore_then(type_());
    let init = just(Token::Eq).ignore_then(expr);
    just(Token::Let)
        .ignore_then(ident())
        .then(type_spec.or_not())
        .then(init)
        .then_ignore(just(Token::Semi))
        .map(|((name, ty), init)| ast::Decl::Let(ast::LetStatement { name, ty, init }))
}

fn constraint_decl<'sc>(
    expr: impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone,
) -> impl Parser<Token<'sc>, ast::Decl, Error = ParseError<'sc>> + Clone {
    just(Token::Constraint)
        .ignore_then(expr)
        .then_ignore(just(Token::Semi))
        .map(ast::Decl::Constraint)
}

fn solve_decl<'sc>() -> impl Parser<Token<'sc>, ast::Decl, Error = ParseError<'sc>> + Clone {
    let solve_satisfy = just(Token::Satisfy).to(ast::SolveFunc::Satisfy);
    let solve_minimize = just(Token::Minimize)
        .ignore_then(ident())
        .map(ast::SolveFunc::Minimize);
    let solve_maximize = just(Token::Maximize)
        .ignore_then(ident())
        .map(ast::SolveFunc::Maximize);

    just(Token::Solve)
        .ignore_then(choice((solve_satisfy, solve_minimize, solve_maximize)))
        .then_ignore(just(Token::Semi))
        .map(ast::Decl::Solve)
}

fn fn_decl<'sc>(
    expr: impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone,
) -> impl Parser<Token<'sc>, ast::Decl, Error = ParseError<'sc>> + Clone {
    let type_spec = just(Token::Colon).ignore_then(type_());

    let params = ident()
        .then(type_spec)
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .delimited_by(just(Token::ParenOpen), just(Token::ParenClose));

    let return_type = just(Token::Arrow).ignore_then(type_());

    just(Token::Fn)
        .ignore_then(ident())
        .then(params)
        .then(return_type)
        .then(code_block_expr(expr))
        .map(|(((name, params), return_type), body)| ast::Decl::Fn {
            name,
            params,
            return_type,
            body,
        })
}

fn code_block_expr<'sc>(
    expr: impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone,
) -> impl Parser<Token<'sc>, ast::Block, Error = ParseError<'sc>> + Clone {
    let code_block_body = choice((
        var_decl(expr.clone()),
        let_decl(expr.clone()),
        constraint_decl(expr.clone()),
    ))
    .repeated()
    .then(expr);

    code_block_body
        .delimited_by(just(Token::BraceOpen), just(Token::BraceClose))
        .map(|(statements, expr)| ast::Block {
            statements,
            final_expr: Box::new(expr),
        })
}

fn unary_op<'sc>(
    expr: impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone,
) -> impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone {
    choice((
        just(Token::Plus).to(ast::UnaryOp::Pos),
        just(Token::Minus).to(ast::UnaryOp::Neg),
        just(Token::Bang).to(ast::UnaryOp::Not),
    ))
    .then(expr)
    .map(|(op, expr)| ast::Expr::UnaryOp {
        op,
        expr: Box::new(expr),
    })
}

fn if_expr<'sc>(
    expr: impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone,
) -> impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone {
    let then_block = code_block_expr(expr.clone());
    let else_block = just(Token::Else).ignore_then(code_block_expr(expr.clone()));

    just(Token::If)
        .ignore_then(expr)
        .then(then_block)
        .then(else_block)
        .map(|((condition, then_block), else_block)| {
            ast::Expr::If(ast::IfExpr {
                condition: Box::new(condition),
                then_block,
                else_block,
            })
        })
}

fn expr<'sc>() -> impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone {
    recursive(|expr| {
        let args = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .delimited_by(just(Token::ParenOpen), just(Token::ParenClose));

        let call = ident()
            .then(args)
            .map(|(name, args)| ast::Expr::Call { name, args });

        let atom = choice((
            immediate().map(ast::Expr::Immediate),
            unary_op(expr.clone()),
            code_block_expr(expr.clone()).map(ast::Expr::Block),
            if_expr(expr.clone()),
            call,
            ident().map(ast::Expr::Ident),
        ));

        comparison_op(additive_op(multiplicative_op(atom)))
    })
}

fn multiplicative_op<'sc, P>(
    parser: P,
) -> impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone
where
    P: Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone,
{
    parser
        .clone()
        .then(
            just(Token::Star)
                .to(ast::BinaryOp::Mul)
                .or(just(Token::Div).to(ast::BinaryOp::Div))
                .or(just(Token::Mod).to(ast::BinaryOp::Mod))
                .then(parser)
                .repeated(),
        )
        .foldl(|lhs, (op, rhs)| ast::Expr::BinaryOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
}

fn additive_op<'sc, P>(
    parser: P,
) -> impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone
where
    P: Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone,
{
    parser
        .clone()
        .then(
            just(Token::Plus)
                .to(ast::BinaryOp::Add)
                .or(just(Token::Minus).to(ast::BinaryOp::Sub))
                .then(parser)
                .repeated(),
        )
        .foldl(|lhs, (op, rhs)| ast::Expr::BinaryOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
}

fn comparison_op<'sc, P>(
    parser: P,
) -> impl Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone
where
    P: Parser<Token<'sc>, ast::Expr, Error = ParseError<'sc>> + Clone,
{
    parser
        .clone()
        .then(
            just(Token::Lt)
                .to(ast::BinaryOp::LessThan)
                .or(just(Token::Gt).to(ast::BinaryOp::GreaterThan))
                .or(just(Token::LtEq).to(ast::BinaryOp::LessThanOrEqual))
                .or(just(Token::GtEq).to(ast::BinaryOp::GreaterThanOrEqual))
                .or(just(Token::EqEq).to(ast::BinaryOp::Equal))
                .or(just(Token::NotEq).to(ast::BinaryOp::NotEqual))
                .then(parser)
                .repeated(),
        )
        .foldl(|lhs, (op, rhs)| ast::Expr::BinaryOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
}

fn ident<'sc>() -> impl Parser<Token<'sc>, ast::Ident, Error = ParseError<'sc>> + Clone {
    select! { Token::Ident(id) => ast::Ident(id.to_owned()) }
}

fn type_<'sc>() -> impl Parser<Token<'sc>, ast::Type, Error = ParseError<'sc>> + Clone {
    choice((
        just(Token::Real).to(ast::Type::Real),
        just(Token::Int).to(ast::Type::Int),
        just(Token::Bool).to(ast::Type::Bool),
        just(Token::String).to(ast::Type::String),
    ))
}

fn immediate<'sc>() -> impl Parser<Token<'sc>, ast::Immediate, Error = ParseError<'sc>> + Clone {
    select! {
        Token::RealLiteral(num_str) => ast::Immediate::Real(num_str.parse().unwrap()),
        Token::IntLiteral(num_str) => ast::Immediate::Int(num_str.parse().unwrap()),
        Token::True => ast::Immediate::Bool(true),
        Token::False => ast::Immediate::Bool(false),
        Token::StringLiteral(str_val) => ast::Immediate::String(str_val)
    }
}

// To-do tests:
// - block - constraints { .. }
// - parens in expressions
// - unary negation, boolean not
// - can't have keywords as idents

#[cfg(test)]
macro_rules! run_parser {
    ($parser: expr, $source: expr) => {{
        let (toks, errs) = lexer::lex($source);
        assert!(errs.is_empty());
        let token_stream = Stream::from_iter($source.len()..$source.len(), toks.into_iter());
        match $parser.parse(token_stream) {
            Ok(ast) => format!("{:?}", ast),
            Err(errors) => format!(
                "{}",
                // Print each error on one line. For each error, start with the span.
                errors.iter().fold(String::new(), |acc, error| {
                    let span = CompileError::Parse {
                        error: error.clone(),
                    }
                    .span();
                    format!("{}@{}..{}: {}\n", acc, span.start, span.end(), error)
                })
            ),
        }
    }};
}

#[cfg(test)]
fn check(actual: &str, expect: expect_test::Expect) {
    expect.assert_eq(actual);
}

#[test]
fn let_decls() {
    check(
        &run_parser!(let_decl(expr()), "let blah = 1.0;"),
        expect_test::expect![[
            r#"Let(LetStatement { name: Ident("blah"), ty: None, init: Immediate(Real(1.0)) })"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: real = 1.0;"),
        expect_test::expect![[
            r#"Let(LetStatement { name: Ident("blah"), ty: Some(Real), init: Immediate(Real(1.0)) })"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: real"),
        expect_test::expect![[r#"
            @14..14: found end of input but expected "="
        "#]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah = 1;"),
        expect_test::expect![[
            r#"Let(LetStatement { name: Ident("blah"), ty: None, init: Immediate(Int(1)) })"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: int = 1;"),
        expect_test::expect![[
            r#"Let(LetStatement { name: Ident("blah"), ty: Some(Int), init: Immediate(Int(1)) })"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: int"),
        expect_test::expect![[r#"
            @13..13: found end of input but expected "="
        "#]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah = true;"),
        expect_test::expect![[
            r#"Let(LetStatement { name: Ident("blah"), ty: None, init: Immediate(Bool(true)) })"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: bool = false;"),
        expect_test::expect![[
            r#"Let(LetStatement { name: Ident("blah"), ty: Some(Bool), init: Immediate(Bool(false)) })"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), "let blah: bool"),
        expect_test::expect![[r#"
            @14..14: found end of input but expected "="
        "#]],
    );
    check(
        &run_parser!(let_decl(expr()), r#"let blah = "hello";"#),
        expect_test::expect![[
            r#"Let(LetStatement { name: Ident("blah"), ty: None, init: Immediate(String("hello")) })"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), r#"let blah: string = "hello";"#),
        expect_test::expect![[
            r#"Let(LetStatement { name: Ident("blah"), ty: Some(String), init: Immediate(String("hello")) })"#
        ]],
    );
    check(
        &run_parser!(let_decl(expr()), r#"let blah: string"#),
        expect_test::expect![[r#"
            @16..16: found end of input but expected "="
        "#]],
    );
}

#[test]
fn var_decls() {
    check(
        &run_parser!(var_decl(expr()), "var blah;"),
        expect_test::expect![[
            r#"Var(VarStatement { name: Ident("blah"), ty: None, init: None })"#
        ]],
    );
    check(
        &run_parser!(var_decl(expr()), "var blah = 1.0;"),
        expect_test::expect![[
            r#"Var(VarStatement { name: Ident("blah"), ty: None, init: Some(Immediate(Real(1.0))) })"#
        ]],
    );
    check(
        &run_parser!(var_decl(expr()), "var blah: real = 1.0;"),
        expect_test::expect![[
            r#"Var(VarStatement { name: Ident("blah"), ty: Some(Real), init: Some(Immediate(Real(1.0))) })"#
        ]],
    );
    check(
        &run_parser!(var_decl(expr()), "var blah: real;"),
        expect_test::expect![[
            r#"Var(VarStatement { name: Ident("blah"), ty: Some(Real), init: None })"#
        ]],
    );
    check(
        &run_parser!(var_decl(expr()), "var blah = 1;"),
        expect_test::expect![[
            r#"Var(VarStatement { name: Ident("blah"), ty: None, init: Some(Immediate(Int(1))) })"#
        ]],
    );
    check(
        &run_parser!(var_decl(expr()), "var blah: int = 1;"),
        expect_test::expect![[
            r#"Var(VarStatement { name: Ident("blah"), ty: Some(Int), init: Some(Immediate(Int(1))) })"#
        ]],
    );
    check(
        &run_parser!(var_decl(expr()), "var blah: int;"),
        expect_test::expect![[
            r#"Var(VarStatement { name: Ident("blah"), ty: Some(Int), init: None })"#
        ]],
    );
    check(
        &run_parser!(var_decl(expr()), "var blah = true;"),
        expect_test::expect![[
            r#"Var(VarStatement { name: Ident("blah"), ty: None, init: Some(Immediate(Bool(true))) })"#
        ]],
    );
    check(
        &run_parser!(var_decl(expr()), "var blah: bool = false;"),
        expect_test::expect![[
            r#"Var(VarStatement { name: Ident("blah"), ty: Some(Bool), init: Some(Immediate(Bool(false))) })"#
        ]],
    );
    check(
        &run_parser!(var_decl(expr()), "var blah: bool;"),
        expect_test::expect![[
            r#"Var(VarStatement { name: Ident("blah"), ty: Some(Bool), init: None })"#
        ]],
    );
    check(
        &run_parser!(var_decl(expr()), r#"var blah = "hello";"#),
        expect_test::expect![[
            r#"Var(VarStatement { name: Ident("blah"), ty: None, init: Some(Immediate(String("hello"))) })"#
        ]],
    );
    check(
        &run_parser!(var_decl(expr()), r#"var blah: string = "hello";"#),
        expect_test::expect![[
            r#"Var(VarStatement { name: Ident("blah"), ty: Some(String), init: Some(Immediate(String("hello"))) })"#
        ]],
    );
    check(
        &run_parser!(var_decl(expr()), r#"var blah: string;"#),
        expect_test::expect![[
            r#"Var(VarStatement { name: Ident("blah"), ty: Some(String), init: None })"#
        ]],
    );
}

#[test]
fn constraint_decls() {
    // Argument just needs to be any expression, as far as the parser is concerned.
    check(
        &run_parser!(constraint_decl(expr()), "constraint blah;"),
        expect_test::expect![[r#"Constraint(Ident(Ident("blah")))"#]],
    );
}

#[test]
fn solve_decls() {
    check(
        &run_parser!(solve_decl(), "solve satisfy;"),
        expect_test::expect!["Solve(Satisfy)"],
    );
    check(
        &run_parser!(solve_decl(), "solve minimize foo;"),
        expect_test::expect![[r#"Solve(Minimize(Ident("foo")))"#]],
    );
    check(
        &run_parser!(solve_decl(), "solve maximize foo;"),
        expect_test::expect![[r#"Solve(Maximize(Ident("foo")))"#]],
    );

    check(
        &run_parser!(solve_decl(), "solve world hunger;"),
        expect_test::expect![[r#"
            @6..11: found "world" but expected "maximize", "minimize",  or "satisfy"
        "#]],
    );
}

#[test]
fn exprs() {
    check(
        &run_parser!(expr(), "123"),
        expect_test::expect!["Immediate(Int(123))"],
    );
    check(
        &run_parser!(expr(), "foo"),
        expect_test::expect![[r#"Ident(Ident("foo"))"#]],
    );
    check(
        &run_parser!(expr(), "!a"),
        expect_test::expect![[r#"UnaryOp { op: Not, expr: Ident(Ident("a")) }"#]],
    );
    check(
        &run_parser!(expr(), "+a"),
        expect_test::expect![[r#"UnaryOp { op: Pos, expr: Ident(Ident("a")) }"#]],
    );
    check(
        &run_parser!(expr(), "-a"),
        expect_test::expect![[r#"UnaryOp { op: Neg, expr: Ident(Ident("a")) }"#]],
    );
    check(
        &run_parser!(expr(), "a * 2.0"),
        expect_test::expect![[r#"BinaryOp { op: Mul, lhs: Ident(Ident("a")), rhs: Immediate(Real(2.0)) }"#]],
    );
    check(
        &run_parser!(expr(), "a / 2.0"),
        expect_test::expect![[r#"BinaryOp { op: Div, lhs: Ident(Ident("a")), rhs: Immediate(Real(2.0)) }"#]],
    );
    check(
        &run_parser!(expr(), "a % 2.0"),
        expect_test::expect![[r#"BinaryOp { op: Mod, lhs: Ident(Ident("a")), rhs: Immediate(Real(2.0)) }"#]],
    );
    check(
        &run_parser!(expr(), "a + 2.0"),
        expect_test::expect![[r#"BinaryOp { op: Add, lhs: Ident(Ident("a")), rhs: Immediate(Real(2.0)) }"#]],
    );
    check(
        &run_parser!(expr(), "a - 2.0"),
        expect_test::expect![[r#"BinaryOp { op: Sub, lhs: Ident(Ident("a")), rhs: Immediate(Real(2.0)) }"#]],
    );
    check(
        &run_parser!(expr(), "a < 2.0"),
        expect_test::expect![[r#"BinaryOp { op: LessThan, lhs: Ident(Ident("a")), rhs: Immediate(Real(2.0)) }"#]],
    );
    check(
        &run_parser!(expr(), "a > 2.0"),
        expect_test::expect![[r#"BinaryOp { op: GreaterThan, lhs: Ident(Ident("a")), rhs: Immediate(Real(2.0)) }"#]],
    );
    check(
        &run_parser!(expr(), "a <= 2.0"),
        expect_test::expect![[r#"BinaryOp { op: LessThanOrEqual, lhs: Ident(Ident("a")), rhs: Immediate(Real(2.0)) }"#]],
    );
    check(
        &run_parser!(expr(), "a >= 2.0"),
        expect_test::expect![[r#"BinaryOp { op: GreaterThanOrEqual, lhs: Ident(Ident("a")), rhs: Immediate(Real(2.0)) }"#]],
    );
    check(
        &run_parser!(expr(), "a == 2.0"),
        expect_test::expect![[r#"BinaryOp { op: Equal, lhs: Ident(Ident("a")), rhs: Immediate(Real(2.0)) }"#]],
    );
    check(
        &run_parser!(expr(), "a != 2.0"),
        expect_test::expect![[r#"BinaryOp { op: NotEqual, lhs: Ident(Ident("a")), rhs: Immediate(Real(2.0)) }"#]],
    );
    check(
        &run_parser!(expr(), "2 * b * 3"),
        expect_test::expect![[r#"BinaryOp { op: Mul, lhs: BinaryOp { op: Mul, lhs: Immediate(Int(2)), rhs: Ident(Ident("b")) }, rhs: Immediate(Int(3)) }"#]],
    );
    check(
        &run_parser!(expr(), "2 < b * 3"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThan, lhs: Immediate(Int(2)), rhs: BinaryOp { op: Mul, lhs: Ident(Ident("b")), rhs: Immediate(Int(3)) } }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2.0 > b * 3.0"),
        expect_test::expect![[
            r#"BinaryOp { op: GreaterThan, lhs: Immediate(Real(2.0)), rhs: BinaryOp { op: Mul, lhs: Ident(Ident("b")), rhs: Immediate(Real(3.0)) } }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2.0 * b < 3.0"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThan, lhs: BinaryOp { op: Mul, lhs: Immediate(Real(2.0)), rhs: Ident(Ident("b")) }, rhs: Immediate(Real(3.0)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2 > b < 3"),
        expect_test::expect![[
            r#"BinaryOp { op: LessThan, lhs: BinaryOp { op: GreaterThan, lhs: Immediate(Int(2)), rhs: Ident(Ident("b")) }, rhs: Immediate(Int(3)) }"#
        ]],
    );
    check(
        &run_parser!(expr(), "2 != b < 3"),
        expect_test::expect![[r#"BinaryOp { op: LessThan, lhs: BinaryOp { op: NotEqual, lhs: Immediate(Int(2)), rhs: Ident(Ident("b")) }, rhs: Immediate(Int(3)) }"#]],
    );
    check(
        &run_parser!(expr(), "2 < b != 3"),
        expect_test::expect![[r#"BinaryOp { op: NotEqual, lhs: BinaryOp { op: LessThan, lhs: Immediate(Int(2)), rhs: Ident(Ident("b")) }, rhs: Immediate(Int(3)) }"#]],
    );
    check(
        &run_parser!(expr(), "a > b * c < d"),
        expect_test::expect![[r#"BinaryOp { op: LessThan, lhs: BinaryOp { op: GreaterThan, lhs: Ident(Ident("a")), rhs: BinaryOp { op: Mul, lhs: Ident(Ident("b")), rhs: Ident(Ident("c")) } }, rhs: Ident(Ident("d")) }"#]],
    );
    check(
        &run_parser!(expr(), "2 + 3 * 4"),
        expect_test::expect!["BinaryOp { op: Add, lhs: Immediate(Int(2)), rhs: BinaryOp { op: Mul, lhs: Immediate(Int(3)), rhs: Immediate(Int(4)) } }"],
    );
    check(
        &run_parser!(expr(), "10 - 8 / 4"),
        expect_test::expect!["BinaryOp { op: Sub, lhs: Immediate(Int(10)), rhs: BinaryOp { op: Div, lhs: Immediate(Int(8)), rhs: Immediate(Int(4)) } }"],
    );
    check(
        &run_parser!(expr(), "10 + 8 % 4"),
        expect_test::expect!["BinaryOp { op: Add, lhs: Immediate(Int(10)), rhs: BinaryOp { op: Mod, lhs: Immediate(Int(8)), rhs: Immediate(Int(4)) } }"],
    );
    check(
        &run_parser!(expr(), "2 + 3 * 4 < 5"),
        expect_test::expect!["BinaryOp { op: LessThan, lhs: BinaryOp { op: Add, lhs: Immediate(Int(2)), rhs: BinaryOp { op: Mul, lhs: Immediate(Int(3)), rhs: Immediate(Int(4)) } }, rhs: Immediate(Int(5)) }"],
    );
    check(
        &run_parser!(expr(), "2 * 3 / 4 < 5"),
        expect_test::expect!["BinaryOp { op: LessThan, lhs: BinaryOp { op: Div, lhs: BinaryOp { op: Mul, lhs: Immediate(Int(2)), rhs: Immediate(Int(3)) }, rhs: Immediate(Int(4)) }, rhs: Immediate(Int(5)) }"],
    );
    check(
        &run_parser!(expr(), "10 - 5 + 3 > 7"),
        expect_test::expect!["BinaryOp { op: GreaterThan, lhs: BinaryOp { op: Add, lhs: BinaryOp { op: Sub, lhs: Immediate(Int(10)), rhs: Immediate(Int(5)) }, rhs: Immediate(Int(3)) }, rhs: Immediate(Int(7)) }"],
    );
    check(
        &run_parser!(expr(), "10 % 2 * 4 < 3"),
        expect_test::expect!["BinaryOp { op: LessThan, lhs: BinaryOp { op: Mul, lhs: BinaryOp { op: Mod, lhs: Immediate(Int(10)), rhs: Immediate(Int(2)) }, rhs: Immediate(Int(4)) }, rhs: Immediate(Int(3)) }"],
    );
    check(
        &run_parser!(expr(), "2 + 3 * 4 - 5 / 2 > 1"),
        expect_test::expect!["BinaryOp { op: GreaterThan, lhs: BinaryOp { op: Sub, lhs: BinaryOp { op: Add, lhs: Immediate(Int(2)), rhs: BinaryOp { op: Mul, lhs: Immediate(Int(3)), rhs: Immediate(Int(4)) } }, rhs: BinaryOp { op: Div, lhs: Immediate(Int(5)), rhs: Immediate(Int(2)) } }, rhs: Immediate(Int(1)) }"],
    );
}

#[test]
fn idents() {
    check(
        &run_parser!(ident(), "foobar"),
        expect_test::expect![[r#"Ident("foobar")"#]],
    );
    check(
        &run_parser!(ident(), "foo_bar"),
        expect_test::expect![[r#"Ident("foo_bar")"#]],
    );
    check(
        &run_parser!(ident(), "FOO_bar"),
        expect_test::expect![[r#"Ident("FOO_bar")"#]],
    );
    check(
        &run_parser!(ident(), "__FOO"),
        expect_test::expect![[r#"Ident("__FOO")"#]],
    );
    check(
        &run_parser!(ident(), "_2_FOO1"),
        expect_test::expect![[r#"Ident("_2_FOO1")"#]],
    );
    check(
        &run_parser!(ident(), "_"),
        expect_test::expect![[r#"Ident("_")"#]],
    );
    check(
        &run_parser!(ident(), "12_ab"),
        expect_test::expect![[r#"
            @0..2: found "12" but expected something else
        "#]],
    );
    check(
        // Lexer will split this into 3 tokens, ident() will parse the first one.
        &run_parser!(ident(), "ab*cd"),
        expect_test::expect![[r#"Ident("ab")"#]],
    );
}

#[test]
fn fn_decl_test() {
    let src = r#"
fn foo(x: real, y: real) -> real {
    let z = 5.0;
    z
}
"#;

    check(
        &run_parser!(yurt_program(), src),
        expect_test::expect![[
            r#"[Fn { name: Ident("foo"), params: [(Ident("x"), Real), (Ident("y"), Real)], return_type: Real, body: Block { statements: [Let(LetStatement { name: Ident("z"), ty: None, init: Immediate(Real(5.0)) })], final_expr: Ident(Ident("z")) } }]"#
        ]],
    );
}

#[test]
fn fn_call() {
    let src = r#"
let x = foo(a*3, c);
"#;

    check(
        &run_parser!(yurt_program(), src),
        expect_test::expect![[
            r#"[Let(LetStatement { name: Ident("x"), ty: None, init: Call { name: Ident("foo"), args: [BinaryOp { op: Mul, lhs: Ident(Ident("a")), rhs: Immediate(Int(3)) }, Ident(Ident("c"))] } })]"#
        ]],
    );
}

#[test]
fn code_blocks() {
    check(
        &run_parser!(let_decl(expr()), "let x = { 0 };"),
        expect_test::expect![[
            r#"Let(LetStatement { name: Ident("x"), ty: None, init: Block(Block { statements: [], final_expr: Immediate(Int(0)) }) })"#
        ]],
    );

    check(
        &run_parser!(let_decl(expr()), "let x = { constraint x > 0.0; 0.0 };"),
        expect_test::expect![[
            r#"Let(LetStatement { name: Ident("x"), ty: None, init: Block(Block { statements: [Constraint(BinaryOp { op: GreaterThan, lhs: Ident(Ident("x")), rhs: Immediate(Real(0.0)) })], final_expr: Immediate(Real(0.0)) }) })"#
        ]],
    );

    check(
        &run_parser!(
            constraint_decl(expr()),
            "constraint { constraint { true }; x > 0 };"
        ),
        expect_test::expect![[
            r#"Constraint(Block(Block { statements: [Constraint(Block(Block { statements: [], final_expr: Immediate(Bool(true)) }))], final_expr: BinaryOp { op: GreaterThan, lhs: Ident(Ident("x")), rhs: Immediate(Int(0)) } }))"#
        ]],
    );

    check(
        &run_parser!(let_decl(expr()), "let x = { 1.0 } * { 2.0 };"),
        expect_test::expect![[
            r#"Let(LetStatement { name: Ident("x"), ty: None, init: BinaryOp { op: Mul, lhs: Block(Block { statements: [], final_expr: Immediate(Real(1.0)) }), rhs: Block(Block { statements: [], final_expr: Immediate(Real(2.0)) }) } })"#
        ]],
    );

    check(
        &format!(
            "{:?}",
            run_parser!(let_decl(expr()), "let x = {};")
        ),
        expect_test::expect![[r#""@9..10: found \"}\" but expected \"!\", \"+\", \"-\", \"{\", \"If\", \"var\", \"let\",  or \"constraint\"\n""#]],
    );
}

#[test]
fn if_exprs() {
    check(
        &run_parser!(if_expr(expr()), "if cond { 1 }"),
        expect_test::expect![[r#"
            @13..13: found end of input but expected "Else"
        "#]],
    );

    check(
        &run_parser!(if_expr(expr()), "if cond { 1 } else { 0 }"),
        expect_test::expect![[
            r#"If(IfExpr { condition: Ident(Ident("cond")), then_block: Block { statements: [], final_expr: Immediate(Int(1)) }, else_block: Block { statements: [], final_expr: Immediate(Int(0)) } })"#
        ]],
    );

    check(
        &run_parser!(
            if_expr(expr()),
            "if cond { if cond { 1 } else { 0 } } else { 2 }"
        ),
        expect_test::expect![[
            r#"If(IfExpr { condition: Ident(Ident("cond")), then_block: Block { statements: [], final_expr: If(IfExpr { condition: Ident(Ident("cond")), then_block: Block { statements: [], final_expr: Immediate(Int(1)) }, else_block: Block { statements: [], final_expr: Immediate(Int(0)) } }) }, else_block: Block { statements: [], final_expr: Immediate(Int(2)) } })"#
        ]],
    );

    check(
        &run_parser!(
            if_expr(expr()),
            "if cond { 1.0 } else { if cond { 2.0 } else { 3.0 } }"
        ),
        expect_test::expect![[
            r#"If(IfExpr { condition: Ident(Ident("cond")), then_block: Block { statements: [], final_expr: Immediate(Real(1.0)) }, else_block: Block { statements: [], final_expr: If(IfExpr { condition: Ident(Ident("cond")), then_block: Block { statements: [], final_expr: Immediate(Real(2.0)) }, else_block: Block { statements: [], final_expr: Immediate(Real(3.0)) } }) } })"#
        ]],
    );
}

#[test]
fn basic_program() {
    let src = r#"
var low_val: real = 1.23;
let high_val = 4.56;        // Implicit type.

// Here's the constraints.
constraint mid > low_val * 2.0;
constraint mid < high_val;

solve minimize mid;
"#;

    check(
        &run_parser!(yurt_program(), src),
        expect_test::expect![[
            r#"[Var(VarStatement { name: Ident("low_val"), ty: Some(Real), init: Some(Immediate(Real(1.23))) }), Let(LetStatement { name: Ident("high_val"), ty: None, init: Immediate(Real(4.56)) }), Constraint(BinaryOp { op: GreaterThan, lhs: Ident(Ident("mid")), rhs: BinaryOp { op: Mul, lhs: Ident(Ident("low_val")), rhs: Immediate(Real(2.0)) } }), Constraint(BinaryOp { op: LessThan, lhs: Ident(Ident("mid")), rhs: Ident(Ident("high_val")) }), Solve(Minimize(Ident("mid")))]"#
        ]],
    );
}

#[test]
fn with_errors() {
    check(
        &run_parser!(yurt_program(), "let low_val: bad = 1.23"),
        expect_test::expect![[r#"
            @13..16: found "bad" but expected "real", "int", "bool",  or "string"
        "#]],
    );
}

#[test]
fn fn_errors() {
    check(
        &run_parser!(yurt_program(), "fn foo() {5}"),
        expect_test::expect![[r#"
            @9..10: found "{" but expected "->"
        "#]],
    );

    check(
        &run_parser!(yurt_program(), "fn foo() -> real {}"),
        expect_test::expect!["Err([ExpectedFound { span: 18..19, expected: [Some(If), Some(BraceOpen), Some(Bang), Some(Minus), Some(Plus), Some(Constraint), Some(Let), Some(Var)], found: Some(BraceClose) }])"],
    );
}

#[test]
fn out_of_order_decls() {
    let src = r#"
solve maximize low;
constraint low < high;
let high = 2.0;
solve satisfy;
let low = 1.0;
"#;

    check(
        &run_parser!(yurt_program(), src),
        expect_test::expect![[
            r#"[Solve(Maximize(Ident("low"))), Constraint(BinaryOp { op: LessThan, lhs: Ident(Ident("low")), rhs: Ident(Ident("high")) }), Let(LetStatement { name: Ident("high"), ty: None, init: Immediate(Real(2.0)) }), Solve(Satisfy), Let(LetStatement { name: Ident("low"), ty: None, init: Immediate(Real(1.0)) })]"#
        ]],
    );
}
