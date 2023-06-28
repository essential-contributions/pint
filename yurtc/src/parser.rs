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
    match yurt_program().parse(token_stream) {
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

fn yurt_program<'sc>() -> impl Parser<Token<'sc>, Ast, Error = Simple<Token<'sc>>> + Clone {
    choice((value_decl(), constraint_decl(), solve_decl(), fn_decl()))
        .repeated()
        .then_ignore(end())
}

fn value_decl<'sc>() -> impl Parser<Token<'sc>, ast::Decl, Error = Simple<Token<'sc>>> + Clone {
    let_statement().map(ast::Decl::Value)
}

fn let_statement<'sc>(
) -> impl Parser<Token<'sc>, ast::LetStatement, Error = Simple<Token<'sc>>> + Clone {
    let type_spec = just(Token::Colon).ignore_then(type_());
    just(Token::Let)
        .ignore_then(ident())
        .then(type_spec.or_not())
        .then_ignore(just(Token::Eq))
        .then(expr())
        .then_ignore(just(Token::Semi))
        .map(|((name, ty), init)| ast::LetStatement { name, ty, init })
}

fn constraint_decl<'sc>() -> impl Parser<Token<'sc>, ast::Decl, Error = Simple<Token<'sc>>> + Clone
{
    just(Token::Constraint)
        .ignore_then(expr())
        .then_ignore(just(Token::Semi))
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
        .then_ignore(just(Token::Semi))
        .map(ast::Decl::Solve)
}

fn fn_decl<'sc>() -> impl Parser<Token<'sc>, ast::Decl, Error = Simple<Token<'sc>>> + Clone {
    let type_spec = just(Token::Colon).ignore_then(type_());

    let params = ident()
        .then(type_spec)
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .delimited_by(just(Token::ParenOpen), just(Token::ParenClose));

    let return_type = just(Token::Arrow).ignore_then(type_());

    let body = let_statement().repeated().then(expr());

    just(Token::Fn)
        .ignore_then(ident())
        .then(params)
        .then(return_type)
        .then(body.delimited_by(just(Token::BraceOpen), just(Token::BraceClose)))
        .map(
            |(((name, params), return_type), (statements, expr))| ast::Decl::Fn {
                name,
                params,
                return_type,
                body: ast::CodeBlock {
                    statements,
                    final_expr: expr,
                },
            },
        )
}

fn expr<'sc>() -> impl Parser<Token<'sc>, ast::Expr, Error = Simple<Token<'sc>>> + Clone {
    let imm = immediate().map(ast::Expr::Immediate);
    let id = ident().map(ast::Expr::Ident);

    recursive(|expr| {
        let args = expr
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .delimited_by(just(Token::ParenOpen), just(Token::ParenClose));

        let call = ident()
            .then(args)
            .map(|(name, args)| ast::Expr::Call { name, args });

        let atom = imm.or(call).or(id);

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
    })
}

fn ident<'sc>() -> impl Parser<Token<'sc>, ast::Ident, Error = Simple<Token<'sc>>> + Clone {
    select! { Token::Ident(id) => ast::Ident(id.to_owned()) }
}

fn type_<'sc>() -> impl Parser<Token<'sc>, ast::Type, Error = Simple<Token<'sc>>> + Clone {
    just(Token::Real).to(ast::Type::Real)
}

fn immediate<'sc>() -> impl Parser<Token<'sc>, ast::Immediate, Error = Simple<Token<'sc>>> + Clone {
    select! { Token::RealNumber(num_str) => ast::Immediate::Real(num_str.parse().unwrap()) }
}

// To-do tests:
// - block - constraints { .. }
// - all floats
// - hex
// - parens in expressions
// - unary negation, boolean not
// - can't have keywords as idents
//
// - errors (currently Simple, need to switch to Rich)

#[cfg(test)]
macro_rules! run_parser {
    ($parser: expr, $source: expr) => {{
        let (toks, errs) = lexer::lex($source);
        assert!(errs.is_empty());
        let token_stream = Stream::from_iter($source.len()..$source.len(), toks.into_iter());
        $parser.parse(token_stream)
    }};
}

#[cfg(test)]
fn check(actual: &str, expect: expect_test::Expect) {
    expect.assert_eq(actual);
}

#[test]
fn value_decls() {
    check(
        &format!("{:?}", run_parser!(value_decl(), "let blah = 1.0;")),
        expect_test::expect![[
            r#"Ok(Value(LetStatement { name: Ident("blah"), ty: None, init: Immediate(Real(1.0)) }))"#
        ]],
    );

    check(
        &format!("{:?}", run_parser!(value_decl(), "let blah: real = 1.0;")),
        expect_test::expect![[
            r#"Ok(Value(LetStatement { name: Ident("blah"), ty: Some(Real), init: Immediate(Real(1.0)) }))"#
        ]],
    );

    check(
        &format!("{:?}", run_parser!(value_decl(), "let blah: real")),
        expect_test::expect![[
            r#"Err([Simple { span: 14..14, reason: Unexpected, expected: {Some(Eq)}, found: None, label: None }])"#
        ]],
    );
}

#[test]
fn constraint_decls() {
    // Argument just needs to be any expression, as far as the parser is concerned.
    check(
        &format!("{:?}", run_parser!(constraint_decl(), "constraint blah;")),
        expect_test::expect![[r#"Ok(Constraint(Ident(Ident("blah"))))"#]],
    );
}

#[test]
fn solve_decls() {
    check(
        &format!("{:?}", run_parser!(solve_decl(), "solve satisfy;")),
        expect_test::expect![[r#"Ok(Solve(Satisfy))"#]],
    );

    check(
        &format!("{:?}", run_parser!(solve_decl(), "solve minimize foo;")),
        expect_test::expect![[r#"Ok(Solve(Minimize(Ident("foo"))))"#]],
    );

    check(
        &format!("{:?}", run_parser!(solve_decl(), "solve maximize foo;")),
        expect_test::expect![[r#"Ok(Solve(Maximize(Ident("foo"))))"#]],
    );

    let res = run_parser!(solve_decl(), "solve world hunger;");
    assert!(res.is_err());
    let simple = &res.unwrap_err()[0];
    assert_eq!(simple.reason(), &chumsky::error::SimpleReason::Unexpected);
    assert_eq!(simple.found(), Some(&Token::Ident("world")));
}

#[test]
fn exprs() {
    check(
        &format!("{:?}", run_parser!(expr(), "123")),
        expect_test::expect![[r#"Ok(Immediate(Real(123.0)))"#]],
    );
    check(
        &format!("{:?}", run_parser!(expr(), "foo")),
        expect_test::expect![[r#"Ok(Ident(Ident("foo")))"#]],
    );
    check(
        &format!("{:?}", run_parser!(expr(), "a * 2")),
        expect_test::expect![[
            r#"Ok(BinaryOp { op: Mul, lhs: Ident(Ident("a")), rhs: Immediate(Real(2.0)) })"#
        ]],
    );
    check(
        &format!("{:?}", run_parser!(expr(), "2 * b * 3")),
        expect_test::expect![[
            r#"Ok(BinaryOp { op: Mul, lhs: BinaryOp { op: Mul, lhs: Immediate(Real(2.0)), rhs: Ident(Ident("b")) }, rhs: Immediate(Real(3.0)) })"#
        ]],
    );
    check(
        &format!("{:?}", run_parser!(expr(), "2 < b * 3")),
        expect_test::expect![[
            r#"Ok(BinaryOp { op: LessThan, lhs: Immediate(Real(2.0)), rhs: BinaryOp { op: Mul, lhs: Ident(Ident("b")), rhs: Immediate(Real(3.0)) } })"#
        ]],
    );
    check(
        &format!("{:?}", run_parser!(expr(), "2 > b * 3")),
        expect_test::expect![[
            r#"Ok(BinaryOp { op: GreaterThan, lhs: Immediate(Real(2.0)), rhs: BinaryOp { op: Mul, lhs: Ident(Ident("b")), rhs: Immediate(Real(3.0)) } })"#
        ]],
    );
    check(
        &format!("{:?}", run_parser!(expr(), "2 * b < 3")),
        expect_test::expect![[
            r#"Ok(BinaryOp { op: LessThan, lhs: BinaryOp { op: Mul, lhs: Immediate(Real(2.0)), rhs: Ident(Ident("b")) }, rhs: Immediate(Real(3.0)) })"#
        ]],
    );
    check(
        &format!("{:?}", run_parser!(expr(), "2 > b < 3")),
        expect_test::expect![[
            r#"Ok(BinaryOp { op: LessThan, lhs: BinaryOp { op: GreaterThan, lhs: Immediate(Real(2.0)), rhs: Ident(Ident("b")) }, rhs: Immediate(Real(3.0)) })"#
        ]],
    );
    check(
        &format!("{:?}", run_parser!(expr(), "a > b * c < d")),
        expect_test::expect![[
            r#"Ok(BinaryOp { op: LessThan, lhs: BinaryOp { op: GreaterThan, lhs: Ident(Ident("a")), rhs: BinaryOp { op: Mul, lhs: Ident(Ident("b")), rhs: Ident(Ident("c")) } }, rhs: Ident(Ident("d")) })"#
        ]],
    );
}

#[test]
fn idents() {
    check(
        &format!("{:?}", run_parser!(ident(), "foobar")),
        expect_test::expect![[r#"Ok(Ident("foobar"))"#]],
    );
    check(
        &format!("{:?}", run_parser!(ident(), "foo_bar")),
        expect_test::expect![[r#"Ok(Ident("foo_bar"))"#]],
    );
    check(
        &format!("{:?}", run_parser!(ident(), "FOO_bar")),
        expect_test::expect![[r#"Ok(Ident("FOO_bar"))"#]],
    );
    check(
        &format!("{:?}", run_parser!(ident(), "__FOO")),
        expect_test::expect![[r#"Ok(Ident("__FOO"))"#]],
    );
    check(
        &format!("{:?}", run_parser!(ident(), "_2_FOO1")),
        expect_test::expect![[r#"Ok(Ident("_2_FOO1"))"#]],
    );
    check(
        &format!("{:?}", run_parser!(ident(), "_")),
        expect_test::expect![[r#"Ok(Ident("_"))"#]],
    );
    check(
        &format!("{:?}", run_parser!(ident(), "12_ab")),
        expect_test::expect![[
            r#"Err([Simple { span: 0..2, reason: Unexpected, expected: {}, found: Some(Number("12")), label: None }])"#
        ]],
    );
    check(
        // Lexer will split this into 3 tokens, ident() will parse the first one.
        &format!("{:?}", run_parser!(ident(), "ab*cd")),
        expect_test::expect![[r#"Ok(Ident("ab"))"#]],
    );
}

#[test]
fn fn_decl_test() {
    let src = r#"
fn foo(x: real, y: real) -> real {
    let z = 5;
    z
}
"#;

    check(
        &format!("{:?}", run_parser!(yurt_program(), src)),
        expect_test::expect![[
            r#"Ok([Fn { name: Ident("foo"), params: [(Ident("x"), Real), (Ident("y"), Real)], return_type: Real, body: CodeBlock { statements: [LetStatement { name: Ident("z"), ty: None, init: Immediate(Real(5.0)) }], final_expr: Ident(Ident("z")) } }])"#
        ]],
    );
}

#[test]
fn fn_call() {
    let src = r#"
let x = foo(a*3, c);
"#;

    check(
        &format!("{:?}", run_parser!(yurt_program(), src)),
        expect_test::expect![[
            r#"Ok([Value(LetStatement { name: Ident("x"), ty: None, init: Call { name: Ident("foo"), args: [BinaryOp { op: Mul, lhs: Ident(Ident("a")), rhs: Immediate(Real(3.0)) }, Ident(Ident("c"))] } })])"#
        ]],
    );
}

#[test]
fn basic_program() {
    let src = r#"
let low_val: real = 1.23;
let high_val = 4.56;        // Implicit type.

// Here's the constraints.
constraint mid > low_val * 2;
constraint mid < high_val;

solve minimize mid;
"#;

    check(
        &format!("{:?}", run_parser!(yurt_program(), src)),
        expect_test::expect![[
            r#"Ok([Value(LetStatement { name: Ident("low_val"), ty: Some(Real), init: Immediate(Real(1.23)) }), Value(LetStatement { name: Ident("high_val"), ty: None, init: Immediate(Real(4.56)) }), Constraint(BinaryOp { op: GreaterThan, lhs: Ident(Ident("mid")), rhs: BinaryOp { op: Mul, lhs: Ident(Ident("low_val")), rhs: Immediate(Real(2.0)) } }), Constraint(BinaryOp { op: LessThan, lhs: Ident(Ident("mid")), rhs: Ident(Ident("high_val")) }), Solve(Minimize(Ident("mid")))])"#
        ]],
    );
}

#[test]
fn with_errors() {
    let src = r#"let low_val: bad = 1.23"#;

    let res = run_parser!(value_decl(), src);
    assert!(res.is_err());
    let errs = res.unwrap_err();
    assert!(matches!(errs[0], Simple { .. }));
}

#[test]
fn fn_errors() {
    let src = r#"fn foo() {5}"#;
    let res = run_parser!(fn_decl(), src);
    assert!(res.is_err());
    let err = &res.unwrap_err()[0];
    assert_eq!(err.reason(), &chumsky::error::SimpleReason::Unexpected);
    assert_eq!(err.found(), Some(&Token::BraceOpen));
    assert_eq!(
        err.expected().collect::<Vec<_>>(),
        vec![&Some(Token::Arrow)]
    );

    let src = r#"fn foo() -> real {}"#;
    let res = run_parser!(fn_decl(), src);
    assert!(res.is_err());
    let err = &res.unwrap_err()[0];
    assert_eq!(err.reason(), &chumsky::error::SimpleReason::Unexpected);
    assert_eq!(err.found(), Some(&Token::BraceClose));
    assert_eq!(err.expected().collect::<Vec<_>>(), vec![&Some(Token::Let)]);
}

#[test]
fn out_of_order_decls() {
    let src = r#"
solve maximize low;
constraint low < high;
let high = 2;
solve satisfy;
let low = 1;
"#;

    check(
        &format!("{:?}", run_parser!(yurt_program(), src)),
        expect_test::expect![[
            r#"Ok([Solve(Maximize(Ident("low"))), Constraint(BinaryOp { op: LessThan, lhs: Ident(Ident("low")), rhs: Ident(Ident("high")) }), Value(LetStatement { name: Ident("high"), ty: None, init: Immediate(Real(2.0)) }), Solve(Satisfy), Value(LetStatement { name: Ident("low"), ty: None, init: Immediate(Real(1.0)) })])"#
        ]],
    );
}
