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

#[test]
fn value_decls() {
    assert_eq!(
        format!("{:?}", run_parser!(value_decl(), "let blah = 1")),
        r#"Ok(Value { name: Ident("blah"), ty: None, init: Immediate(Real(1.0)) })"#
    );

    assert_eq!(
        format!("{:?}", run_parser!(value_decl(), "let blah: real = 1")),
        r#"Ok(Value { name: Ident("blah"), ty: Some(Real), init: Immediate(Real(1.0)) })"#
    );

    assert_eq!(
        format!("{:?}", run_parser!(value_decl(), "let blah: real")),
        r#"Err([Simple { span: 14..14, reason: Unexpected, expected: {Some(Eq)}, found: None, label: None }])"#
    );
}

#[test]
fn constraint_decls() {
    // Argument just needs to be any expression, as far as the parser is concerned.
    assert_eq!(
        format!("{:?}", run_parser!(constraint_decl(), "constraint blah")),
        r#"Ok(Constraint(Ident(Ident("blah"))))"#
    );
}

#[test]
fn solve_decls() {
    assert_eq!(
        format!("{:?}", run_parser!(solve_decl(), "solve satisfy")),
        r#"Ok(Solve(Satisfy))"#
    );

    assert_eq!(
        format!("{:?}", run_parser!(solve_decl(), "solve minimize foo")),
        r#"Ok(Solve(Minimize(Ident("foo"))))"#
    );

    assert_eq!(
        format!("{:?}", run_parser!(solve_decl(), "solve maximize foo")),
        r#"Ok(Solve(Maximize(Ident("foo"))))"#
    );

    let res = run_parser!(solve_decl(), "solve world hunger");
    assert!(res.is_err());
    let simple = &res.unwrap_err()[0];
    assert_eq!(simple.reason(), &chumsky::error::SimpleReason::Unexpected);
    assert_eq!(simple.found(), Some(&Token::Ident("world")));
}

#[test]
fn exprs() {
    assert_eq!(
        format!("{:?}", run_parser!(expr(), "123")),
        r#"Ok(Immediate(Real(123.0)))"#,
    );
    assert_eq!(
        format!("{:?}", run_parser!(expr(), "foo")),
        r#"Ok(Ident(Ident("foo")))"#,
    );
    assert_eq!(
        format!("{:?}", run_parser!(expr(), "a * 2")),
        r#"Ok(BinaryOp { op: Mul, lhs: Ident(Ident("a")), rhs: Immediate(Real(2.0)) })"#,
    );
    assert_eq!(
        format!("{:?}", run_parser!(expr(), "2 * b * 3")),
        [
            r#"Ok("#,
            r#"BinaryOp { op: Mul, "#,
            r#"lhs: BinaryOp { op: Mul, lhs: Immediate(Real(2.0)), rhs: Ident(Ident("b")) }, "#,
            r#"rhs: Immediate(Real(3.0)) "#,
            r#"})"#,
        ]
        .concat()
    );
    assert_eq!(
        format!("{:?}", run_parser!(expr(), "2 < b * 3")),
        [
            // Mul binds tighter than the less-than.
            r#"Ok(BinaryOp { "#,
            r#"op: LessThan, "#,
            r#"lhs: Immediate(Real(2.0)), "#,
            r#"rhs: BinaryOp { "#,
            r#"op: Mul, "#,
            r#"lhs: Ident(Ident("b")), "#,
            r#"rhs: Immediate(Real(3.0)) "#,
            r#"} "#,
            r#"})"#,
        ]
        .concat()
    );
    assert_eq!(
        format!("{:?}", run_parser!(expr(), "2 > b * 3")),
        [
            // Mul binds tighter than the greater-than.
            r#"Ok(BinaryOp { "#,
            r#"op: GreaterThan, "#,
            r#"lhs: Immediate(Real(2.0)), "#,
            r#"rhs: BinaryOp { "#,
            r#"op: Mul, "#,
            r#"lhs: Ident(Ident("b")), "#,
            r#"rhs: Immediate(Real(3.0)) "#,
            r#"} "#,
            r#"})"#,
        ]
        .concat()
    );
    assert_eq!(
        format!("{:?}", run_parser!(expr(), "2 * b < 3")),
        [
            // Mul binds tighter, putting the less-than on the outside.
            r#"Ok(BinaryOp { op: LessThan, "#,
            r#"lhs: BinaryOp { "#,
            r#"op: Mul, "#,
            r#"lhs: Immediate(Real(2.0)), "#,
            r#"rhs: Ident(Ident("b")) "#,
            r#"}, "#,
            r#"rhs: Immediate(Real(3.0)) "#,
            r#"})"#,
        ]
        .concat()
    );
    assert_eq!(
        format!("{:?}", run_parser!(expr(), "2 > b < 3")),
        [
            // Parses OK, but should be a type error.
            r#"Ok(BinaryOp { "#,
            r#"op: LessThan, "#,
            r#"lhs: BinaryOp { "#,
            r#"op: GreaterThan, "#,
            r#"lhs: Immediate(Real(2.0)), "#,
            r#"rhs: Ident(Ident("b")) "#,
            r#"}, "#,
            r#"rhs: Immediate(Real(3.0)) "#,
            r#"})"#
        ]
        .concat()
    );
    assert_eq!(
        format!("{:?}", run_parser!(expr(), "a > b * c < d")),
        [
            // Ditto, type error.  But the mul binds the tightest.
            r#"Ok(BinaryOp { "#,
            r#"op: LessThan, "#,
            r#"lhs: BinaryOp { "#,
            r#"op: GreaterThan, "#,
            r#"lhs: Ident(Ident("a")), "#,
            r#"rhs: BinaryOp { "#,
            r#"op: Mul, "#,
            r#"lhs: Ident(Ident("b")), "#,
            r#"rhs: Ident(Ident("c")) "#,
            r#"} "#,
            r#"}, "#,
            r#"rhs: Ident(Ident("d")) "#,
            r#"})"#,
        ]
        .concat()
    );
}

#[test]
fn idents() {
    assert_eq!(
        format!("{:?}", run_parser!(ident(), "foobar")),
        r#"Ok(Ident("foobar"))"#
    );
    assert_eq!(
        format!("{:?}", run_parser!(ident(), "foo_bar")),
        r#"Ok(Ident("foo_bar"))"#
    );
    assert_eq!(
        format!("{:?}", run_parser!(ident(), "FOO_bar")),
        r#"Ok(Ident("FOO_bar"))"#
    );
    assert_eq!(
        format!("{:?}", run_parser!(ident(), "__FOO")),
        r#"Ok(Ident("__FOO"))"#
    );
    assert_eq!(
        format!("{:?}", run_parser!(ident(), "_2_FOO1")),
        r#"Ok(Ident("_2_FOO1"))"#
    );
    assert_eq!(
        format!("{:?}", run_parser!(ident(), "_")),
        r#"Ok(Ident("_"))"#
    );
    assert_eq!(
        format!("{:?}", run_parser!(ident(), "12_ab")),
        [
            r#"Err([Simple { span: 0..2, reason: Unexpected, expected: {}, "#,
            r#"found: Some(Number("12")), label: None }])"#
        ]
        .concat()
    );
    assert_eq!(
        // Lexer will split this into 3 tokens, ident() will parse the first one.
        format!("{:?}", run_parser!(ident(), "ab*cd")),
        r#"Ok(Ident("ab"))"#
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

    // This is still a bit crude for larger ASTs.
    let res = run_parser!(yurt_program(), src);
    assert_eq!(
        format!("{res:?}"),
        [
            r#"Ok(["#,
            r#"Value { name: Ident("low_val"), ty: Some(Real), init: Immediate(Real(1.23)) }, "#,
            r#"Value { name: Ident("high_val"), ty: None, init: Immediate(Real(4.56)) }, "#,
            r#"Constraint(BinaryOp { "#,
            r#"op: GreaterThan, "#,
            r#"lhs: Ident(Ident("mid")), "#,
            r#"rhs: BinaryOp { "#,
            r#"op: Mul, "#,
            r#"lhs: Ident(Ident("low_val")), "#,
            r#"rhs: Immediate(Real(2.0)) } "#,
            r#"}), "#,
            r#"Constraint(BinaryOp { "#,
            r#"op: LessThan, "#,
            r#"lhs: Ident(Ident("mid")), "#,
            r#"rhs: Ident(Ident("high_val")) }), "#,
            r#"Solve(Minimize(Ident("mid")))"#,
            r#"])"#
        ]
        .concat()
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
fn out_of_order_decls() {
    let src = r#"
solve maximize low;
constraint low < high;
let high = 2;
solve satisfy;
let low = 1;
"#;

    let res = run_parser!(yurt_program(), src);
    assert_eq!(
        format!("{res:?}"),
        [
            r#"Ok(["#,
            r#"Solve(Maximize(Ident("low"))), "#,
            r#"Constraint(BinaryOp { "#,
            r#"op: LessThan, "#,
            r#"lhs: Ident(Ident("low")), "#,
            r#"rhs: Ident(Ident("high")) }), "#,
            r#"Value { name: Ident("high"), ty: None, init: Immediate(Real(2.0)) }, "#,
            r#"Solve(Satisfy), "#,
            r#"Value { name: Ident("low"), ty: None, init: Immediate(Real(1.0)) }"#,
            r#"])"#
        ]
        .concat()
    );
}
