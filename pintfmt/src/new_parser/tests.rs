#[cfg(test)]
use expect_test::expect;

#[cfg(test)]
use lalrpop_util::lalrpop_mod;

#[cfg(test)]
lalrpop_mod!(#[allow(unused)] pub pintfmt_parser);

#[cfg(test)]
fn check(actual: &str, expect: expect_test::Expect) {
    expect.assert_eq(actual);
}

#[cfg(test)]
fn run_parser(source: &str) -> String {
    use crate::lexer;

    let lexer = lexer::Lexer::new(&source);
    let parser = pintfmt_parser::NewlineParser::new();
    let ast = parser.parse(lexer);

    match ast {
        Ok(ast) => format!("{:#?}", ast),
        Err(ast) => format!("{}", ast),
    }
}

#[test]
fn newlines() {
    check(run_parser("\n").as_str(), expect![["Newline"]]);
}
