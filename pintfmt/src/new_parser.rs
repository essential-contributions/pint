use lalrpop_util::lalrpop_mod;

lalrpop_mod!(#[allow(clippy::ptr_arg, clippy::type_complexity)] pub pint_parser);

pub fn parse_str_to_ast(source: &str) {
    assert!(pint_parser::TermParser::new().parse("22").is_ok())
}
