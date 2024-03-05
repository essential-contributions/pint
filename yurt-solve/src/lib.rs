#[cfg(feature = "solver-scip")]
mod error;
pub mod flatyurt;
#[cfg(feature = "solver-scip")]
pub mod scip;

use crate::flatyurt::FlatYurt;
#[cfg(feature = "solver-scip")]
use crate::scip::Solver;
use lalrpop_util::lalrpop_mod;
#[cfg(feature = "solver-scip")]
use russcip::ProblemCreated;

lalrpop_mod!(#[allow(unused, clippy::ptr_arg, clippy::type_complexity)] pub flatyurt_parser);

pub fn parse_flatyurt(
    src: &str,
) -> Result<FlatYurt, lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token, &'static str>> {
    flatyurt_parser::FlatYurtParser::new().parse(src)
}

#[cfg(feature = "solver-scip")]
pub fn solver(ast: &FlatYurt) -> Solver<ProblemCreated> {
    Solver::<ProblemCreated>::new(ast)
}
