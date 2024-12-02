#[cfg(feature = "solver-scip")]
mod error;
pub mod flatpint;
#[cfg(feature = "solver-scip")]
pub mod scip;

use crate::flatpint::FlatPint;
#[cfg(feature = "solver-scip")]
use crate::scip::Solver;
use lalrpop_util::lalrpop_mod;
#[cfg(feature = "solver-scip")]
use russcip::ProblemCreated;

lalrpop_mod!(#[allow(unused, clippy::ptr_arg, clippy::type_complexity, clippy::empty_line_after_outer_attr)] pub flatpint_parser);

pub fn parse_flatpint(
    src: &str,
) -> Result<FlatPint, lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token, &'static str>> {
    flatpint_parser::FlatPintParser::new().parse(src)
}

#[cfg(feature = "solver-scip")]
pub fn solver(ast: &FlatPint) -> Solver<ProblemCreated> {
    Solver::<ProblemCreated>::new(ast)
}
