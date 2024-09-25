mod dead_code_elimination;
mod expr_simplification;

use dead_code_elimination::dead_code_elimination;
use expr_simplification::simplify_exprs;

use crate::error::Handler;

impl super::Contract {
    pub fn optimize(mut self, handler: &Handler) -> Self {
        // Remove dead code
        dead_code_elimination(handler, &mut self);

        simplify_exprs(&mut self);

        self
    }
}
