mod dead_code_elimination;
use dead_code_elimination::dead_code_elimination;

use crate::error::Handler;

impl super::Contract {
    pub fn optimize(mut self, handler: &Handler) -> Self {
        // Remove dead code
        dead_code_elimination(handler, &mut self);

        self
    }
}
