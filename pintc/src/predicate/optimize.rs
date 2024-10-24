mod const_folding;
mod dead_code_elimination;

use const_folding::const_folding;
use dead_code_elimination::dead_code_elimination;

use crate::error::Handler;

impl super::Contract {
    pub fn optimize(mut self, handler: &Handler) -> Self {
        const_folding(&mut self);

        dead_code_elimination(handler, &mut self);

        self
    }
}
