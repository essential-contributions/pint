mod dead_code_elimination;
use dead_code_elimination::dead_code_elimination;

use crate::error::Handler;

impl super::Contract {
    pub fn optimize(mut self, handler: &Handler) -> Self {
        // Remove dead code
        let _ = dead_code_elimination(&mut self, handler);

        self
    }
}
