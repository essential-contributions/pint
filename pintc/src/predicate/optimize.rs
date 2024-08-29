mod dead_code_elimination;
use dead_code_elimination::dead_code_elimination;

impl super::Contract {
    pub fn optimize(mut self) -> Self {
        // Remove dead code
        dead_code_elimination(&mut self);

        self
    }
}
