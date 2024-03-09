use std::fmt::{Display, Formatter, Result};

impl Display for super::IntermediateIntent {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for var in &self.vars {
            writeln!(f, "{};", self.with_ii(var.0))?;
        }
        for state in &self.states {
            writeln!(f, "{};", self.with_ii(state.0))?;
        }
        for r#enum in &self.enums {
            writeln!(f, "{};", self.with_ii(r#enum))?;
        }
        for new_type in &self.new_types {
            writeln!(f, "{};", self.with_ii(new_type))?;
        }
        for constraint in &self.constraints {
            writeln!(f, "constraint {};", self.with_ii(constraint.0))?;
        }
        for directive in &self.directives {
            writeln!(f, "{};", self.with_ii(directive.0.clone()))?;
        }
        Ok(())
    }
}
