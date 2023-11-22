use std::fmt::{Display, Formatter, Result};

impl Display for super::IntermediateIntent {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for var in &self.vars {
            writeln!(f, "{};", self.with_ii(var.0))?;
        }
        for state in &self.states {
            writeln!(f, "{};", self.with_ii(state))?;
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
        for interface in &self.interfaces {
            writeln!(f, "{};", self.with_ii(interface))?;
        }
        for contract in &self.contracts {
            writeln!(f, "{};", self.with_ii(contract))?;
        }
        for r#extern in &self.externs {
            write!(f, "extern {{")?;
            for fn_sig in &r#extern.0 {
                write!(f, " {};", self.with_ii(fn_sig))?;
            }
            writeln!(f, " }};")?;
        }
        for directive in &self.directives {
            writeln!(f, "{};", self.with_ii(directive.0.clone()))?;
        }
        Ok(())
    }
}
