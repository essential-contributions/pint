use std::fmt::{Display, Formatter, Result};

impl Display for super::Program {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self.kind {
            super::ProgramKind::Stateless => self.root_ii().fmt_with_indent(f, 0)?,
            super::ProgramKind::Stateful => {
                for (name, ii) in &self.iis {
                    if name == Self::ROOT_II_NAME {
                        writeln!(f, "{}", self.root_ii())?;
                    } else {
                        writeln!(f, "intent {name} {{")?;
                        ii.fmt_with_indent(f, 1)?;
                        writeln!(f, "}}\n")?;
                    }
                }
            }
        }

        Ok(())
    }
}

impl Display for super::IntermediateIntent {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.fmt_with_indent(f, 0)
    }
}

impl super::IntermediateIntent {
    fn fmt_with_indent(&self, f: &mut Formatter, indent: usize) -> Result {
        let indentation = " ".repeat(4 * indent);
        for var in &self.vars {
            writeln!(f, "{indentation}{};", self.with_ii(var.0))?;
        }
        for state in &self.states {
            writeln!(f, "{indentation}{};", self.with_ii(state.0))?;
        }
        for r#enum in &self.enums {
            writeln!(f, "{indentation}{};", self.with_ii(r#enum))?;
        }
        for new_type in &self.new_types {
            writeln!(f, "{indentation}{};", self.with_ii(new_type))?;
        }
        for constraint in &self.constraints {
            writeln!(f, "{indentation}constraint {};", self.with_ii(constraint.0))?;
        }
        for directive in &self.directives {
            writeln!(f, "{indentation}{};", self.with_ii(directive.0.clone()))?;
        }
        Ok(())
    }
}
