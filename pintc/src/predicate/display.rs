use std::fmt::{Display, Formatter, Result};

impl Display for super::Program {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for (path, cnst) in &self.consts {
            writeln!(f, "const {path}{};", self.root_pred().with_pred(cnst))?;
        }

        match self.kind {
            super::ProgramKind::Stateless => self.root_pred().fmt_with_indent(f, 0)?,
            super::ProgramKind::Stateful => {
                for (name, pred) in &self.preds {
                    if name == Self::ROOT_PRED_NAME {
                        writeln!(f, "{}", self.root_pred())?;
                    } else {
                        writeln!(f, "predicate {name} {{")?;
                        pred.fmt_with_indent(f, 1)?;
                        writeln!(f, "}}\n")?;
                    }
                }
            }
        }

        Ok(())
    }
}

impl Display for super::Predicate {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.fmt_with_indent(f, 0)
    }
}

impl super::Predicate {
    fn fmt_with_indent(&self, f: &mut Formatter, indent: usize) -> Result {
        let indentation = " ".repeat(4 * indent);

        if let Some(storage) = &self.storage {
            writeln!(f, "{indentation}storage {{")?;
            for storage_var in &storage.0 {
                writeln!(f, "{indentation}    {}", self.with_pred(storage_var))?;
            }
            writeln!(f, "{indentation}}}")?;
        }

        for super::Interface {
            name,
            storage,
            predicate_interfaces,
            ..
        } in &self.interfaces
        {
            writeln!(f, "{indentation}interface {name} {{",)?;

            // Print storage
            if let Some(storage) = &storage {
                writeln!(f, "{indentation}    storage {{")?;
                for storage_var in &storage.0 {
                    writeln!(f, "{indentation}        {}", self.with_pred(storage_var))?;
                }
                writeln!(f, "{indentation}    }}")?;
            }

            // Print each predicate interface
            for predicate_interface in predicate_interfaces {
                write!(f, "{indentation}    predicate {}", predicate_interface.name)?;

                if predicate_interface.vars.is_empty() {
                    writeln!(f, ";")?;
                } else {
                    writeln!(f, " {{")?;
                    for var in &predicate_interface.vars {
                        writeln!(
                            f,
                            "{indentation}        pub var {}: {};",
                            var.name,
                            self.with_pred(var.ty.clone())
                        )?;
                    }
                    writeln!(f, "{indentation}    }}")?;
                }
            }

            writeln!(f, "{indentation}}}")?;
        }

        for super::InterfaceInstance {
            name,
            interface,
            address,
            ..
        } in &self.interface_instances
        {
            writeln!(
                f,
                "{indentation}interface {name} = {interface}({})",
                self.with_pred(address)
            )?;
        }

        for super::PredicateInstance {
            name,
            interface_instance,
            predicate,
            address,
            ..
        } in &self.predicate_instances
        {
            writeln!(
                f,
                "{indentation}predicate {name} = {interface_instance}::{predicate}({})",
                self.with_pred(address)
            )?;
        }

        for (var_key, _) in self.vars() {
            writeln!(f, "{indentation}{};", self.with_pred(var_key))?;
        }

        for (state_key, _) in self.states() {
            writeln!(f, "{indentation}{};", self.with_pred(state_key))?;
        }

        for r#enum in &self.enums {
            writeln!(f, "{indentation}{};", self.with_pred(r#enum))?;
        }

        for new_type in &self.new_types {
            writeln!(f, "{indentation}{};", self.with_pred(new_type))?;
        }

        for constraint in &self.constraints {
            writeln!(f, "{indentation}{};", self.with_pred(constraint))?;
        }

        for if_decl in &self.if_decls {
            if_decl.fmt_with_indent(f, self, indent)?;
        }

        for directive in &self.directives {
            writeln!(f, "{indentation}{};", self.with_pred(directive.0.clone()))?;
        }

        Ok(())
    }
}
