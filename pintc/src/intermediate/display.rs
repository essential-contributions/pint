use std::fmt::{Display, Formatter, Result};

impl Display for super::Program {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for (path, cnst) in &self.consts {
            writeln!(f, "const {path}{};", self.root_ii().with_ii(cnst))?;
        }

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

        if let Some(storage) = &self.storage {
            writeln!(f, "{indentation}storage {{")?;
            for storage_var in &storage.0 {
                writeln!(f, "{indentation}    {}", self.with_ii(storage_var))?;
            }
            writeln!(f, "{indentation}}}")?;
        }

        for super::Interface {
            name,
            storage,
            intent_interfaces,
            ..
        } in &self.interfaces
        {
            writeln!(f, "{indentation}interface {name} {{",)?;

            // Print storage
            if let Some(storage) = &storage {
                writeln!(f, "{indentation}    storage {{")?;
                for storage_var in &storage.0 {
                    writeln!(f, "{indentation}        {}", self.with_ii(storage_var))?;
                }
                writeln!(f, "{indentation}    }}")?;
            }

            // Print each intent interface
            for intent_interface in intent_interfaces {
                write!(f, "{indentation}    intent {}", intent_interface.name)?;

                if intent_interface.vars.is_empty() {
                    writeln!(f, ";")?;
                } else {
                    writeln!(f, " {{")?;
                    for var in &intent_interface.vars {
                        writeln!(
                            f,
                            "{indentation}        pub var {}: {};",
                            var.name,
                            self.with_ii(var.ty.clone())
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
                self.with_ii(address)
            )?;
        }

        for super::IntentInstance {
            name,
            interface_instance,
            intent,
            address,
            ..
        } in &self.intent_instances
        {
            writeln!(
                f,
                "{indentation}intent {name} = {interface_instance}::{intent}({})",
                self.with_ii(address)
            )?;
        }

        for (var_key, _) in self.vars() {
            writeln!(f, "{indentation}{};", self.with_ii(var_key))?;
        }

        for (state_key, _) in self.states() {
            writeln!(f, "{indentation}{};", self.with_ii(state_key))?;
        }

        for r#enum in &self.enums {
            writeln!(f, "{indentation}{};", self.with_ii(r#enum))?;
        }

        for new_type in &self.new_types {
            writeln!(f, "{indentation}{};", self.with_ii(new_type))?;
        }

        for constraint in &self.constraints {
            writeln!(f, "{indentation}{};", self.with_ii(constraint))?;
        }

        for if_decl in &self.if_decls {
            if_decl.fmt_with_indent(f, self, indent)?;
        }

        for directive in &self.directives {
            writeln!(f, "{indentation}{};", self.with_ii(directive.0.clone()))?;
        }

        Ok(())
    }
}
