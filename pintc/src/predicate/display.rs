use std::fmt::{Display, Formatter, Result};

impl Display for super::Contract {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for (path, cnst) in &self.consts {
            writeln!(f, "const {path}{};", self.root_pred().with_pred(self, cnst))?;
        }

        for r#enum in &self.enums {
            writeln!(f, "{};", self.root_pred().with_pred(self, r#enum))?;
        }

        for new_type in &self.new_types {
            writeln!(f, "{};", self.root_pred().with_pred(self, new_type))?;
        }

        if let Some(storage) = &self.storage {
            writeln!(f, "storage {{")?;
            for storage_var in &storage.0 {
                writeln!(f, "    {}", self.root_pred().with_pred(self, storage_var))?;
            }
            writeln!(f, "}}")?;
        }

        for pred in self.preds.values() {
            if pred.name == Self::ROOT_PRED_NAME {
                self.root_pred().fmt_with_indent(f, self, 0)?
            } else {
                writeln!(f, "\npredicate {} {{", pred.name)?;
                pred.fmt_with_indent(f, self, 1)?;
                writeln!(f, "}}")?;
            }
        }

        Ok(())
    }
}

//impl Display for super::Predicate {
//    fn fmt(&self, f: &mut Formatter) -> Result {
//        self.fmt_with_indent(f, 0)
//    }
//}

impl super::Predicate {
    fn fmt_with_indent(
        &self,
        f: &mut Formatter,
        contract: &super::Contract,
        indent: usize,
    ) -> Result {
        let indentation = " ".repeat(4 * indent);

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
                    writeln!(
                        f,
                        "{indentation}        {}",
                        self.with_pred(contract, storage_var)
                    )?;
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
                            self.with_pred(contract, var.ty.clone())
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
                self.with_pred(contract, address)
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
                self.with_pred(contract, address)
            )?;
        }

        for (var_key, _) in self.vars() {
            writeln!(f, "{indentation}{};", self.with_pred(contract, var_key))?;
        }

        for (state_key, _) in self.states() {
            writeln!(f, "{indentation}{};", self.with_pred(contract, state_key))?;
        }

        for constraint in &self.constraints {
            writeln!(f, "{indentation}{};", self.with_pred(contract, constraint))?;
        }

        for if_decl in &self.if_decls {
            if_decl.fmt_with_indent(f, contract, self, indent)?;
        }

        Ok(())
    }
}
