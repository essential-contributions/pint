use super::*;

use std::fmt::{Display, Formatter, Result};

#[derive(Clone, Copy)]
pub struct WithContract<'a, T> {
    pub thing: T,
    pub contract: &'a Contract,
}

impl<'a, T> WithContract<'a, T> {
    pub fn new(thing: T, contract: &'a Contract) -> Self {
        WithContract { thing, contract }
    }
}

impl Contract {
    /// Helps out some `thing: T` by adding `self` as context.
    pub fn with_ctrct<T>(&self, thing: T) -> WithContract<T> {
        WithContract {
            thing,
            contract: self,
        }
    }
}

pub(crate) trait DisplayWithContract {
    fn fmt(&self, f: &mut Formatter, contract: &Contract) -> Result;
}

impl<T: DisplayWithContract> Display for WithContract<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.thing.fmt(f, self.contract)
    }
}

impl<T: DisplayWithContract> DisplayWithContract for &T {
    fn fmt(&self, f: &mut Formatter, contract: &Contract) -> Result {
        (*self).fmt(f, contract)
    }
}

#[derive(Clone, Copy)]
pub struct WithPred<'a, T> {
    pub thing: T,
    pub contract: &'a Contract,
    pub pred: &'a Predicate,
}

impl<'a, T> WithPred<'a, T> {
    pub fn new(thing: T, contract: &'a Contract, pred: &'a Predicate) -> Self {
        WithPred {
            thing,
            contract,
            pred,
        }
    }
}

impl Predicate {
    /// Helps out some `thing: T` by adding `self` as context.
    pub fn with_pred<'a, T>(&'a self, contract: &'a Contract, thing: T) -> WithPred<T> {
        WithPred {
            thing,
            contract,
            pred: self,
        }
    }
}

pub(crate) trait DisplayWithPred {
    fn fmt(&self, f: &mut Formatter, contract: &Contract, pred: &Predicate) -> Result;
}

impl<T: DisplayWithPred> Display for WithPred<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.thing.fmt(f, self.contract, self.pred)
    }
}

impl<T: DisplayWithPred> DisplayWithPred for &T {
    fn fmt(&self, f: &mut Formatter, contract: &Contract, pred: &Predicate) -> Result {
        (*self).fmt(f, contract, pred)
    }
}

impl Display for Contract {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for (path, cnst) in &self.consts {
            writeln!(f, "const {path}{};", self.with_ctrct(cnst))?;
        }

        for r#union in self.unions.values() {
            writeln!(f, "{};", self.with_ctrct(union))?;
        }

        for new_type in &self.new_types {
            writeln!(f, "{};", self.with_ctrct(new_type))?;
        }

        if let Some(storage) = &self.storage {
            writeln!(f, "storage {{")?;
            for storage_var in &storage.0 {
                writeln!(f, "    {}", self.with_ctrct(storage_var))?;
            }
            writeln!(f, "}}")?;
        }

        self.fmt_interfaces(f)?;

        for pred in self.preds.values() {
            writeln!(f, "\npredicate {}(", pred.name)?;
            for param in &pred.params {
                writeln!(
                    f,
                    "    {}: {},",
                    param.name.name,
                    self.with_ctrct(param.ty.clone())
                )?;
            }

            writeln!(f, ") {{")?;
            pred.fmt_with_indent(f, self, 1)?;
            writeln!(f, "}}")?;
        }

        Ok(())
    }
}

impl Contract {
    fn fmt_interfaces(&self, f: &mut Formatter) -> Result {
        for Interface {
            name,
            storage,
            predicate_interfaces,
            ..
        } in &self.interfaces
        {
            writeln!(f, "interface {name} {{",)?;

            // Print storage
            if let Some(storage) = &storage {
                writeln!(f, "    storage {{")?;
                for storage_var in &storage.0 {
                    writeln!(f, "        {}", self.with_ctrct(storage_var))?;
                }
                writeln!(f, "    }}")?;
            }

            // Print each predicate interface
            for predicate_interface in predicate_interfaces {
                write!(f, "    predicate {}", predicate_interface.name)?;

                if predicate_interface.params.is_empty() {
                    writeln!(f, "();")?;
                } else {
                    writeln!(f, " (")?;
                    for param in &predicate_interface.params {
                        writeln!(
                            f,
                            "        {}: {},",
                            param.name,
                            self.with_ctrct(param.ty.clone())
                        )?;
                    }
                    writeln!(f, "    );")?;
                }
            }

            writeln!(f, "}}")?;
        }

        Ok(())
    }
}

impl Predicate {
    fn fmt_with_indent(&self, f: &mut Formatter, contract: &Contract, indent: usize) -> Result {
        let indentation = " ".repeat(4 * indent);

        for (state_key, _) in self.states() {
            writeln!(f, "{indentation}{};", self.with_pred(contract, state_key))?;
        }

        for constraint in &self.constraints {
            writeln!(f, "{indentation}{};", contract.with_ctrct(constraint))?;
        }

        for if_decl in &self.if_decls {
            if_decl.fmt_with_indent(f, contract, self, indent)?;
        }

        for match_decl in &self.match_decls {
            match_decl.fmt_with_indent(f, contract, self, indent)?;
        }

        Ok(())
    }
}

impl DisplayWithContract for Const {
    fn fmt(&self, f: &mut Formatter, contract: &Contract) -> Result {
        if !self.decl_ty.is_unknown() {
            write!(f, ": {}", contract.with_ctrct(&self.decl_ty))?;
        }

        write!(f, " = {}", contract.with_ctrct(self.expr))
    }
}

impl DisplayWithContract for ConstraintDecl {
    fn fmt(&self, f: &mut Formatter, contract: &Contract) -> Result {
        write!(f, "constraint {}", contract.with_ctrct(self.expr))
    }
}

impl DisplayWithContract for StorageVar {
    fn fmt(&self, f: &mut Formatter, contract: &Contract) -> Result {
        write!(f, "{}: {},", self.name.name, contract.with_ctrct(&self.ty))
    }
}

impl DisplayWithPred for IfDecl {
    fn fmt(&self, f: &mut Formatter, contract: &Contract, pred: &Predicate) -> Result {
        self.fmt_with_indent(f, contract, pred, 0)
    }
}

impl DisplayWithPred for MatchDecl {
    fn fmt(&self, f: &mut Formatter, contract: &Contract, pred: &Predicate) -> Result {
        self.fmt_with_indent(f, contract, pred, 0)
    }
}
