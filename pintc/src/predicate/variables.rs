use super::{Contract, DisplayWithPred, ExprKey, Ident, Predicate};
use crate::{
    error::{ErrorEmitted, Handler},
    span::Span,
    types::Type,
};
use std::fmt::{self, Formatter};

/// A variable specification with an optional type.
#[derive(Clone, Debug)]
pub struct Variable {
    pub name: String,
    pub expr: ExprKey,
    pub span: Span,
}

slotmap::new_key_type! { pub struct VariableKey; }

#[derive(Debug, Default, Clone)]
pub struct Variables {
    variables: slotmap::SlotMap<VariableKey, Variable>,
    variable_types: slotmap::SecondaryMap<VariableKey, Type>,
    order: Vec<VariableKey>,
}

impl Variables {
    /// Returns a read-only iterator to the `variables` map
    pub fn variables(&self) -> impl Iterator<Item = (VariableKey, &Variable)> {
        self.order.iter().map(|&key| (key, &self.variables[key]))
    }

    /// Returns the order of the provided `VariableKey` as tracked in the `order` vector
    pub fn position(&self, key: VariableKey) -> Option<usize> {
        self.order.iter().position(|k| *k == key)
    }

    /// Inserts a variable variable with its type
    pub fn insert(&mut self, variable: Variable, ty: Type) -> VariableKey {
        let key = self.variables.insert(variable);
        self.variable_types.insert(key, ty);
        self.order.push(key);
        key
    }

    /// Inserts a variable variable with its type at a particular position
    pub fn insert_at(&mut self, index: usize, variable: Variable, ty: Type) -> VariableKey {
        let key = self.variables.insert(variable);
        self.variable_types.insert(key, ty);
        self.order.insert(index, key);
        key
    }

    /// Removes a variable variable and its type given a `VariableKey`
    pub fn remove(&mut self, key: VariableKey) {
        self.variables.remove(key);
        self.variable_types.remove(key);
        self.order.retain(|el| *el != key);
    }

    /// Apply function `f` on every variable
    pub fn update_variables(&mut self, f: impl FnOnce(&mut Variable) + std::marker::Copy) {
        for (_, variable) in self.variables.iter_mut() {
            f(variable)
        }
    }

    /// Apply function `f` on every variable type
    /// Only the `Type`s can be mutated, not the keys
    pub fn update_types(&mut self, f: impl FnOnce(VariableKey, &mut Type) + std::marker::Copy) {
        for (key, ty) in self.variable_types.iter_mut() {
            f(key, ty)
        }
    }
}

impl VariableKey {
    /// Returns an `Option` containing the `Variable` corresponding to key `self`. Returns `None` if
    /// the key can't be found in the `variables` map.
    pub fn try_get<'a>(&'a self, pred: &'a Predicate) -> Option<&Variable> {
        pred.variables.variables.get(*self)
    }

    /// Returns the `Variable` corresponding to key `self`. Panics if the key can't be found in the
    /// `variables` map.
    pub fn get<'a>(&'a self, pred: &'a Predicate) -> &Variable {
        pred.variables.variables.get(*self).unwrap()
    }

    /// Returns the type of key `self` given a `Predicate`. Panics if the type can't be
    /// found in the `variable_types` map.
    pub fn get_ty<'a>(&'a self, pred: &'a Predicate) -> &Type {
        pred.variables.variable_types.get(*self).unwrap()
    }

    /// Set the type of key `self` in a `Predicate`. Panics if the type can't be found in
    /// the `variable_types` map.
    pub fn set_ty<'a>(&'a self, ty: Type, pred: &'a mut Predicate) {
        pred.variables.variable_types.insert(*self, ty);
    }
}

impl DisplayWithPred for VariableKey {
    fn fmt(&self, f: &mut Formatter, contract: &Contract, pred: &Predicate) -> fmt::Result {
        let variable = &self.get(pred);
        write!(f, "let {}", variable.name)?;
        let ty = self.get_ty(pred);
        if !ty.is_unknown() {
            write!(f, ": {}", contract.with_ctrct(ty))?;
        }
        write!(f, " = {}", contract.with_ctrct(&variable.expr))
    }
}

impl Predicate {
    #[allow(clippy::too_many_arguments)]
    pub fn insert_variable(
        &mut self,
        handler: &Handler,
        mod_prefix: &str,
        local_prefix: Option<&str>,
        name: &Ident,
        ty: Option<Type>,
        expr: ExprKey,
        span: Span,
    ) -> std::result::Result<VariableKey, ErrorEmitted> {
        let name =
            self.symbols
                .add_symbol(handler, mod_prefix, local_prefix, name, span.clone())?;
        let variable_key = self.variables.insert(
            Variable {
                name,
                expr,
                span: span.clone(),
            },
            if let Some(ty) = ty {
                ty
            } else {
                Type::Unknown(span.clone())
            },
        );

        Ok(variable_key)
    }

    pub(crate) fn variables(&self) -> impl Iterator<Item = (VariableKey, &Variable)> {
        self.variables.variables()
    }
}
