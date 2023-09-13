use crate::{
    ast::Ident,
    span::{Span, Spanned},
    types::FnSig,
    util::write_many,
};

use std::fmt::{Display, Formatter, Result};

#[derive(Clone, Debug, PartialEq)]
pub(super) struct InterfaceDecl<Type> {
    pub(super) name: Ident,
    pub(super) functions: Vec<FnSig<Type>>,
    pub(super) span: Span,
}

impl<Type> Spanned for InterfaceDecl<Type> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<Type: Display> Display for InterfaceDecl<Type> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "interface {} {{ ", self.name)?;
        for function in &self.functions {
            write!(f, "{function}; ")?;
        }
        write!(f, "}}")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct ContractDecl<Path, Expr, Type> {
    pub(super) name: Ident,
    pub(super) id: Expr,
    pub(super) interfaces: Vec<Path>,
    pub(super) functions: Vec<FnSig<Type>>,
    pub(super) span: Span,
}

impl<Path, Expr, Type> Spanned for ContractDecl<Path, Expr, Type> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<Path: Display, Expr: Display, Type: Display> Display for ContractDecl<Path, Expr, Type> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "contract {}({})", self.name, self.id)?;
        if !self.interfaces.is_empty() {
            write!(f, " implements ")?;
            write_many!(f, self.interfaces, ", ");
        }
        write!(f, " {{ ")?;
        for function in &self.functions {
            write!(f, "{function}; ")?;
        }
        write!(f, "}}")
    }
}
