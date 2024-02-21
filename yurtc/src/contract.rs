use crate::{
    expr::Ident,
    intermediate::{DisplayWithII, ExprKey, IntermediateIntent},
    span::{Span, Spanned},
    types::{FnSig, Path},
    util::write_many,
};
use std::fmt::{Formatter, Result};

#[derive(Clone, Debug, PartialEq)]
pub struct InterfaceDecl {
    pub(super) name: Ident,
    pub(super) functions: Vec<FnSig>,
    pub(super) span: Span,
}

impl Spanned for InterfaceDecl {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl DisplayWithII for InterfaceDecl {
    fn fmt(&self, f: &mut Formatter<'_>, ii: &IntermediateIntent) -> Result {
        write!(f, "interface {} {{ ", self.name)?;
        for function in &self.functions {
            write!(f, "{}; ", ii.with_ii(function))?;
        }
        write!(f, "}}")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ContractDecl {
    pub(super) name: Ident,
    pub(super) id: ExprKey,
    pub(super) interfaces: Vec<Path>,
    pub(super) functions: Vec<FnSig>,
    pub(super) span: Span,
}

impl Spanned for ContractDecl {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl DisplayWithII for ContractDecl {
    fn fmt(&self, f: &mut Formatter<'_>, ii: &IntermediateIntent) -> Result {
        write!(f, "contract {}({})", self.name, ii.with_ii(self.id))?;
        if !self.interfaces.is_empty() {
            write!(f, " implements ")?;
            write_many!(f, self.interfaces, ", ");
        }
        write!(f, " {{ ")?;
        for function in &self.functions {
            write!(f, "{}; ", ii.with_ii(function))?;
        }
        write!(f, "}}")
    }
}
