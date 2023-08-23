use crate::{ast::Ident, error::Span, types::FnSig};

#[derive(Clone, Debug, PartialEq)]
pub(super) struct InterfaceDecl<Type> {
    pub(super) name: Ident,
    pub(super) functions: Vec<FnSig<Type>>,
    pub(super) span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct ContractDecl<Path, Expr, Type> {
    pub(super) name: Ident,
    pub(super) id: Expr,
    pub(super) interfaces: Vec<Path>,
    pub(super) functions: Vec<FnSig<Type>>,
    pub(super) span: Span,
}
