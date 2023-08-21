use crate::{error::Span, types::FnSig};

#[derive(Clone, Debug, PartialEq)]
pub(super) struct InterfaceDecl<Type> {
    pub(super) name: String,
    pub(super) functions: Vec<FnSig<Type>>,
    pub(super) name_span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct ContractDecl<Ident, Expr, Type> {
    pub(super) name: String,
    pub(super) id: Expr,
    pub(super) interfaces: Vec<Ident>,
    pub(super) functions: Vec<FnSig<Type>>,
    pub(super) name_span: Span,
}
