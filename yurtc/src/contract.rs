use crate::{
    ast::Ident,
    span::{Span, Spanned},
    types::FnSig,
};

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
