use crate::ast::Ident;
use crate::error::Span;

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Type<Path, Expr> {
    Bool,
    Int,
    Real,
    String,
    Array { ty: Box<Self>, range: Expr },
    Tuple(Vec<(Option<Ident>, Self)>),
    CustomType(Path),
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct EnumDecl {
    pub(super) name: Ident,
    pub(super) variants: Vec<Ident>,
    pub(super) span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct FnSig<Type> {
    pub(super) name: Ident,
    pub(super) params: Vec<(Ident, Type)>,
    pub(super) return_type: Type,
    pub(super) span: Span,
}
