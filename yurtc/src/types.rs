use crate::error::Span;

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Type<Ident, Expr> {
    Bool,
    Int,
    Real,
    String,
    Array { ty: Box<Self>, range: Expr },
    Tuple(Vec<(Option<String>, Self)>),
    CustomType(Ident),
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct EnumDecl {
    pub(super) name: String,
    pub(super) variants: Vec<String>,
    pub(super) name_span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct FnSig<Type> {
    pub(super) name: String,
    pub(super) params: Vec<(String, Type)>,
    pub(super) return_type: Type,
    pub(super) span: Span,
}
