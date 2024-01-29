use crate::{
    expr::Ident,
    intent::intermediate::ExprKey,
    span::{Span, Spanned},
};

mod display;

pub type Path = String;

#[derive(Clone, Debug, PartialEq)]
pub enum PrimitiveKind {
    Bool,
    Int,
    Real,
    String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Error(Span),
    Primitive {
        kind: PrimitiveKind,
        span: Span,
    },
    Array {
        ty: Box<Self>,
        range: ExprKey,
        span: Span,
    },
    Tuple {
        fields: Vec<(Option<Ident>, Self)>,
        span: Span,
    },
    Custom {
        path: Path,
        span: Span,
    },
}

impl Spanned for Type {
    fn span(&self) -> &Span {
        use Type::*;
        match &self {
            Error(span)
            | Primitive { span, .. }
            | Array { span, .. }
            | Tuple { span, .. }
            | Custom { span, .. } => span,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumDecl {
    pub(super) name: Ident,
    pub(super) variants: Vec<Ident>,
    pub(super) span: Span,
}

impl Spanned for EnumDecl {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct NewTypeDecl {
    pub(super) name: Ident,
    pub(super) ty: Type,
    pub(super) span: Span,
}

impl Spanned for NewTypeDecl {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnSig {
    pub(super) name: Ident,
    pub(super) params: Vec<(Ident, Type)>,
    pub(super) return_type: Type,
    pub(super) span: Span,
}

impl Spanned for FnSig {
    fn span(&self) -> &Span {
        &self.span
    }
}
