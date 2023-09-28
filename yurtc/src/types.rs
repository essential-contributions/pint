use crate::{
    ast::Ident,
    span::{Span, Spanned},
};

mod display;

#[derive(Clone, Debug, PartialEq)]
pub enum PrimitiveKind {
    Bool,
    Int,
    Real,
    String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type<Path, Expr> {
    Primitive {
        kind: PrimitiveKind,
        span: Span,
    },
    Array {
        ty: Box<Self>,
        range: Expr,
        span: Span,
    },
    Tuple {
        fields: Vec<(Option<Ident>, Self)>,
        span: Span,
    },
    CustomType {
        path: Path,
        span: Span,
    },
}

impl<Path, Expr> Spanned for Type<Path, Expr> {
    fn span(&self) -> &Span {
        use Type::*;
        match &self {
            Primitive { span, .. }
            | Array { span, .. }
            | Tuple { span, .. }
            | CustomType { span, .. } => span,
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
pub struct FnSig<Type> {
    pub(super) name: Ident,
    pub(super) params: Vec<(Ident, Type)>,
    pub(super) return_type: Type,
    pub(super) span: Span,
}

impl<Type> Spanned for FnSig<Type> {
    fn span(&self) -> &Span {
        &self.span
    }
}
