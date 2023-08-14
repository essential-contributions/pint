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
