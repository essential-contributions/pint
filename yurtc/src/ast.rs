#[derive(Clone, Debug, PartialEq)]
pub(super) enum AstDecl {
    Value {
        name: AstIdent,
        ty: Option<AstType>,
        init: AstExpr,
    },
    Constraint(AstExpr),
    Solve(AstSolveFunc),
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct AstIdent(pub(super) String);

#[derive(Clone, Debug, PartialEq)]
pub(super) enum AstType {
    Real,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum AstExpr {
    Immediate(AstImmediate),
    Ident(AstIdent),
    BinaryOp {
        op: AstBinaryOp,
        lhs: Box<AstExpr>,
        rhs: Box<AstExpr>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum AstBinaryOp {
    Mul,

    LessThan,
    GreaterThan,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum AstImmediate {
    Real(f64),
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum AstSolveFunc {
    Satisfy,
    Minimize(AstIdent),
    Maximize(AstIdent),
}
