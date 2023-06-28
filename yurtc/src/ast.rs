#[derive(Clone, Debug, PartialEq)]
pub(super) struct LetStatement {
    pub(super) name: Ident,
    pub(super) ty: Option<Type>,
    pub(super) init: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct CodeBlock {
    pub(super) statements: Vec<LetStatement>,
    pub(super) final_expr: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Decl {
    Value(LetStatement),
    Constraint(Expr),
    Fn {
        name: Ident,
        params: Vec<(Ident, Type)>,
        return_type: Type,
        body: CodeBlock,
    },
    Solve(SolveFunc),
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct Ident(pub(super) String);

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Type {
    Real,
    Int,
    Bool,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Expr {
    Immediate(Immediate),
    Ident(Ident),
    BinaryOp {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Call {
        name: Ident,
        args: Vec<Expr>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum BinaryOp {
    Mul,

    LessThan,
    GreaterThan,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Immediate {
    Real(f64),
    Integer(i64),
    Boolean(bool),
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum SolveFunc {
    Satisfy,
    Minimize(Ident),
    Maximize(Ident),
}
