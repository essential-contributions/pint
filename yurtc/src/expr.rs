#[derive(Clone, Debug, PartialEq)]
pub(super) enum Expr<Ident, BlockExpr> {
    Immediate(Immediate),
    Ident(Ident),
    UnaryOp {
        op: UnaryOp,
        expr: Box<Self>,
    },
    BinaryOp {
        op: BinaryOp,
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    Call {
        name: Ident,
        args: Vec<Self>,
    },
    Block(BlockExpr),
    If {
        condition: Box<Self>,
        then_block: BlockExpr,
        else_block: BlockExpr,
    },
    Cond {
        branches: Vec<CondBranch<Self>>,
        else_result: Box<Self>,
    },
    Array(Vec<Self>),
    ArrayElementAccess {
        array: Box<Self>,
        index: Box<Self>,
    },
    Tuple(Vec<(Option<String>, Self)>),
    TupleFieldAccess {
        tuple: Box<Self>,
        field: itertools::Either<usize, String>,
    },
    Cast {
        value: Box<Self>,
        ty: Box<super::types::Type<Ident, Self>>,
    },
    In {
        value: Box<Self>,
        collection: Box<Self>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Immediate {
    Real(f64),
    Int(i64),
    BigInt(num_bigint::BigInt),
    Bool(bool),
    String(String),
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct CondBranch<E> {
    pub(super) condition: Box<E>,
    pub(super) result: Box<E>,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum UnaryOp {
    Pos,
    Neg,
    Not,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum BinaryOp {
    Mul,
    Div,
    Add,
    Sub,
    Mod,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
    LogicalAnd,
    LogicalOr,
}
