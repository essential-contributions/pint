#[derive(Clone, Debug, PartialEq)]
pub(super) struct LetDecl {
    pub(super) name: Ident,
    pub(super) ty: Option<Type>,
    pub(super) init: Option<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct Block {
    pub(super) statements: Vec<Decl>,
    pub(super) final_expr: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Decl {
    Let(LetDecl),
    Constraint(Expr),
    Fn {
        name: Ident,
        params: Vec<(Ident, Type)>,
        return_type: Type,
        body: Block,
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
    String,
    Tuple(Vec<Type>),
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Expr {
    Immediate(Immediate),
    Ident(Ident),
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    BinaryOp {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Call {
        name: Ident,
        args: Vec<Expr>,
    },
    Block(Block),
    If(IfExpr),
    Tuple(Vec<Expr>),
    TupleIndex {
        tuple: Box<Expr>,
        index: usize,
    },
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
pub(super) enum SolveFunc {
    Satisfy,
    Minimize(Ident),
    Maximize(Ident),
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct IfExpr {
    pub(super) condition: Box<Expr>,
    pub(super) then_block: Block,
    pub(super) else_block: Block,
}
