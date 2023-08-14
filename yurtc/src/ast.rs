use crate::error::Span;

use itertools::Either;

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Decl {
    Use {
        is_absolute: bool,
        use_tree: UseTree,
    },
    Let {
        name: String,
        ty: Option<Type>,
        init: Option<Expr>,
        span: Span,
    },
    Constraint {
        expr: Expr,
        span: Span,
    },
    Fn {
        fn_sig: FnSig,
        body: Block,
    },
    Solve {
        directive: SolveFunc,
        span: Span,
    },
    Enum {
        name: String,
        variants: Vec<String>,
        name_span: Span,
    },
    Interface {
        name: String,
        functions: Vec<FnSig>,
        name_span: Span,
    },
    Contract {
        name: String,
        id: Expr,
        interfaces: Vec<Ident>,
        functions: Vec<FnSig>,
        name_span: Span,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum UseTree {
    Glob,
    Name {
        name: String,
    },
    Path {
        prefix: String,
        suffix: Box<UseTree>,
    },
    Group {
        imports: Vec<UseTree>,
    },
    Alias {
        name: String,
        alias: String,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct Block {
    pub(super) statements: Vec<Decl>,
    pub(super) final_expr: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct FnSig {
    pub(super) name: String,
    pub(super) params: Vec<(String, Type)>,
    pub(super) return_type: Type,
    pub(super) span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct Ident {
    pub(super) path: Vec<String>,
    pub(super) is_absolute: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Type {
    Real,
    Int,
    Bool,
    String,
    Array { ty: Box<Type>, range: Expr },
    Tuple(Vec<(Option<String>, Type)>),
    CustomType(Ident),
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
    Cond(CondExpr),
    Array(Vec<Expr>),
    ArrayElementAccess {
        array: Box<Expr>,
        index: Box<Expr>,
    },
    Tuple(Vec<(Option<String>, Expr)>),
    TupleFieldAccess {
        tuple: Box<Expr>,
        field: Either<usize, String>,
    },
    Cast {
        value: Box<Expr>,
        ty: Box<Type>,
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
    LogicalAnd,
    LogicalOr,
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

#[derive(Clone, Debug, PartialEq)]
pub(super) struct CondBranch {
    pub(super) condition: Box<Expr>,
    pub(super) result: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct CondExpr {
    pub(super) branches: Vec<CondBranch>,
    pub(super) else_result: Box<Expr>,
}
