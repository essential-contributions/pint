use crate::{
    ast::Ident,
    span::{Span, Spanned},
};

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Expr<Path, BlockExpr> {
    Immediate {
        value: Immediate,
        span: Span,
    },
    Path(Path),
    UnaryOp {
        op: UnaryOp,
        expr: Box<Self>,
        span: Span,
    },
    BinaryOp {
        op: BinaryOp,
        lhs: Box<Self>,
        rhs: Box<Self>,
        span: Span,
    },
    Call {
        name: Path,
        args: Vec<Self>,
        span: Span,
    },
    Block(BlockExpr),
    If {
        condition: Box<Self>,
        then_block: BlockExpr,
        else_block: BlockExpr,
        span: Span,
    },
    Cond {
        branches: Vec<CondBranch<Self>>,
        else_result: Box<Self>,
        span: Span,
    },
    Array {
        elements: Vec<Self>,
        span: Span,
    },
    ArrayElementAccess {
        array: Box<Self>,
        index: Box<Self>,
        span: Span,
    },
    Tuple {
        fields: Vec<(Option<Ident>, Self)>,
        span: Span,
    },
    TupleFieldAccess {
        tuple: Box<Self>,
        field: TupleAccess,
        span: Span,
    },
    Cast {
        value: Box<Self>,
        ty: Box<super::types::Type<Path, Self>>,
        span: Span,
    },
    In {
        value: Box<Self>,
        collection: Box<Self>,
        span: Span,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum TupleAccess {
    Index(usize),
    Name(Ident),
}

impl<Path, BlockExpr> Spanned for Expr<Path, BlockExpr>
where
    Path: Spanned,
    BlockExpr: Spanned,
{
    fn span(&self) -> &Span {
        use Expr::*;
        match &self {
            Immediate { span, .. }
            | UnaryOp { span, .. }
            | BinaryOp { span, .. }
            | Call { span, .. }
            | If { span, .. }
            | Cond { span, .. }
            | Array { span, .. }
            | ArrayElementAccess { span, .. }
            | Tuple { span, .. }
            | TupleFieldAccess { span, .. }
            | Cast { span, .. }
            | In { span, .. } => span,
            Path(path) => path.span(),
            Block(block) => block.span(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Immediate {
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
    pub(super) span: Span,
}

impl<E> Spanned for CondBranch<E> {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum UnaryOp {
    Pos,
    Neg,
    Not,
    NextState,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum BinaryOp {
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
