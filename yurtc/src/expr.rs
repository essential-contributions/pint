use crate::{
    intent::intermediate::{ExprKey, VarKey, CallKey},
    span::{Span, Spanned},
    types::{Path, Type},
};

mod display;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Error(Span),
    Immediate {
        value: Immediate,
        span: Span,
    },
    PathByKey(VarKey, Span),
    PathByName(Path, Span),
    UnaryOp {
        op: UnaryOp,
        expr: ExprKey,
        span: Span,
    },
    BinaryOp {
        op: BinaryOp,
        lhs: ExprKey,
        rhs: ExprKey,
        span: Span,
    },
    MacroCall {
        call: CallKey,
        span: Span,
    },
    FnCall {
        name: Path,
        args: Vec<ExprKey>,
        span: Span,
    },
    If {
        condition: ExprKey,
        then_block: ExprKey,
        else_block: ExprKey,
        span: Span,
    },
    Array {
        elements: Vec<ExprKey>,
        span: Span,
    },
    ArrayElementAccess {
        array: ExprKey,
        index: ExprKey,
        span: Span,
    },
    Tuple {
        fields: Vec<(Option<Ident>, ExprKey)>,
        span: Span,
    },
    TupleFieldAccess {
        tuple: ExprKey,
        field: TupleAccess,
        span: Span,
    },
    Cast {
        value: ExprKey,
        ty: Box<Type>,
        span: Span,
    },
    In {
        value: ExprKey,
        collection: ExprKey,
        span: Span,
    },
    Range {
        lb: ExprKey,
        ub: ExprKey,
        span: Span,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Ident {
    pub(super) name: String,
    pub(super) span: Span,
}

impl Ident {
    pub(crate) fn to_full_path(&self, mod_prefix: &str) -> Self {
        Ident {
            name: mod_prefix.to_owned() + &self.name,
            span: self.span.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TupleAccess {
    Error,
    Index(usize),
    Name(Ident),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Immediate {
    Real(f64),
    Int(i64),
    BigInt(num_bigint::BigInt),
    Bool(bool),
    String(String),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum UnaryOp {
    Pos,
    Neg,
    Not,
    NextState,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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

impl Spanned for Expr {
    fn span(&self) -> &Span {
        match self {
            Expr::Error(span)
            | Expr::Immediate { span, .. }
            | Expr::PathByKey(_, span)
            | Expr::PathByName(_, span)
            | Expr::UnaryOp { span, .. }
            | Expr::BinaryOp { span, .. }
            | Expr::MacroCall { span, .. }
            | Expr::FnCall { span, .. }
            | Expr::If { span, .. }
            | Expr::Array { span, .. }
            | Expr::ArrayElementAccess { span, .. }
            | Expr::Tuple { span, .. }
            | Expr::TupleFieldAccess { span, .. }
            | Expr::Cast { span, .. }
            | Expr::In { span, .. }
            | Expr::Range { span, .. } => span,
        }
    }
}
