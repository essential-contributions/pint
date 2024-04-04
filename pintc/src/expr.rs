use crate::{
    intermediate::{CallKey, ExprKey, VarKey},
    span::{Span, Spanned},
    types::{Path, Type},
};

use std::collections::HashMap;

mod display;
mod evaluate;

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
        range_expr: ExprKey,
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
    Generator {
        kind: GeneratorKind,
        gen_ranges: Vec<(Ident, ExprKey)>,
        conditions: Vec<ExprKey>,
        body: ExprKey,
        span: Span,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Ident {
    pub(super) name: String,
    pub(super) span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TupleAccess {
    Error,
    Index(usize),
    Name(Ident),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Immediate {
    Error,
    Real(f64),
    Int(i64),
    Bool(bool),
    String(String),
    B256([u64; 4]),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum UnaryOp {
    Error,
    Neg,
    Not,
    NextState,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // Comparison
    Equal,
    NotEqual,
    LessThanOrEqual,
    LessThan,
    GreaterThanOrEqual,
    GreaterThan,

    // Logical
    LogicalAnd,
    LogicalOr,
}

impl BinaryOp {
    pub(crate) fn as_str(&self) -> &'static str {
        match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::Equal => "==",
            BinaryOp::NotEqual => "!=",
            BinaryOp::LessThanOrEqual => "<=",
            BinaryOp::LessThan => "<",
            BinaryOp::GreaterThanOrEqual => ">=",
            BinaryOp::GreaterThan => ">",
            BinaryOp::LogicalAnd => "&&",
            BinaryOp::LogicalOr => "||",
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum GeneratorKind {
    ForAll,
    Exists,
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
            | Expr::Generator { span, .. }
            | Expr::Range { span, .. } => span,
        }
    }
}

impl Expr {
    pub fn replace_ref<F: FnMut(&mut ExprKey)>(&mut self, mut replace: F) {
        match self {
            Expr::UnaryOp { expr, .. } => replace(expr),
            Expr::BinaryOp { lhs, rhs, .. } => {
                replace(lhs);
                replace(rhs);
            }
            Expr::FnCall { args, .. } => args.iter_mut().for_each(replace),
            Expr::If {
                condition,
                then_block,
                else_block,
                ..
            } => {
                replace(condition);
                replace(then_block);
                replace(else_block);
            }
            Expr::Array { elements, .. } => elements.iter_mut().for_each(replace),
            Expr::ArrayElementAccess { array, index, .. } => {
                replace(array);
                replace(index);
            }
            Expr::Tuple { fields, .. } => fields.iter_mut().for_each(|(_, expr)| replace(expr)),
            Expr::TupleFieldAccess { tuple, .. } => replace(tuple),
            Expr::Cast { value, .. } => replace(value),
            Expr::In {
                value, collection, ..
            } => {
                replace(value);
                replace(collection);
            }
            Expr::Range { lb, ub, .. } => {
                replace(lb);
                replace(ub);
            }
            Expr::Generator {
                gen_ranges,
                conditions,
                body,
                ..
            } => {
                gen_ranges.iter_mut().for_each(|(_, expr)| replace(expr));
                conditions.iter_mut().for_each(&mut replace);
                replace(body);
            }

            Expr::MacroCall { .. }
            | Expr::PathByName(_, _)
            | Expr::PathByKey(_, _)
            | Expr::Immediate { .. }
            | Expr::Error(_) => {}
        }
    }

    pub fn replace_one_to_one(&mut self, old_key: ExprKey, new_key: ExprKey) {
        self.replace_ref(|expr: &mut ExprKey| {
            if *expr == old_key {
                *expr = new_key;
            }
        });
    }

    pub fn replace_ref_by_map(&mut self, keys: &HashMap<ExprKey, ExprKey>) {
        self.replace_ref(|old_key: &mut ExprKey| {
            if let Some(new_key) = keys.get(old_key) {
                *old_key = *new_key;
            }
        });
    }
}
