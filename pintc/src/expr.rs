use crate::{
    intermediate::{CallKey, ExprKey, VarKey},
    span::{empty_span, Span, Spanned},
    types::{Path, PrimitiveKind, Type},
};

use fxhash::FxHashMap;

mod display;
pub(crate) mod evaluate;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Error(Span),
    Immediate {
        value: Immediate,
        span: Span,
    },
    Array {
        elements: Vec<ExprKey>,
        range_expr: ExprKey,
        span: Span,
    },
    Tuple {
        fields: Vec<(Option<Ident>, ExprKey)>,
        span: Span,
    },
    PathByKey(VarKey, Span),
    PathByName(Path, Span),
    StorageAccess(String, Span),
    ExternalStorageAccess {
        interface_instance: Path,
        name: String,
        span: Span,
    },
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
    IntrinsicCall {
        name: Ident,
        args: Vec<ExprKey>,
        span: Span,
    },
    Select {
        condition: ExprKey,
        then_expr: ExprKey,
        else_expr: ExprKey,
        span: Span,
    },
    Index {
        expr: ExprKey,
        index: ExprKey,
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
    pub(super) hygienic: bool,
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
    Nil,
    Real(f64),
    Int(i64),
    Bool(bool),
    String(String),
    B256([u64; 4]),
    Array(Vec<Immediate>),
    Tuple(Vec<(Option<Ident>, Immediate)>),
}

impl Immediate {
    pub fn get_ty(&self, opt_span: Option<&Span>) -> Type {
        let span = opt_span.cloned().unwrap_or_else(empty_span);

        match self {
            Immediate::Error => Type::Error(span),

            Immediate::Array(elements) => {
                // Assume all elements have the same type.
                Type::Array {
                    ty: Box::new(
                        elements
                            .get(0)
                            .map(|el0| el0.get_ty(opt_span))
                            .unwrap_or_else(|| Type::Error(span.clone())),
                    ),
                    range: None,
                    size: Some(elements.len() as i64),
                    span,
                }
            }

            Immediate::Tuple(fields) => Type::Tuple {
                fields: fields
                    .iter()
                    .map(|(name, fld_imm)| (name.clone(), fld_imm.get_ty(opt_span)))
                    .collect(),
                span,
            },

            _ => Type::Primitive {
                kind: match self {
                    Immediate::Nil => PrimitiveKind::Nil,
                    Immediate::Real(_) => PrimitiveKind::Real,
                    Immediate::Int(_) => PrimitiveKind::Int,
                    Immediate::Bool(_) => PrimitiveKind::Bool,
                    Immediate::String(_) => PrimitiveKind::String,
                    Immediate::B256(_) => PrimitiveKind::B256,

                    Immediate::Error | Immediate::Array { .. } | Immediate::Tuple(_) => {
                        unreachable!()
                    }
                },
                span,
            },
        }
    }
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
            | Expr::Array { span, .. }
            | Expr::Tuple { span, .. }
            | Expr::PathByKey(_, span)
            | Expr::PathByName(_, span)
            | Expr::StorageAccess(_, span)
            | Expr::ExternalStorageAccess { span, .. }
            | Expr::UnaryOp { span, .. }
            | Expr::BinaryOp { span, .. }
            | Expr::MacroCall { span, .. }
            | Expr::IntrinsicCall { span, .. }
            | Expr::Select { span, .. }
            | Expr::Index { span, .. }
            | Expr::TupleFieldAccess { span, .. }
            | Expr::Cast { span, .. }
            | Expr::In { span, .. }
            | Expr::Generator { span, .. }
            | Expr::Range { span, .. } => span,
        }
    }
}

impl Expr {
    pub fn is_nil(&self) -> bool {
        matches!(
            self,
            Expr::Immediate {
                value: Immediate::Nil,
                ..
            }
        )
    }

    pub fn replace_ref<F: FnMut(&mut ExprKey)>(&mut self, mut replace: F) {
        match self {
            Expr::Immediate { .. } => {}
            Expr::Array { elements, .. } => elements.iter_mut().for_each(replace),
            Expr::Tuple { fields, .. } => fields.iter_mut().for_each(|(_, expr)| replace(expr)),
            Expr::UnaryOp { expr, .. } => replace(expr),
            Expr::BinaryOp { lhs, rhs, .. } => {
                replace(lhs);
                replace(rhs);
            }
            Expr::IntrinsicCall { args, .. } => args.iter_mut().for_each(replace),
            Expr::Select {
                condition,
                then_expr,
                else_expr,
                ..
            } => {
                replace(condition);
                replace(then_expr);
                replace(else_expr);
            }
            Expr::Index { expr, index, .. } => {
                replace(expr);
                replace(index);
            }
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
            | Expr::StorageAccess(_, _)
            | Expr::ExternalStorageAccess { .. }
            | Expr::PathByKey(_, _)
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

    pub fn replace_ref_by_map(&mut self, keys: &FxHashMap<ExprKey, ExprKey>) {
        self.replace_ref(|old_key: &mut ExprKey| {
            if let Some(new_key) = keys.get(old_key) {
                *old_key = *new_key;
            }
        });
    }
}
