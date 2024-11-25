use crate::{
    predicate::{CallKey, ExprKey, UnionKey},
    span::{empty_span, Span, Spanned},
    types::{PrimitiveKind, Type},
};
pub(crate) use intrinsics::{ExternalIntrinsic, InternalIntrinsic, IntrinsicKind};

use fxhash::FxHashMap;

mod display;
pub(crate) mod evaluate;
pub(crate) mod intrinsics;

#[derive(Clone, Debug)]
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
    UnionVariant {
        path: String,
        path_span: Span,
        value: Option<ExprKey>,
        span: Span,
    },
    Path(String, Span),
    LocalStorageAccess {
        name: String,
        mutable: bool,
        span: Span,
    },
    ExternalStorageAccess {
        interface: String,
        address: ExprKey,
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
        path: String,
        span: Span,
    },
    IntrinsicCall {
        kind: (IntrinsicKind, Span),
        args: Vec<ExprKey>,
        span: Span,
    },
    LocalPredicateCall {
        predicate: String,
        args: Vec<ExprKey>,
        span: Span,
    },
    ExternalPredicateCall {
        interface: String,
        c_addr: ExprKey,
        predicate: String,
        p_addr: ExprKey,
        args: Vec<ExprKey>,
        span: Span,
    },
    Select {
        condition: ExprKey,
        then_expr: ExprKey,
        else_expr: ExprKey,
        span: Span,
    },
    Match {
        match_expr: ExprKey,
        match_branches: Vec<MatchBranch>,
        else_branch: Option<MatchElse>,
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
        ty: Type,
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
    Map {
        param: Ident,
        range: ExprKey,
        body: ExprKey,
        span: Span,
    },
    UnionTag {
        union_expr: ExprKey,
        span: Span,
    },
    UnionValue {
        union_expr: ExprKey,
        variant_ty: Type,
        span: Span,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Ident {
    pub(super) name: String,
    pub(super) hygienic: bool,
    pub(super) span: Span,
}

impl Default for Ident {
    fn default() -> Self {
        Self {
            name: String::default(),
            hygienic: bool::default(),
            span: empty_span(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TupleAccess {
    Error,
    Index(usize),
    Name(Ident),
}

#[derive(Clone, Debug)]
pub struct MatchBranch {
    pub(super) name: String,
    pub(super) name_span: Span,
    pub(super) binding: Option<Ident>,
    pub(super) constraints: Vec<ExprKey>,
    pub(super) expr: ExprKey,
}

#[derive(Clone, Debug)]
pub struct MatchElse {
    pub(super) constraints: Vec<ExprKey>,
    pub(super) expr: ExprKey,
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
    UnionVariant {
        tag_num: i64,
        value_size: usize,
        value: Option<Box<Immediate>>,
        decl: UnionKey,
    },
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
                            .first()
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

            Immediate::UnionVariant { decl, .. } => Type::Union { decl: *decl, span },

            _ => Type::Primitive {
                kind: match self {
                    Immediate::Nil => PrimitiveKind::Nil,
                    Immediate::Real(_) => PrimitiveKind::Real,
                    Immediate::Int(_) => PrimitiveKind::Int,
                    Immediate::Bool(_) => PrimitiveKind::Bool,
                    Immediate::String(_) => PrimitiveKind::String,
                    Immediate::B256(_) => PrimitiveKind::B256,

                    Immediate::Error
                    | Immediate::Array { .. }
                    | Immediate::Tuple(_)
                    | Immediate::UnionVariant { .. } => {
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
            | Expr::UnionVariant { span, .. }
            | Expr::Path(_, span)
            | Expr::LocalStorageAccess { span, .. }
            | Expr::ExternalStorageAccess { span, .. }
            | Expr::UnaryOp { span, .. }
            | Expr::BinaryOp { span, .. }
            | Expr::MacroCall { span, .. }
            | Expr::IntrinsicCall { span, .. }
            | Expr::LocalPredicateCall { span, .. }
            | Expr::ExternalPredicateCall { span, .. }
            | Expr::Select { span, .. }
            | Expr::Match { span, .. }
            | Expr::Index { span, .. }
            | Expr::TupleFieldAccess { span, .. }
            | Expr::Cast { span, .. }
            | Expr::In { span, .. }
            | Expr::Generator { span, .. }
            | Expr::Range { span, .. }
            | Expr::Map { span, .. }
            | Expr::UnionTag { span, .. }
            | Expr::UnionValue { span, .. } => span,
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

    pub fn is_immediate(&self) -> bool {
        matches!(self, Expr::Immediate { .. })
    }

    pub fn replace_ref<F: FnMut(&mut ExprKey)>(&mut self, mut replace: F) {
        match self {
            Expr::Immediate { .. } => {}
            Expr::Array {
                elements,
                range_expr,
                ..
            } => {
                replace(range_expr);
                elements.iter_mut().for_each(replace);
            }
            Expr::Tuple { fields, .. } => fields.iter_mut().for_each(|(_, expr)| replace(expr)),
            Expr::UnionVariant { value, .. } => {
                if let Some(value) = value {
                    replace(value)
                }
            }
            Expr::UnaryOp { expr, .. } => replace(expr),
            Expr::ExternalStorageAccess { address, .. } => replace(address),
            Expr::BinaryOp { lhs, rhs, .. } => {
                replace(lhs);
                replace(rhs);
            }
            Expr::IntrinsicCall { args, .. } => args.iter_mut().for_each(replace),
            Expr::LocalPredicateCall { args, .. } => args.iter_mut().for_each(replace),
            Expr::ExternalPredicateCall {
                c_addr,
                p_addr,
                args,
                ..
            } => {
                replace(c_addr);
                replace(p_addr);
                args.iter_mut().for_each(replace)
            }
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
            Expr::Match {
                match_expr,
                match_branches,
                else_branch: else_expr,
                ..
            } => {
                replace(match_expr);
                match_branches.iter_mut().for_each(
                    |MatchBranch {
                         constraints, expr, ..
                     }| {
                        constraints.iter_mut().for_each(&mut replace);
                        replace(expr);
                    },
                );
                if let Some(MatchElse { constraints, expr }) = else_expr {
                    constraints.iter_mut().for_each(&mut replace);
                    replace(expr);
                }
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

            Expr::Map { range, body, .. } => {
                replace(range);
                replace(body);
            }

            Expr::UnionTag { union_expr, .. } | Expr::UnionValue { union_expr, .. } => {
                replace(union_expr)
            }

            Expr::MacroCall { .. }
            | Expr::Path(_, _)
            | Expr::LocalStorageAccess { .. }
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
