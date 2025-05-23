use crate::{
    predicate::{CallKey, Contract, ExprKey, UnionKey},
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
    Nil(Span),
    KeyValue {
        lhs: ExprKey,
        rhs: ExprKey,
        span: Span,
    },
    Path(String, Span),
    AsmBlock {
        args: Vec<ExprKey>,
        ops: Vec<AsmOp>,
        span: Span,
    },
    LocalStorageAccess {
        name: String,
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

#[derive(Clone, Debug)]
pub enum AsmOp {
    Imm(i64, Span),
    Op(Ident),
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
                Type::FixedArray {
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
    Unwrap,
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

    Concat,
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
            BinaryOp::Concat => "++",
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
            | Expr::Nil(span)
            | Expr::KeyValue { span, .. }
            | Expr::Path(_, span)
            | Expr::AsmBlock { span, .. }
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
    pub fn is_immediate(&self) -> bool {
        matches!(self, Expr::Immediate { .. })
    }

    pub fn is_asm_block(&self) -> bool {
        matches!(self, Expr::AsmBlock { .. })
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Expr::Nil(_))
    }

    /// Returns true if `self` is a `__len(_)` intrinsic with the argument being a pre-state
    /// storage vector access
    pub fn is_pre_storage_vector_len(&self, contract: &Contract) -> bool {
        if let Expr::IntrinsicCall {
            kind: (IntrinsicKind::External(ExternalIntrinsic::ArrayLen), _),
            args,
            ..
        } = self
        {
            if let Some(arg) = args.first() {
                if arg.is_pre_storage_access(contract) {
                    if let Some(ty) = arg.get_ty(contract).get_optional_ty() {
                        return ty.is_unsized_array();
                    }
                }
            }
        }

        false
    }

    /// Returns true if `self` is a `__len(_)` intrinsic with the argument being a post-state
    /// storage vector access
    pub fn is_post_storage_vector_len(&self, contract: &Contract) -> bool {
        if let Expr::IntrinsicCall {
            kind: (IntrinsicKind::External(ExternalIntrinsic::ArrayLen), _),
            args,
            ..
        } = self
        {
            if let Some(arg) = args.first() {
                if arg.is_post_storage_access(contract) {
                    if let Some(ty) = arg.get_ty(contract).get_optional_ty() {
                        return ty.is_unsized_array();
                    }
                }
            }
        }

        false
    }

    pub fn is_storage_vec_len(&self, contract: &Contract) -> bool {
        self.is_pre_storage_vector_len(contract) || self.is_post_storage_vector_len(contract)
    }

    pub fn is_pre_storage_access_intrinsic(&self) -> bool {
        matches!(
            self,
            Expr::IntrinsicCall {
                kind: (
                    IntrinsicKind::Internal(
                        InternalIntrinsic::PreState | InternalIntrinsic::PreStateExtern
                    ),
                    _
                ),
                ..
            }
        )
    }

    pub fn is_post_storage_access_intrinsic(&self) -> bool {
        matches!(
            self,
            Expr::IntrinsicCall {
                kind: (
                    IntrinsicKind::Internal(
                        InternalIntrinsic::PostState | InternalIntrinsic::PostStateExtern
                    ),
                    _
                ),
                ..
            }
        )
    }

    pub fn is_storage_access_intrinsic(&self) -> bool {
        self.is_pre_storage_access_intrinsic() || self.is_post_storage_access_intrinsic()
    }

    pub fn is_pre_storage_access(&self, contract: &Contract) -> bool {
        self.is_pre_storage_access_intrinsic()
            || self.is_pre_storage_vector_len(contract)
            || match self {
                Expr::LocalStorageAccess { .. } | Expr::ExternalStorageAccess { .. } => true,
                Expr::TupleFieldAccess { tuple, .. } => tuple.is_storage_access(contract),
                Expr::Index { expr, .. } => expr.is_storage_access(contract),
                _ => false,
            }
    }

    pub fn is_post_storage_access(&self, contract: &Contract) -> bool {
        self.is_post_storage_access_intrinsic()
            || self.is_post_storage_vector_len(contract)
            || match self {
                Expr::UnaryOp {
                    op: UnaryOp::NextState,
                    expr,
                    ..
                } => expr.is_storage_access(contract),
                _ => false,
            }
    }

    pub fn is_storage_access(&self, contract: &Contract) -> bool {
        self.is_pre_storage_access(contract) || self.is_post_storage_access(contract)
    }

    pub fn eq(&self, contract: &Contract, other: &Self) -> bool {
        match (self, other) {
            (
                Expr::Immediate {
                    value: lhs_value, ..
                },
                Expr::Immediate {
                    value: rhs_value, ..
                },
            ) => immediate_eq(lhs_value, rhs_value),

            (
                Expr::Array {
                    elements: lhs_elements,
                    range_expr: lhs_range_expr,
                    ..
                },
                Expr::Array {
                    elements: rhs_elements,
                    range_expr: rhs_range_expr,
                    ..
                },
            ) => array_eq(
                contract,
                lhs_elements,
                *lhs_range_expr,
                rhs_elements,
                *rhs_range_expr,
            ),

            (
                Expr::Tuple {
                    fields: lhs_fields, ..
                },
                Expr::Tuple {
                    fields: rhs_fields, ..
                },
            ) => tuple_eq(contract, lhs_fields, rhs_fields),

            (
                Expr::UnionVariant {
                    path: lhs_path,
                    value: lhs_value,
                    ..
                },
                Expr::UnionVariant {
                    path: rhs_path,
                    value: rhs_value,
                    ..
                },
            ) => union_variant_eq(contract, lhs_path, lhs_value, rhs_path, rhs_value),

            (Expr::Nil(_), Expr::Nil(_)) => false,

            (
                Expr::KeyValue {
                    lhs: lhs_lhs,
                    rhs: lhs_rhs,
                    ..
                },
                Expr::KeyValue {
                    lhs: rhs_lhs,
                    rhs: rhs_rhs,
                    ..
                },
            ) => key_value_eq(contract, *lhs_lhs, *lhs_rhs, *rhs_lhs, *rhs_rhs),

            (Expr::Path(lhs_path, ..), Expr::Path(rhs_path, ..)) => path_eq(lhs_path, rhs_path),
            (
                Expr::AsmBlock {
                    args: lhs_args,
                    ops: lhs_ops,
                    ..
                },
                Expr::AsmBlock {
                    args: rhs_args,
                    ops: rhs_ops,
                    ..
                },
            ) => asm_block_eq(contract, lhs_args, lhs_ops, rhs_args, rhs_ops),

            (
                Expr::LocalStorageAccess { name: lhs_name, .. },
                Expr::LocalStorageAccess { name: rhs_name, .. },
            ) => lhs_name == rhs_name,

            (
                Expr::ExternalStorageAccess {
                    interface: lhs_interface,
                    address: lhs_address,
                    name: lhs_name,
                    ..
                },
                Expr::ExternalStorageAccess {
                    interface: rhs_interface,
                    address: rhs_address,
                    name: rhs_name,
                    ..
                },
            ) => external_storage_access_eq(
                contract,
                lhs_interface,
                *lhs_address,
                lhs_name,
                rhs_interface,
                *rhs_address,
                rhs_name,
            ),

            (
                Expr::UnaryOp {
                    op: lhs_op,
                    expr: lhs_expr,
                    ..
                },
                Expr::UnaryOp {
                    op: rhs_op,
                    expr: rhs_expr,
                    ..
                },
            ) => unary_op_eq(contract, lhs_op, *lhs_expr, rhs_op, *rhs_expr),

            (
                Expr::BinaryOp {
                    op: lhs_op,
                    lhs: lhs_lhs,
                    rhs: lhs_rhs,
                    ..
                },
                Expr::BinaryOp {
                    op: rhs_op,
                    lhs: rhs_lhs,
                    rhs: rhs_rhs,
                    ..
                },
            ) => binary_op_eq(
                contract, lhs_op, *lhs_lhs, *lhs_rhs, rhs_op, *rhs_lhs, *rhs_rhs,
            ),

            (Expr::MacroCall { path: lhs_path, .. }, Expr::MacroCall { path: rhs_path, .. }) => {
                macro_call_eq(lhs_path, rhs_path)
            }

            (
                Expr::IntrinsicCall {
                    kind: lhs_kind,
                    args: lhs_args,
                    ..
                },
                Expr::IntrinsicCall {
                    kind: rhs_kind,
                    args: rhs_args,
                    ..
                },
            ) => intrinsic_call_eq(contract, lhs_kind, lhs_args, rhs_kind, rhs_args),

            (
                Expr::LocalPredicateCall {
                    predicate: lhs_predicate,
                    args: lhs_args,
                    ..
                },
                Expr::LocalPredicateCall {
                    predicate: rhs_predicate,
                    args: rhs_args,
                    ..
                },
            ) => {
                local_predicate_call_eq(contract, lhs_predicate, lhs_args, rhs_predicate, rhs_args)
            }
            (
                Expr::ExternalPredicateCall {
                    interface: lhs_interface,
                    c_addr: lhs_c_addr,
                    predicate: lhs_predicate,
                    p_addr: lhs_p_addr,
                    args: lhs_args,
                    ..
                },
                Expr::ExternalPredicateCall {
                    interface: rhs_interface,
                    c_addr: rhs_c_addr,
                    predicate: rhs_predicate,
                    p_addr: rhs_p_addr,
                    args: rhs_args,
                    ..
                },
            ) => external_predicate_call_eq(
                contract,
                lhs_interface,
                *lhs_c_addr,
                lhs_predicate,
                *lhs_p_addr,
                lhs_args,
                rhs_interface,
                *rhs_c_addr,
                rhs_predicate,
                *rhs_p_addr,
                rhs_args,
            ),

            (
                Expr::Select {
                    condition: lhs_condition,
                    then_expr: lhs_then_expr,
                    else_expr: lhs_else_expr,
                    ..
                },
                Expr::Select {
                    condition: rhs_condition,
                    then_expr: rhs_then_expr,
                    else_expr: rhs_else_expr,
                    ..
                },
            ) => select_eq(
                contract,
                *lhs_condition,
                *lhs_then_expr,
                *lhs_else_expr,
                *rhs_condition,
                *rhs_then_expr,
                *rhs_else_expr,
            ),

            (
                Expr::Match {
                    match_expr: lhs_match_expr,
                    match_branches: lhs_match_branches,
                    else_branch: lhs_else_branch,
                    ..
                },
                Expr::Match {
                    match_expr: rhs_match_expr,
                    match_branches: rhs_match_branches,
                    else_branch: rhs_else_branch,
                    ..
                },
            ) => match_eq(
                contract,
                *lhs_match_expr,
                lhs_match_branches,
                lhs_else_branch,
                *rhs_match_expr,
                rhs_match_branches,
                rhs_else_branch,
            ),

            (
                Expr::Index {
                    expr: lhs_expr,
                    index: lhs_index,
                    ..
                },
                Expr::Index {
                    expr: rhs_expr,
                    index: rhs_index,
                    ..
                },
            ) => index_eq(contract, *lhs_expr, *lhs_index, *rhs_expr, *rhs_index),

            (
                Expr::TupleFieldAccess {
                    tuple: lhs_tuple,
                    field: lhs_field,
                    ..
                },
                Expr::TupleFieldAccess {
                    tuple: rhs_tuple,
                    field: rhs_field,
                    ..
                },
            ) => tuple_field_access_eq(contract, *lhs_tuple, lhs_field, *rhs_tuple, rhs_field),

            (
                Expr::Cast {
                    value: lhs_value,
                    ty: lhs_ty,
                    ..
                },
                Expr::Cast {
                    value: rhs_value,
                    ty: rhs_ty,
                    ..
                },
            ) => cast_eq(contract, *lhs_value, lhs_ty, *rhs_value, rhs_ty),

            (
                Expr::In {
                    value: lhs_value,
                    collection: lhs_collection,
                    ..
                },
                Expr::In {
                    value: rhs_value,
                    collection: rhs_collection,
                    ..
                },
            ) => in_eq(
                contract,
                *lhs_value,
                *lhs_collection,
                *rhs_value,
                *rhs_collection,
            ),

            (
                Expr::Range {
                    lb: lhs_lb,
                    ub: lhs_ub,
                    ..
                },
                Expr::Range {
                    lb: rhs_lb,
                    ub: rhs_ub,
                    ..
                },
            ) => range_eq(contract, *lhs_lb, *lhs_ub, *rhs_lb, *rhs_ub),

            (
                Expr::Generator {
                    kind: lhs_kind,
                    gen_ranges: lhs_gen_ranges,
                    conditions: lhs_conditions,
                    body: lhs_body,
                    ..
                },
                Expr::Generator {
                    kind: rhs_kind,
                    gen_ranges: rhs_gen_ranges,
                    conditions: rhs_conditions,
                    body: rhs_body,
                    ..
                },
            ) => generator_eq(
                contract,
                lhs_kind,
                lhs_gen_ranges,
                lhs_conditions,
                *lhs_body,
                rhs_kind,
                rhs_gen_ranges,
                rhs_conditions,
                *rhs_body,
            ),

            (
                Expr::Map {
                    param: lhs_param,
                    range: lhs_range,
                    body: lhs_body,
                    ..
                },
                Expr::Map {
                    param: rhs_param,
                    range: rhs_range,
                    body: rhs_body,
                    ..
                },
            ) => map_eq(
                contract, lhs_param, *lhs_range, *lhs_body, rhs_param, *rhs_range, *rhs_body,
            ),

            (
                Expr::UnionTag {
                    union_expr: lhs_union_expr,
                    ..
                },
                Expr::UnionTag {
                    union_expr: rhs_union_expr,
                    ..
                },
            ) => union_tag_eq(contract, *lhs_union_expr, *rhs_union_expr),

            (
                Expr::UnionValue {
                    union_expr: lhs_union_expr,
                    variant_ty: lhs_variant_ty,
                    ..
                },
                Expr::UnionValue {
                    union_expr: rhs_union_expr,
                    variant_ty: rhs_variant_ty,
                    ..
                },
            ) => union_value_eq(
                contract,
                *lhs_union_expr,
                lhs_variant_ty,
                *rhs_union_expr,
                rhs_variant_ty,
            ),

            (Expr::Error(..), _)
            | (Expr::Immediate { .. }, _)
            | (Expr::Array { .. }, _)
            | (Expr::Tuple { .. }, _)
            | (Expr::UnionVariant { .. }, _)
            | (Expr::Nil(_), _)
            | (Expr::KeyValue { .. }, _)
            | (Expr::Path(..), _)
            | (Expr::AsmBlock { .. }, ..)
            | (Expr::LocalStorageAccess { .. }, _)
            | (Expr::ExternalStorageAccess { .. }, _)
            | (Expr::UnaryOp { .. }, _)
            | (Expr::BinaryOp { .. }, _)
            | (Expr::MacroCall { .. }, _)
            | (Expr::IntrinsicCall { .. }, _)
            | (Expr::LocalPredicateCall { .. }, _)
            | (Expr::ExternalPredicateCall { .. }, _)
            | (Expr::Select { .. }, _)
            | (Expr::Match { .. }, _)
            | (Expr::Index { .. }, _)
            | (Expr::TupleFieldAccess { .. }, _)
            | (Expr::Cast { .. }, _)
            | (Expr::In { .. }, _)
            | (Expr::Range { .. }, _)
            | (Expr::Generator { .. }, _)
            | (Expr::Map { .. }, _)
            | (Expr::UnionTag { .. }, _)
            | (Expr::UnionValue { .. }, _) => false,
        }
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
            Expr::KeyValue { lhs, rhs, .. } => {
                replace(lhs);
                replace(rhs);
            }
            Expr::UnaryOp { expr, .. } => replace(expr),
            Expr::AsmBlock { args, .. } => args.iter_mut().for_each(replace),
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
            | Expr::Nil(_)
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

pub fn immediate_eq(lhs_value: &Immediate, rhs_value: &Immediate) -> bool {
    lhs_value == rhs_value
}

pub fn array_eq(
    contract: &Contract,
    lhs_elements: &[ExprKey],
    lhs_range_expr: ExprKey,
    rhs_elements: &[ExprKey],
    rhs_range_expr: ExprKey,
) -> bool {
    lhs_elements.len() == rhs_elements.len()
        && lhs_elements
            .iter()
            .zip(rhs_elements.iter())
            .all(|(lhs_element, rhs_element)| {
                lhs_element
                    .get(contract)
                    .eq(contract, rhs_element.get(contract))
            })
        && lhs_range_expr
            .get(contract)
            .eq(contract, rhs_range_expr.get(contract))
}

pub fn tuple_eq(
    contract: &Contract,
    lhs_fields: &[(Option<Ident>, ExprKey)],
    rhs_fields: &[(Option<Ident>, ExprKey)],
) -> bool {
    lhs_fields.len() == rhs_fields.len()
        && lhs_fields.iter().zip(rhs_fields.iter()).all(
            |((lhs_ident, lhs_field), (rhs_ident, rhs_field))| {
                lhs_field
                    .get(contract)
                    .eq(contract, rhs_field.get(contract))
                    && lhs_ident == rhs_ident
            },
        )
}

pub fn union_variant_eq(
    contract: &Contract,
    lhs_path: &String,
    lhs_value: &Option<ExprKey>,
    rhs_path: &String,
    rhs_value: &Option<ExprKey>,
) -> bool {
    lhs_path == rhs_path
        && match (lhs_value, rhs_value) {
            (Some(lhs_value), Some(rhs_value)) => lhs_value
                .get(contract)
                .eq(contract, rhs_value.get(contract)),

            (None, None) => true,

            _ => false,
        }
}

pub fn path_eq(lhs_path: &String, rhs_path: &String) -> bool {
    lhs_path == rhs_path
}

pub fn key_value_eq(
    contract: &Contract,
    lhs_lhs: ExprKey,
    lhs_rhs: ExprKey,
    rhs_lhs: ExprKey,
    rhs_rhs: ExprKey,
) -> bool {
    lhs_lhs.get(contract).eq(contract, rhs_lhs.get(contract))
        && lhs_rhs.get(contract).eq(contract, rhs_rhs.get(contract))
        || lhs_lhs.get(contract).eq(contract, rhs_rhs.get(contract))
            && lhs_rhs.get(contract).eq(contract, rhs_lhs.get(contract))
}

pub fn asm_block_eq(
    contract: &Contract,
    lhs_args: &[ExprKey],
    lhs_ops: &[AsmOp],
    rhs_args: &[ExprKey],
    rhs_ops: &[AsmOp],
) -> bool {
    lhs_args
        .iter()
        .zip(rhs_args.iter())
        .all(|(lhs_arg, rhs_arg)| lhs_arg.get(contract).eq(contract, rhs_arg.get(contract)))
        && lhs_ops
            .iter()
            .zip(rhs_ops.iter())
            .all(|(lhs_op, rhs_op)| match (lhs_op, rhs_op) {
                (AsmOp::Imm(lhs_imm, _), AsmOp::Imm(rhs_imm, _)) => lhs_imm == rhs_imm,
                (AsmOp::Op(lhs_op), AsmOp::Op(rhs_op)) => lhs_op.name == rhs_op.name,
                _ => false,
            })
}

pub fn external_storage_access_eq(
    contract: &Contract,
    lhs_interface: &String,
    lhs_address: ExprKey,
    lhs_name: &String,
    rhs_interface: &String,
    rhs_address: ExprKey,
    rhs_name: &String,
) -> bool {
    lhs_interface == rhs_interface
        && lhs_address
            .get(contract)
            .eq(contract, rhs_address.get(contract))
        && lhs_name == rhs_name
}

pub fn unary_op_eq(
    contract: &Contract,
    lhs_op: &UnaryOp,
    lhs_expr: ExprKey,
    rhs_op: &UnaryOp,
    rhs_expr: ExprKey,
) -> bool {
    lhs_op == rhs_op && lhs_expr.get(contract).eq(contract, rhs_expr.get(contract))
}

pub fn binary_op_eq(
    contract: &Contract,
    lhs_op: &BinaryOp,
    lhs_lhs: ExprKey,
    lhs_rhs: ExprKey,
    rhs_op: &BinaryOp,
    rhs_lhs: ExprKey,
    rhs_rhs: ExprKey,
) -> bool {
    match lhs_op {
        // These ops are commutative
        BinaryOp::Add | BinaryOp::Mul | BinaryOp::Equal | BinaryOp::NotEqual | BinaryOp::Concat => {
            lhs_op == rhs_op
                && (lhs_lhs.get(contract).eq(contract, rhs_lhs.get(contract))
                    && lhs_rhs.get(contract).eq(contract, rhs_rhs.get(contract))
                    || lhs_lhs.get(contract).eq(contract, rhs_rhs.get(contract))
                        && lhs_rhs.get(contract).eq(contract, rhs_lhs.get(contract)))
        }

        // These ops are not commutative
        BinaryOp::Sub
        | BinaryOp::Div
        | BinaryOp::Mod
        | BinaryOp::LessThanOrEqual
        | BinaryOp::LessThan
        | BinaryOp::GreaterThanOrEqual
        | BinaryOp::GreaterThan
        | BinaryOp::LogicalAnd
        | BinaryOp::LogicalOr => {
            lhs_op == rhs_op
                && lhs_lhs.get(contract).eq(contract, rhs_lhs.get(contract))
                && lhs_rhs.get(contract).eq(contract, rhs_rhs.get(contract))
        }
    }
}

pub fn macro_call_eq(lhs_path: &String, rhs_path: &String) -> bool {
    lhs_path == rhs_path
}

pub fn intrinsic_call_eq(
    contract: &Contract,
    lhs_kind: &(IntrinsicKind, Span),
    lhs_args: &[ExprKey],
    rhs_kind: &(IntrinsicKind, Span),
    rhs_args: &[ExprKey],
) -> bool {
    lhs_args.len() == rhs_args.len()
        && lhs_args
            .iter()
            .zip(rhs_args.iter())
            .all(|(lhs_arg, rhs_arg)| lhs_arg.get(contract).eq(contract, rhs_arg.get(contract)))
        && match (lhs_kind, rhs_kind) {
            ((IntrinsicKind::External(lhs_kind), _), (IntrinsicKind::External(rhs_kind), _)) => {
                lhs_kind == rhs_kind
            }

            ((IntrinsicKind::Internal(lhs_kind), _), (IntrinsicKind::Internal(rhs_kind), _)) => {
                lhs_kind == rhs_kind
            }

            ((IntrinsicKind::Error, _), (IntrinsicKind::Error, _)) => true,

            _ => false,
        }
}

pub fn local_predicate_call_eq(
    contract: &Contract,
    lhs_predicate: &String,
    lhs_args: &[ExprKey],
    rhs_predicate: &String,
    rhs_args: &[ExprKey],
) -> bool {
    lhs_predicate == rhs_predicate
        && lhs_args.len() == rhs_args.len()
        && lhs_args
            .iter()
            .zip(rhs_args.iter())
            .all(|(lhs_arg, rhs_arg)| lhs_arg.get(contract).eq(contract, rhs_arg.get(contract)))
}

#[allow(clippy::too_many_arguments)]
pub fn external_predicate_call_eq(
    contract: &Contract,
    lhs_interface: &String,
    lhs_c_addr: ExprKey,
    lhs_predicate: &String,
    lhs_p_addr: ExprKey,
    lhs_args: &[ExprKey],
    rhs_interface: &String,
    rhs_c_addr: ExprKey,
    rhs_predicate: &String,
    rhs_p_addr: ExprKey,
    rhs_args: &[ExprKey],
) -> bool {
    lhs_interface == rhs_interface
        && lhs_c_addr
            .get(contract)
            .eq(contract, rhs_c_addr.get(contract))
        && lhs_predicate == rhs_predicate
        && lhs_p_addr
            .get(contract)
            .eq(contract, rhs_p_addr.get(contract))
        && lhs_args.len() == rhs_args.len()
        && lhs_args
            .iter()
            .zip(rhs_args.iter())
            .all(|(lhs_arg, rhs_arg)| lhs_arg.get(contract).eq(contract, rhs_arg.get(contract)))
}

pub fn select_eq(
    contract: &Contract,
    lhs_condition: ExprKey,
    lhs_then_expr: ExprKey,
    lhs_else_expr: ExprKey,
    rhs_condition: ExprKey,
    rhs_then_expr: ExprKey,
    rhs_else_expr: ExprKey,
) -> bool {
    lhs_condition
        .get(contract)
        .eq(contract, rhs_condition.get(contract))
        && lhs_then_expr
            .get(contract)
            .eq(contract, rhs_then_expr.get(contract))
        && lhs_else_expr
            .get(contract)
            .eq(contract, rhs_else_expr.get(contract))
}

pub fn match_eq(
    contract: &Contract,
    lhs_match_expr: ExprKey,
    lhs_match_branches: &[MatchBranch],
    lhs_else_branch: &Option<MatchElse>,
    rhs_match_expr: ExprKey,
    rhs_match_branches: &[MatchBranch],
    rhs_else_branch: &Option<MatchElse>,
) -> bool {
    lhs_match_expr
        .get(contract)
        .eq(contract, rhs_match_expr.get(contract))
        && lhs_match_branches.len() != rhs_match_branches.len()
        && lhs_match_branches
            .iter()
            .zip(rhs_match_branches.iter())
            .all(|(lhs_match_branch, rhs_match_branch)| {
                let (
                    MatchBranch {
                        name: lhs_name,
                        binding: lhs_binding,
                        constraints: lhs_constraints,
                        expr: lhs_expr,
                        ..
                    },
                    MatchBranch {
                        name: rhs_name,
                        binding: rhs_binding,
                        constraints: rhs_constraints,
                        expr: rhs_expr,
                        ..
                    },
                ) = (lhs_match_branch, rhs_match_branch.clone());

                *lhs_name == rhs_name
                    && match (lhs_binding, rhs_binding) {
                        (
                            Some(Ident {
                                name: lhs_name,
                                hygienic: lhs_hygienic,
                                ..
                            }),
                            Some(Ident {
                                name: rhs_name,
                                hygienic: rhs_hygienic,
                                ..
                            }),
                        ) => *lhs_name == rhs_name && *lhs_hygienic == rhs_hygienic,

                        (None, None) => true,

                        _ => false,
                    }
                    && lhs_constraints.len() == rhs_constraints.len()
                    && lhs_constraints.iter().zip(rhs_constraints.iter()).all(
                        |(lhs_constraint, rhs_constraint)| {
                            lhs_constraint
                                .get(contract)
                                .eq(contract, rhs_constraint.get(contract))
                        },
                    )
                    && lhs_expr.get(contract).eq(contract, rhs_expr.get(contract))
            })
        && if let (
            Some(MatchElse {
                constraints: lhs_constraints,
                expr: lhs_expr,
            }),
            Some(MatchElse {
                constraints: rhs_constraints,
                expr: rhs_expr,
            }),
        ) = (lhs_else_branch, rhs_else_branch)
        {
            lhs_constraints.len() == rhs_constraints.len()
                && lhs_constraints.iter().zip(rhs_constraints.iter()).all(
                    |(lhs_constraint, rhs_constraint)| {
                        lhs_constraint
                            .get(contract)
                            .eq(contract, rhs_constraint.get(contract))
                    },
                )
                && lhs_expr.eq(rhs_expr)
        } else {
            lhs_else_branch.is_none() && rhs_else_branch.is_none()
        }
}

pub fn index_eq(
    contract: &Contract,
    lhs_expr: ExprKey,
    lhs_index: ExprKey,
    rhs_expr: ExprKey,
    rhs_index: ExprKey,
) -> bool {
    lhs_expr.get(contract).eq(contract, rhs_expr.get(contract))
        && lhs_index
            .get(contract)
            .eq(contract, rhs_index.get(contract))
}

pub fn tuple_field_access_eq(
    contract: &Contract,
    lhs_tuple: ExprKey,
    lhs_field: &TupleAccess,
    rhs_tuple: ExprKey,
    rhs_field: &TupleAccess,
) -> bool {
    lhs_tuple
        .get(contract)
        .eq(contract, rhs_tuple.get(contract))
        && match (lhs_field, rhs_field) {
            (TupleAccess::Error, TupleAccess::Error) => true,

            (TupleAccess::Index(lhs_index), TupleAccess::Index(rhs_index)) => {
                lhs_index == rhs_index
            }

            (TupleAccess::Name(lhs_name), TupleAccess::Name(rhs_name)) => lhs_name == rhs_name,

            _ => false,
        }
}

pub fn cast_eq(
    contract: &Contract,
    lhs_value: ExprKey,
    lhs_ty: &Type,
    rhs_value: ExprKey,
    rhs_ty: &Type,
) -> bool {
    lhs_value
        .get(contract)
        .eq(contract, rhs_value.get(contract))
        && lhs_ty.eq(contract, rhs_ty)
}

pub fn in_eq(
    contract: &Contract,
    lhs_value: ExprKey,
    lhs_collection: ExprKey,
    rhs_value: ExprKey,
    rhs_collection: ExprKey,
) -> bool {
    lhs_value
        .get(contract)
        .eq(contract, rhs_value.get(contract))
        && lhs_collection
            .get(contract)
            .eq(contract, rhs_collection.get(contract))
}

pub fn range_eq(
    contract: &Contract,
    lhs_lb: ExprKey,
    lhs_ub: ExprKey,
    rhs_lb: ExprKey,
    rhs_ub: ExprKey,
) -> bool {
    lhs_lb.get(contract).eq(contract, rhs_lb.get(contract))
        && lhs_ub.get(contract).eq(contract, rhs_ub.get(contract))
}

#[allow(clippy::too_many_arguments)]
pub fn generator_eq(
    contract: &Contract,
    lhs_kind: &GeneratorKind,
    lhs_gen_ranges: &[(Ident, ExprKey)],
    lhs_conditions: &[ExprKey],
    lhs_body: ExprKey,
    rhs_kind: &GeneratorKind,
    rhs_gen_ranges: &[(Ident, ExprKey)],
    rhs_conditions: &[ExprKey],
    rhs_body: ExprKey,
) -> bool {
    lhs_kind == rhs_kind
        && rhs_gen_ranges.len() == lhs_gen_ranges.len()
        && lhs_gen_ranges.iter().zip(rhs_gen_ranges.iter()).all(
            |((lhs_ident, lhs_gen_range), (rhs_ident, rhs_gen_range))| {
                lhs_ident == rhs_ident
                    && lhs_gen_range
                        .get(contract)
                        .eq(contract, rhs_gen_range.get(contract))
            },
        )
        && lhs_conditions.len() != rhs_conditions.len()
        && lhs_conditions
            .iter()
            .zip(rhs_conditions.iter())
            .all(|(lhs_condition, rhs_condition)| {
                lhs_condition
                    .get(contract)
                    .eq(contract, rhs_condition.get(contract))
            })
        && lhs_body == rhs_body
}

pub fn map_eq(
    contract: &Contract,
    lhs_param: &Ident,
    lhs_range: ExprKey,
    lhs_body: ExprKey,
    rhs_param: &Ident,
    rhs_range: ExprKey,
    rhs_body: ExprKey,
) -> bool {
    lhs_param == rhs_param
        && lhs_range
            .get(contract)
            .eq(contract, rhs_range.get(contract))
        && lhs_body.get(contract).eq(contract, rhs_body.get(contract))
}

pub fn union_tag_eq(contract: &Contract, lhs_union_expr: ExprKey, rhs_union_expr: ExprKey) -> bool {
    lhs_union_expr
        .get(contract)
        .eq(contract, rhs_union_expr.get(contract))
}

pub fn union_value_eq(
    contract: &Contract,
    lhs_union_expr: ExprKey,
    lhs_variant_ty: &Type,
    rhs_union_expr: ExprKey,
    rhs_variant_ty: &Type,
) -> bool {
    lhs_union_expr
        .get(contract)
        .eq(contract, rhs_union_expr.get(contract))
        && lhs_variant_ty.eq(contract, rhs_variant_ty)
}
