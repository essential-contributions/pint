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

    pub fn is_storage_access(&self) -> bool {
        matches!(
            self,
            Expr::LocalStorageAccess { .. }
                | Expr::ExternalStorageAccess { .. }
                | Expr::IntrinsicCall {
                    kind: (
                        IntrinsicKind::Internal(
                            InternalIntrinsic::StorageGet | InternalIntrinsic::StorageGetExtern
                        ),
                        _
                    ),
                    ..
                }
        )
    }

    #[allow(unused_variables)] // todo - ian - remove
                               // todo - ian - document?
                               // todo - ian - handle macro calls
    pub fn eq(&self, contract: &Contract, other: &Self) -> bool {
        match (self, other) {
            (
                Expr::Immediate {
                    value: lhs_value, ..
                },
                Expr::Immediate {
                    value: rhs_value, ..
                },
            ) => lhs_value == rhs_value,

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
            ) => {
                if lhs_elements.len() != rhs_elements.len() {
                    return false;
                }

                for (i, lhs_element) in lhs_elements.iter().enumerate() {
                    if !lhs_element
                        .get(contract)
                        .eq(contract, rhs_elements[i].get(contract))
                    {
                        return false;
                    }
                }

                return lhs_range_expr
                    .get(contract)
                    .eq(contract, rhs_range_expr.get(contract));
            }

            (
                Expr::Tuple {
                    fields: lhs_fields, ..
                },
                Expr::Tuple {
                    fields: rhs_fields, ..
                },
            ) => {
                if lhs_fields.len() != rhs_fields.len() {
                    return false;
                }

                for (i, (lhs_ident, lhs_field)) in lhs_fields.iter().enumerate() {
                    let (rhs_ident, rhs_field) = &rhs_fields[i];
                    if !lhs_field
                        .get(contract)
                        .eq(contract, rhs_field.get(contract))
                        || (lhs_ident != rhs_ident)
                    {
                        return false;
                    }
                }

                true
            }

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
            ) => {
                if lhs_path != rhs_path {
                    false
                } else {
                    match (lhs_value, rhs_value) {
                        (Some(lhs_value), Some(rhs_value)) => {
                            return lhs_value
                                .get(contract)
                                .eq(contract, rhs_value.get(contract))
                        }

                        (None, None) => true,

                        _ => false,
                    }
                }
            }

            (Expr::Path(lhs_path, ..), Expr::Path(rhs_path, ..)) => lhs_path == rhs_path,

            (
                Expr::LocalStorageAccess {
                    name: lhs_name,
                    mutable: lhs_mutable,
                    ..
                },
                Expr::LocalStorageAccess {
                    name: rhs_name,
                    mutable: rhs_mutable,
                    ..
                },
            ) => lhs_name == rhs_name && lhs_mutable == rhs_mutable,

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
            ) => {
                lhs_interface == rhs_interface
                    && lhs_address
                        .get(contract)
                        .eq(contract, rhs_address.get(contract))
                    && lhs_name == rhs_name
            }

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
            ) => lhs_op == rhs_op && lhs_expr.get(contract).eq(contract, rhs_expr.get(contract)),

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
            ) => match lhs_op {
                BinaryOp::Add
                | BinaryOp::Mul
                | BinaryOp::Equal
                | BinaryOp::NotEqual
                | BinaryOp::LogicalAnd
                | BinaryOp::LogicalOr => {
                    let is_op_eq = lhs_op == rhs_op;
                    let is_lhs_eq = lhs_lhs.get(contract).eq(contract, rhs_lhs.get(contract));
                    if is_lhs_eq {
                        return is_op_eq
                            && is_lhs_eq
                            && lhs_rhs.get(contract).eq(contract, rhs_rhs.get(contract));
                    } else {
                        return is_op_eq
                            && lhs_lhs.get(contract).eq(contract, rhs_rhs.get(contract))
                            && lhs_rhs.get(contract).eq(contract, rhs_lhs.get(contract));
                    }
                }

                BinaryOp::Sub
                | BinaryOp::Div
                | BinaryOp::Mod
                | BinaryOp::LessThanOrEqual
                | BinaryOp::LessThan
                | BinaryOp::GreaterThanOrEqual
                | BinaryOp::GreaterThan => {
                    return lhs_op == rhs_op
                        && lhs_lhs.get(contract).eq(contract, rhs_lhs.get(contract))
                        && lhs_rhs.get(contract).eq(contract, rhs_rhs.get(contract))
                }
            },

            (
                Expr::MacroCall {
                    call: lhs_call,
                    path: lhs_path,
                    ..
                },
                Expr::MacroCall {
                    call: rhs_call,
                    path: rhs_path,
                    ..
                },
            ) => {
                todo!() // @mohammad, need help here please
                        // if lhs_path != rhs_path {
                        //     return false;
                        // }
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
            ) => {
                if lhs_args.len() != rhs_args.len() {
                    return false;
                }

                for (i, lhs_arg) in lhs_args.iter().enumerate() {
                    if !lhs_arg
                        .get(contract)
                        .eq(contract, rhs_args[i].get(contract))
                    {
                        return false;
                    }
                }

                match (lhs_kind, rhs_kind) {
                    (
                        (IntrinsicKind::External(lhs_kind), _),
                        (IntrinsicKind::External(rhs_kind), _),
                    ) => lhs_kind == rhs_kind,

                    (
                        (IntrinsicKind::Internal(lhs_kind), _),
                        (IntrinsicKind::Internal(rhs_kind), _),
                    ) => lhs_kind == rhs_kind,

                    ((IntrinsicKind::Error, _), (IntrinsicKind::Error, _)) => true,

                    _ => false,
                }
            }

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
                if lhs_predicate != rhs_predicate {
                    return false;
                }

                if lhs_args.len() != rhs_args.len() {
                    return false;
                }

                for (i, lhs_arg) in lhs_args.iter().enumerate() {
                    if !lhs_arg
                        .get(contract)
                        .eq(contract, rhs_args[i].get(contract))
                    {
                        return false;
                    }
                }

                true
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
            ) => {
                if !(lhs_interface == rhs_interface
                    && lhs_c_addr
                        .get(contract)
                        .eq(contract, rhs_c_addr.get(contract))
                    && lhs_predicate == rhs_predicate
                    && lhs_p_addr
                        .get(contract)
                        .eq(contract, rhs_p_addr.get(contract)))
                {
                    return false;
                }

                if lhs_args.len() != rhs_args.len() {
                    return false;
                }

                for (i, lhs_arg) in lhs_args.iter().enumerate() {
                    if !lhs_arg
                        .get(contract)
                        .eq(contract, rhs_args[i].get(contract))
                    {
                        return false;
                    }
                }

                true
            }

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
            ) => {
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
            ) => {
                if !lhs_match_expr
                    .get(contract)
                    .eq(contract, rhs_match_expr.get(contract))
                {
                    return false;
                }

                if lhs_match_branches.len() != rhs_match_branches.len() {
                    return false;
                }

                for (i, lhs_match_branch) in lhs_match_branches.iter().enumerate() {
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
                    ) = (lhs_match_branch, rhs_match_branches[i].clone());

                    if *lhs_name != rhs_name {
                        return false;
                    }

                    if let (
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
                    ) = (lhs_binding, rhs_binding)
                    {
                        if *lhs_name != rhs_name || *lhs_hygienic != rhs_hygienic {
                            return false;
                        }
                    };

                    if lhs_constraints.len() != rhs_constraints.len() {
                        return false;
                    }

                    for (j, lhs_constraint) in lhs_constraints.iter().enumerate() {
                        if !lhs_constraint
                            .get(contract)
                            .eq(contract, rhs_constraints[j].get(contract))
                        {
                            return false;
                        }
                    }

                    if !lhs_expr.get(contract).eq(contract, rhs_expr.get(contract)) {
                        return false;
                    }
                }

                if let (
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
                    if lhs_constraints.len() != rhs_constraints.len() {
                        return false;
                    }

                    for (i, lhs_constraint) in lhs_constraints.iter().enumerate() {
                        if !lhs_constraint
                            .get(contract)
                            .eq(contract, rhs_constraints[i].get(contract))
                        {
                            return false;
                        }
                    }
                }

                true
            }

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
            ) => {
                lhs_expr.get(contract).eq(contract, rhs_expr.get(contract))
                    && lhs_index
                        .get(contract)
                        .eq(contract, rhs_index.get(contract))
            }

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
            ) => {
                if !lhs_tuple
                    .get(contract)
                    .eq(contract, rhs_tuple.get(contract))
                {
                    return false;
                }

                match (lhs_field, rhs_field) {
                    (TupleAccess::Error, TupleAccess::Error) => true,

                    (TupleAccess::Index(lhs_index), TupleAccess::Index(rhs_index)) => {
                        lhs_index == rhs_index
                    }

                    (TupleAccess::Name(lhs_name), TupleAccess::Name(rhs_name)) => {
                        lhs_name == rhs_name
                    }

                    _ => false,
                }
            }

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
            ) => {
                lhs_value
                    .get(contract)
                    .eq(contract, rhs_value.get(contract))
                    && lhs_ty.eq(contract, rhs_ty)
            }

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
            ) => {
                lhs_value
                    .get(contract)
                    .eq(contract, rhs_value.get(contract))
                    && lhs_collection
                        .get(contract)
                        .eq(contract, rhs_collection.get(contract))
            }

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
            ) => {
                lhs_lb.get(contract).eq(contract, rhs_lb.get(contract))
                    && lhs_ub.get(contract).eq(contract, rhs_ub.get(contract))
            }

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
            ) => {
                if lhs_kind != rhs_kind {
                    return false;
                }

                if rhs_gen_ranges.len() != lhs_gen_ranges.len() {
                    return false;
                }

                for (i, (lhs_ident, lhs_gen_range)) in lhs_gen_ranges.iter().enumerate() {
                    let (rhs_ident, rhs_gen_range) = &rhs_gen_ranges[i];

                    if lhs_ident != rhs_ident {
                        return false;
                    }

                    if !lhs_gen_range
                        .get(contract)
                        .eq(contract, rhs_gen_range.get(contract))
                    {
                        return false;
                    }
                }

                if lhs_conditions.len() != rhs_conditions.len() {
                    return false;
                }

                for (i, lhs_condition) in lhs_conditions.iter().enumerate() {
                    if !lhs_condition
                        .get(contract)
                        .eq(contract, rhs_conditions[i].get(contract))
                    {
                        return false;
                    }
                }

                lhs_body == rhs_body
            }

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
            ) => {
                lhs_param == rhs_param
                    && lhs_range
                        .get(contract)
                        .eq(contract, rhs_range.get(contract))
                    && lhs_body.get(contract).eq(contract, rhs_body.get(contract))
            }

            (
                Expr::UnionTag {
                    union_expr: lhs_union_expr,
                    ..
                },
                Expr::UnionTag {
                    union_expr: rhs_union_expr,
                    ..
                },
            ) => lhs_union_expr
                .get(contract)
                .eq(contract, rhs_union_expr.get(contract)),

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
            ) => {
                lhs_union_expr
                    .get(contract)
                    .eq(contract, rhs_union_expr.get(contract))
                    && lhs_variant_ty.eq(contract, rhs_variant_ty)
            }

            _ => false,
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
