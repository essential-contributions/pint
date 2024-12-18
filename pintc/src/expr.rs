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

    #[allow(unused_variables)] // todo - ian - remove
    pub fn eq(&self, contract: &Contract, other: &Self) -> bool {
        // println!("inside eq, {:#?} against {:#?}", self, other);
        match (self, other) {
            (Expr::Error(_), Expr::Error(_)) => todo!(), // do we handle errors? I don't think we should...

            (
                Expr::Immediate {
                    value: lhs_value, ..
                },
                Expr::Immediate {
                    value: rhs_value, ..
                },
            ) => {
                // println!("checking immediates");
                // println!("lhs_value == rhs_value: {}", lhs_value == rhs_value);
                lhs_value == rhs_value
            }

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
            ) => todo!(),

            (
                Expr::Tuple {
                    fields: lhs_fields, ..
                },
                Expr::Tuple {
                    fields: rhs_fields, ..
                },
            ) => todo!(),

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
                todo!()
            }

            (Expr::Path(lhs_path, ..), Expr::Path(rhs_path, ..)) => {
                // println!("checking paths");
                // println!("lhs_path == rhs_path: {}", lhs_path == rhs_path);
                lhs_path == rhs_path
            }

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
            ) => todo!(),

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
            ) => todo!(),

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
            ) => todo!(),

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
                    // println!("checking binary ops");
                    // println!("lhs_op == rhs_op: {}", lhs_op == rhs_op);
                    // println!(
                    //     "lhs_lhs.eq(rhs_lhs): {}",
                    //     lhs_lhs.get(contract).eq(contract, rhs_lhs.get(contract))
                    // );
                    // println!(
                    //     "lhs_rhs.eq(rhs_rhs): {}",
                    //     lhs_rhs.get(contract).eq(contract, rhs_rhs.get(contract))
                    // );
                    let is_op_same = lhs_op == rhs_op;
                    let is_lhs_same = lhs_lhs.get(contract).eq(contract, rhs_lhs.get(contract));
                    if is_lhs_same {
                        return is_op_same
                            && is_lhs_same
                            && lhs_rhs.get(contract).eq(contract, rhs_rhs.get(contract));
                    } else {
                        return is_op_same
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
            ) => todo!(),

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
                todo!()
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
            ) => todo!(),

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
            ) => todo!(),

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
            ) => todo!(),

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
            ) => todo!(),

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
            ) => todo!(),

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
            ) => todo!(),

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
            ) => todo!(),

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
            ) => todo!(),

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
            ) => todo!(),

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
            ) => todo!(),

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
            ) => todo!(),

            (
                Expr::UnionTag {
                    union_expr: lhs_union_expr,
                    ..
                },
                Expr::UnionTag {
                    union_expr: rhs_union_expr,
                    ..
                },
            ) => todo!(),

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
            ) => todo!(),

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
