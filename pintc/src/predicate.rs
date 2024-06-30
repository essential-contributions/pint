use crate::{
    error::{Error, ErrorEmitted, Handler, ParseError},
    expr::{self, Expr, Ident},
    span::Span,
    types::{EnumDecl, EphemeralDecl, NewTypeDecl, Path, Type},
};
use exprs::ExprsIter;
pub use exprs::{ExprKey, Exprs};
use fxhash::FxHashMap;
use pint_abi_types::{KeyedVarABI, PredicateABI, ProgramABI};
pub use states::{State, StateKey, States};
use std::{
    collections::BTreeMap,
    fmt::{self, Formatter},
};
pub use vars::{Var, VarKey, Vars};

mod analyse;
mod check_program_kind;
mod display;
mod exprs;
mod states;
mod transform;
mod vars;

slotmap::new_key_type! { pub struct CallKey; }

#[derive(Debug, Default, Clone)]
pub enum ProgramKind {
    #[default]
    Stateless,
    Stateful,
}

/// A Program is a collection of predicates. There are two types of programs:
///
/// * Stateless: these must have a single Pred in the `BTreeMap` with an the name
/// `Program::ROOT_PRED_NAME` and cannot own state.
/// * Stateful: these must have at least one predicate other than the root predicate. The root predicate,
/// which has the name `Program::ROOT_PRED_NAME`, contains everything that lives outside `predicate { ..
/// }` declarations. Stateful programs are allowed to own state and the state is shared by all
/// predicates.
#[derive(Debug, Default)]
pub struct Program {
    pub kind: ProgramKind,
    pub preds: BTreeMap<String, Predicate>,

    pub consts: FxHashMap<String, Const>,
}

impl Program {
    pub const ROOT_PRED_NAME: &'static str = "";

    pub fn compile(self, handler: &Handler) -> Result<Self, ErrorEmitted> {
        let type_checked = handler.scope(|handler| self.type_check(handler))?;
        handler.scope(|handler| type_checked.flatten(handler))
    }

    /// The root predicate is the one named `Predicates::ROOT_PRED_NAME`
    pub fn root_pred(&self) -> &Predicate {
        self.preds.get(Self::ROOT_PRED_NAME).unwrap()
    }

    /// The root predicate is the one named `Predicates::ROOT_PRED_NAME`
    pub fn root_pred_mut(&mut self) -> &mut Predicate {
        self.preds.get_mut(Self::ROOT_PRED_NAME).unwrap()
    }

    /// Generates a `ProgramABI` given a `Program`
    pub fn abi(&self, handler: &Handler) -> Result<ProgramABI, ErrorEmitted> {
        Ok(ProgramABI {
            predicates: self
                .preds
                .values()
                .map(|pred| pred.abi(handler))
                .collect::<Result<_, _>>()?,
            storage: self
                .root_pred()
                .storage
                .as_ref()
                .map(|(storage, _)| {
                    storage
                        .iter()
                        .enumerate()
                        .map(|(index, StorageVar { name, ty, .. })| {
                            // The key of `ty` is either the `index` if the storage type is
                            // primitive or a map, or it's `[index, 0]`. The `0` here is a
                            // placeholder for offsets.
                            Ok(KeyedVarABI {
                                name: name.to_string(),
                                ty: if ty.is_any_primitive() || ty.is_map() {
                                    ty.abi_with_key(handler, vec![Some(index)], self.root_pred())?
                                } else {
                                    ty.abi_with_key(
                                        handler,
                                        vec![Some(index), Some(0)],
                                        self.root_pred(),
                                    )?
                                },
                            })
                        })
                        .collect::<Result<_, _>>()
                })
                .unwrap_or(Ok(vec![]))?,
        })
    }
}

/// An in-progress predicate, possibly malformed or containing redundant information.  Designed to
/// be iterated upon and to be reduced to a [Predicate].
#[derive(Debug, Default)]
pub struct Predicate {
    pub name: String,

    pub vars: Vars,
    pub states: States,
    pub exprs: Exprs,

    pub constraints: Vec<ConstraintDecl>,
    pub if_decls: Vec<IfDecl>,
    pub directives: Vec<(SolveFunc, Span)>,

    pub ephemerals: Vec<EphemeralDecl>,
    pub enums: Vec<EnumDecl>,
    pub new_types: Vec<NewTypeDecl>,

    // Each of the initialised variables.  Used by type inference.
    pub var_inits: slotmap::SecondaryMap<VarKey, ExprKey>,

    // CallKey is used in a secondary map in the parser context to access the actual call data.
    pub calls: slotmap::SlotMap<CallKey, Path>,

    // Keep track of obsolete expanded macro calls in case they're erroneously depended upon.
    pub removed_macro_calls: slotmap::SecondaryMap<ExprKey, Span>,

    // A list of all storage variables in the order in which they were declared
    pub storage: Option<(Vec<StorageVar>, Span)>,

    // A list of all availabe interfaces
    pub interfaces: Vec<Interface>,

    // A list of all availabe interface instances
    pub interface_instances: Vec<InterfaceInstance>,

    // A list of all availabe predicate instances
    pub predicate_instances: Vec<PredicateInstance>,

    pub top_level_symbols: FxHashMap<String, Span>,
}

impl Predicate {
    pub fn new(name: String) -> Self {
        Self {
            name,
            ..Default::default()
        }
    }

    pub fn is_root(&self) -> bool {
        self.name == Program::ROOT_PRED_NAME
    }

    /// Generate a `PredicateABI` given an `Predicate`
    pub fn abi(&self, handler: &Handler) -> Result<PredicateABI, ErrorEmitted> {
        Ok(PredicateABI {
            name: self.name.clone(),
            vars: self
                .vars()
                .filter(|(_, var)| !var.is_pub)
                .map(|(var_key, _)| var_key.abi(self))
                .collect::<Result<_, _>>()?,
            pub_vars: self
                .vars()
                .filter(|(_, var)| var.is_pub)
                .enumerate()
                .map(|(index, (var_key, Var { name, .. }))| {
                    Ok(KeyedVarABI {
                        name: name.to_string(),
                        ty: {
                            let ty = var_key.get_ty(self);
                            if ty.is_any_primitive() || ty.is_map() {
                                ty.abi_with_key(handler, vec![Some(index)], self)?
                            } else {
                                ty.abi_with_key(handler, vec![Some(index), Some(0)], self)?
                            }
                        },
                    })
                })
                .collect::<Result<Vec<_>, _>>()?,
        })
    }

    /// Helps out some `thing: T` by adding `self` as context.
    pub fn with_pred<T>(&self, thing: T) -> WithPred<T> {
        WithPred { thing, pred: self }
    }

    pub fn insert_ephemeral(
        &mut self,
        handler: &Handler,
        mod_prefix: &str,
        name: &Ident,
        ty: Type,
    ) -> std::result::Result<(), ErrorEmitted> {
        let full_name = Self::make_full_symbol(mod_prefix, None, name);
        if !self
            .ephemerals
            .iter()
            .any(|eph_decl| eph_decl.name == full_name)
        {
            self.add_top_level_symbol_with_name(
                handler,
                name,
                full_name.clone(),
                name.span.clone(),
            )?;
            self.ephemerals.push(EphemeralDecl {
                name: full_name,
                ty,
                span: name.span.clone(),
            });
        }
        Ok(())
    }

    pub fn insert_eq_or_ineq_constraint(&mut self, var_key: VarKey, expr_key: ExprKey, span: Span) {
        let var_span = &var_key.get(self).span;

        let var_expr_key = self.exprs.insert(
            Expr::PathByKey(var_key, var_span.clone()),
            Type::Unknown(var_span.clone()),
        );

        if let Some(Expr::Range { lb, ub, .. }) = expr_key.try_get(self).cloned() {
            let geq_expr_key = self.exprs.insert(
                Expr::BinaryOp {
                    op: expr::BinaryOp::GreaterThanOrEqual,
                    lhs: var_expr_key,
                    rhs: lb,
                    span: span.clone(),
                },
                Type::Unknown(span.clone()),
            );
            self.constraints.push(ConstraintDecl {
                expr: geq_expr_key,
                span: span.clone(),
            });
            let geq_expr_key = self.exprs.insert(
                Expr::BinaryOp {
                    op: expr::BinaryOp::LessThanOrEqual,
                    lhs: var_expr_key,
                    rhs: ub,
                    span: span.clone(),
                },
                Type::Unknown(span.clone()),
            );
            self.constraints.push(ConstraintDecl {
                expr: geq_expr_key,
                span,
            });
        } else {
            let eq_expr_key = self.exprs.insert(
                Expr::BinaryOp {
                    op: expr::BinaryOp::Equal,
                    lhs: var_expr_key,
                    rhs: expr_key,
                    span: span.clone(),
                },
                Type::Unknown(span.clone()),
            );
            self.constraints.push(ConstraintDecl {
                expr: eq_expr_key,
                span,
            });
        }
    }

    fn make_full_symbol(mod_prefix: &str, local_scope: Option<&str>, name: &Ident) -> String {
        let local_scope_str = local_scope
            .map(|ls| ls.to_owned() + "::")
            .unwrap_or_default();
        mod_prefix.to_owned() + &local_scope_str + &name.name
    }

    fn add_top_level_symbol_with_name(
        &mut self,
        handler: &Handler,
        short_name: &Ident,
        full_name: String,
        span: Span,
    ) -> std::result::Result<String, ErrorEmitted> {
        self.top_level_symbols
            .get(&full_name)
            .map(|prev_span| {
                // Name clash.
                Err(handler.emit_err(Error::Parse {
                    error: ParseError::NameClash {
                        sym: short_name.name.clone(),
                        span: short_name.span.clone(),
                        prev_span: prev_span.clone(),
                    },
                }))
            })
            .unwrap_or_else(|| {
                // Not found in the symbol table.
                self.top_level_symbols.insert(full_name.clone(), span);
                Ok(full_name)
            })
    }

    pub fn add_top_level_symbol(
        &mut self,
        handler: &Handler,
        mod_prefix: &str,
        local_scope: Option<&str>,
        name: &Ident,
        span: Span,
    ) -> std::result::Result<String, ErrorEmitted> {
        let full_name = Self::make_full_symbol(mod_prefix, local_scope, name);
        self.add_top_level_symbol_with_name(handler, name, full_name, span)
    }

    pub fn replace_exprs(&mut self, old_expr: ExprKey, new_expr: ExprKey) {
        self.exprs
            .update_exprs(|_, expr| expr.replace_one_to_one(old_expr, new_expr));

        self.constraints
            .iter_mut()
            .for_each(|ConstraintDecl { expr, .. }| {
                if *expr == old_expr {
                    *expr = new_expr;
                }
            });

        self.directives.iter_mut().for_each(|(solve_func, _)| {
            if let Some(expr) = solve_func.get_mut_expr() {
                if *expr == old_expr {
                    *expr = new_expr;
                }
            }
        });

        self.var_inits.iter_mut().for_each(|(_, expr)| {
            if *expr == old_expr {
                *expr = new_expr;
            }
        });

        self.interface_instances
            .iter_mut()
            .for_each(|InterfaceInstance { address, .. }| {
                if *address == old_expr {
                    *address = new_expr;
                }
            });

        self.predicate_instances
            .iter_mut()
            .for_each(|PredicateInstance { address, .. }| {
                if *address == old_expr {
                    *address = new_expr;
                }
            });
    }

    pub fn replace_exprs_by_map(&mut self, expr_map: &FxHashMap<ExprKey, ExprKey>) {
        self.exprs
            .update_exprs(|_, expr| expr.replace_ref_by_map(expr_map));

        self.constraints
            .iter_mut()
            .for_each(|ConstraintDecl { expr, .. }| {
                if let Some(new_expr) = expr_map.get(expr) {
                    *expr = *new_expr;
                }
            });

        self.directives.iter_mut().for_each(|(solve_func, _)| {
            if let Some(expr) = solve_func.get_mut_expr() {
                if let Some(new_expr) = expr_map.get(expr) {
                    *expr = *new_expr;
                }
            }
        });

        self.var_inits.iter_mut().for_each(|(_, expr)| {
            if let Some(new_expr) = expr_map.get(expr) {
                *expr = *new_expr;
            }
        });
    }

    pub(crate) fn exprs(&self) -> ExprsIter {
        ExprsIter::new(self)
    }

    pub(crate) fn visitor<F: FnMut(ExprKey, &Expr)>(&self, kind: VisitorKind, mut f: F) {
        for expr_key in self.root_set() {
            self.visitor_from_key(kind, expr_key, &mut |k, e| f(k, e));
        }
    }

    // Panics if `root_key` is invalid.
    fn visitor_from_key(
        &self,
        kind: VisitorKind,
        expr_key: ExprKey,
        f: &mut impl FnMut(ExprKey, &Expr),
    ) {
        let expr = expr_key.get(self);

        if kind == VisitorKind::DepthFirstParentsBeforeChildren {
            // Visit the parent before recursing.
            f(expr_key, expr);
        }

        match expr {
            Expr::Error(_)
            | Expr::PathByKey(_, _)
            | Expr::PathByName(_, _)
            | Expr::StorageAccess(_, _)
            | Expr::ExternalStorageAccess { .. }
            | Expr::MacroCall { .. }
            | Expr::Immediate { .. } => {}

            Expr::Array {
                elements,
                range_expr,
                ..
            } => {
                for element in elements {
                    self.visitor_from_key(kind, *element, f);
                }
                self.visitor_from_key(kind, *range_expr, f);
            }

            Expr::Tuple { fields, .. } => {
                for (_, field) in fields {
                    self.visitor_from_key(kind, *field, f);
                }
            }

            Expr::UnaryOp { expr, .. } => self.visitor_from_key(kind, *expr, f),

            Expr::BinaryOp { lhs, rhs, .. } => {
                self.visitor_from_key(kind, *lhs, f);
                self.visitor_from_key(kind, *rhs, f);
            }

            Expr::IntrinsicCall { args, .. } => {
                for arg in args {
                    self.visitor_from_key(kind, *arg, f);
                }
            }

            Expr::Select {
                condition,
                then_expr,
                else_expr,
                ..
            } => {
                self.visitor_from_key(kind, *condition, f);
                self.visitor_from_key(kind, *then_expr, f);
                self.visitor_from_key(kind, *else_expr, f);
            }

            Expr::Index { expr, index, .. } => {
                self.visitor_from_key(kind, *expr, f);
                self.visitor_from_key(kind, *index, f);
            }

            Expr::TupleFieldAccess { tuple, .. } => {
                self.visitor_from_key(kind, *tuple, f);
            }

            Expr::Cast { value, .. } => self.visitor_from_key(kind, *value, f),

            Expr::In {
                value, collection, ..
            } => {
                self.visitor_from_key(kind, *value, f);
                self.visitor_from_key(kind, *collection, f);
            }

            Expr::Range { lb, ub, .. } => {
                self.visitor_from_key(kind, *lb, f);
                self.visitor_from_key(kind, *ub, f);
            }

            Expr::Generator {
                gen_ranges,
                conditions,
                body,
                ..
            } => {
                for (_, range) in gen_ranges {
                    self.visitor_from_key(kind, *range, f);
                }
                for condition in conditions {
                    self.visitor_from_key(kind, *condition, f);
                }
                self.visitor_from_key(kind, *body, f);
            }
        }

        if kind == VisitorKind::DepthFirstChildrenBeforeParents {
            // Visit the parent after recursing.
            f(expr_key, expr);
        }
    }

    /// Return an iterator to the 'root set' of expressions, based on the constraints, states and
    /// directives.
    fn root_set(&self) -> impl Iterator<Item = ExprKey> + '_ {
        self.constraints
            .iter()
            .map(|c| c.expr)
            .chain(self.states().map(|(_, state)| state.expr))
            .chain(
                self.directives
                    .iter()
                    .filter_map(|(solve_func, _)| solve_func.get_expr().cloned()),
            )
            .chain(self.interface_instances.iter().map(|ii| ii.address))
            .chain(self.predicate_instances.iter().map(|pi| pi.address))
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum VisitorKind {
    DepthFirstChildrenBeforeParents,
    DepthFirstParentsBeforeChildren,
}

#[derive(Clone, Debug)]
pub struct Const {
    pub(crate) expr: ExprKey,
    pub(crate) decl_ty: Type,
}

impl DisplayWithPred for Const {
    fn fmt(&self, f: &mut Formatter, pred: &Predicate) -> fmt::Result {
        if !self.decl_ty.is_unknown() {
            write!(f, ": {}", pred.with_pred(&self.decl_ty))?;
        }

        write!(f, " = {}", pred.with_pred(self.expr))
    }
}

#[derive(Clone, Debug)]
pub struct ConstraintDecl {
    pub expr: ExprKey,
    pub span: Span,
}

impl DisplayWithPred for ConstraintDecl {
    fn fmt(&self, f: &mut Formatter, pred: &Predicate) -> fmt::Result {
        write!(f, "constraint {}", pred.with_pred(self.expr))
    }
}

#[derive(Clone, Debug)]
pub enum BlockStatement {
    Constraint(ConstraintDecl),
    If(IfDecl),
}

impl BlockStatement {
    fn fmt_with_indent(&self, f: &mut Formatter, pred: &Predicate, indent: usize) -> fmt::Result {
        let indentation = " ".repeat(4 * indent);
        match self {
            Self::Constraint(constraint) => {
                writeln!(
                    f,
                    "{indentation}constraint {}",
                    pred.with_pred(constraint.expr)
                )
            }
            Self::If(if_decl) => if_decl.fmt_with_indent(f, pred, indent),
        }
    }
}

#[derive(Clone, Debug)]
pub struct IfDecl {
    pub condition: ExprKey,
    pub then_block: Vec<BlockStatement>,
    pub else_block: Option<Vec<BlockStatement>>,
    pub span: Span,
}

impl IfDecl {
    fn fmt_with_indent(&self, f: &mut Formatter, pred: &Predicate, indent: usize) -> fmt::Result {
        let indentation = " ".repeat(4 * indent);
        writeln!(f, "{indentation}if {} {{", pred.with_pred(self.condition))?;
        for block_statament in &self.then_block {
            block_statament.fmt_with_indent(f, pred, indent + 1)?;
        }
        if let Some(else_block) = &self.else_block {
            writeln!(f, "{indentation}}} else {{")?;
            for block_statament in else_block {
                block_statament.fmt_with_indent(f, pred, indent + 1)?;
            }
        }
        writeln!(f, "{indentation}}}")
    }
}

#[derive(Clone, Debug)]
pub enum SolveFunc {
    Satisfy,
    Minimize(ExprKey),
    Maximize(ExprKey),
}

impl SolveFunc {
    pub(crate) fn get_expr(&self) -> Option<&ExprKey> {
        match self {
            SolveFunc::Satisfy => None,
            SolveFunc::Minimize(e) | SolveFunc::Maximize(e) => Some(e),
        }
    }

    pub(crate) fn get_mut_expr(&mut self) -> Option<&mut ExprKey> {
        match self {
            SolveFunc::Satisfy => None,
            SolveFunc::Minimize(e) | SolveFunc::Maximize(e) => Some(e),
        }
    }
}

impl DisplayWithPred for SolveFunc {
    fn fmt(&self, f: &mut Formatter, pred: &Predicate) -> std::fmt::Result {
        write!(f, "solve ")?;
        match self {
            SolveFunc::Satisfy => write!(f, "satisfy"),
            SolveFunc::Minimize(key) => write!(f, "minimize {}", pred.with_pred(key)),
            SolveFunc::Maximize(key) => write!(f, "maximize {}", pred.with_pred(key)),
        }
    }
}

#[derive(Clone, Debug)]
pub struct StorageVar {
    pub name: Ident,
    pub ty: Type,
    pub span: Span,
}

impl DisplayWithPred for StorageVar {
    fn fmt(&self, f: &mut Formatter, pred: &Predicate) -> fmt::Result {
        write!(f, "{}: {},", self.name.name, pred.with_pred(&self.ty))
    }
}

/// A a predicate interface that belong in an `Interface`.
#[derive(Clone, Debug)]
pub struct PredicateInterface {
    pub name: Ident,
    pub vars: Vec<InterfaceVar>,
    pub span: Span,
}

/// A declaration inside an `Interface`. This could either be a `storage` declaration or a
/// predicate interface declaration
#[derive(Clone, Debug)]
pub enum InterfaceDecl {
    StorageDecl((Vec<StorageVar>, Span)),
    PredicateInterface(PredicateInterface),
}

/// full interface to an external contract
#[derive(Clone, Debug)]
pub struct Interface {
    pub name: Ident,
    pub storage: Option<(Vec<StorageVar>, Span)>,
    pub predicate_interfaces: Vec<PredicateInterface>,
    pub span: Span,
}

/// A decision variable that lives inside a predicate interface. Unlike `Var`, the type here is not
/// optional
#[derive(Clone, Debug)]
pub struct InterfaceVar {
    pub name: Ident,
    pub ty: Type,
    pub span: Span,
}

/// An interface instance that specifies an address
#[derive(Clone, Debug)]
pub struct InterfaceInstance {
    pub name: Ident,
    pub interface: Path,
    pub address: ExprKey,
    pub span: Span,
}

/// A predicate instance that specifies an address
#[derive(Clone, Debug)]
pub struct PredicateInstance {
    pub name: Ident,
    pub interface_instance: Path,
    pub predicate: Ident,
    pub address: ExprKey,
    pub span: Span,
}

#[derive(Clone, Copy)]
pub struct WithPred<'a, T> {
    pub thing: T,
    pub pred: &'a Predicate,
}

impl<'a, T> WithPred<'a, T> {
    pub fn new(thing: T, pred: &'a Predicate) -> Self {
        WithPred { thing, pred }
    }
}

pub(crate) trait DisplayWithPred {
    fn fmt(&self, f: &mut fmt::Formatter, pred: &Predicate) -> fmt::Result;
}

impl<T: DisplayWithPred> fmt::Display for WithPred<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.thing.fmt(f, self.pred)
    }
}

impl<T: DisplayWithPred> DisplayWithPred for &T {
    fn fmt(&self, f: &mut fmt::Formatter, pred: &Predicate) -> fmt::Result {
        (*self).fmt(f, pred)
    }
}
