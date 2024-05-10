use crate::{
    error::{Error, ErrorEmitted, Handler, ParseError},
    expr::{self, Expr, Ident, Immediate},
    span::{empty_span, Span},
    types::{EnumDecl, EphemeralDecl, NewTypeDecl, Path, Type},
};
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    fmt::{self, Formatter},
};

mod analyse;
mod check_program_kind;
mod display;
mod transform;

slotmap::new_key_type! { pub struct VarKey; }
slotmap::new_key_type! { pub struct StateKey; }
slotmap::new_key_type! { pub struct ExprKey; }
slotmap::new_key_type! { pub struct CallKey; }

#[derive(Debug, Default, Clone)]
pub enum ProgramKind {
    #[default]
    Stateless,
    Stateful,
}

/// A Program is a collection of intents. There are two types of programs:
///
/// * Stateless: these must have a single II in the `BTreeMap` with an the name
/// `Program::ROOT_II_NAME` and cannot own state.
/// * Stateful: these must have at least one intent other than the root intent. The root intent,
/// which has the name `Program::ROOT_II_NAME`, contains everything that lives outside `intent { ..
/// }` declarations. Stateful programs are allowed to own state and the state is shared by all
/// intents.
#[derive(Debug, Default)]
pub struct Program {
    pub kind: ProgramKind,
    pub iis: BTreeMap<String, IntermediateIntent>,
}

impl Program {
    pub const ROOT_II_NAME: &'static str = "";

    pub fn compile(self, handler: &Handler) -> Result<Self, ErrorEmitted> {
        let type_checked = handler.scope(|handler| self.type_check(handler))?;
        handler.scope(|handler| type_checked.flatten(handler))
    }

    /// The root intent is the one named `Intents::ROOT_II_NAME`
    pub fn root_ii(&self) -> &IntermediateIntent {
        self.iis.get(Self::ROOT_II_NAME).unwrap()
    }

    /// The root intent is the one named `Intents::ROOT_II_NAME`
    pub fn root_ii_mut(&mut self) -> &mut IntermediateIntent {
        self.iis.get_mut(Self::ROOT_II_NAME).unwrap()
    }
}

/// An in-progress intent, possibly malformed or containing redundant information.  Designed to be
/// iterated upon and to be reduced to an [Intent].
#[derive(Debug, Default)]
pub struct IntermediateIntent {
    pub vars: slotmap::SlotMap<VarKey, Var>,
    pub var_types: slotmap::SecondaryMap<VarKey, Type>,

    pub states: slotmap::SlotMap<StateKey, State>,
    pub state_types: slotmap::SecondaryMap<StateKey, Type>,

    pub exprs: slotmap::SlotMap<ExprKey, Expr>,
    pub expr_types: slotmap::SecondaryMap<ExprKey, Type>,

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

    // A list of all storage variables in the order in which they were declared
    pub externs: Vec<Extern>,

    pub top_level_symbols: BTreeMap<String, Span>,
}

impl IntermediateIntent {
    /// Helps out some `thing: T` by adding `self` as context.
    pub fn with_ii<T>(&self, thing: T) -> WithII<T> {
        WithII { thing, ii: self }
    }

    /// Insert an expression with its type into the appropriate maps
    pub fn insert_expr(&mut self, expr: Expr, ty: Type) -> ExprKey {
        let expr_key = self.exprs.insert(expr);
        self.expr_types.insert(expr_key, ty);
        expr_key
    }

    pub fn insert_var(
        &mut self,
        handler: &Handler,
        mod_prefix: &str,
        local_scope: Option<&str>,
        name: &Ident,
        ty: Option<Type>,
    ) -> std::result::Result<VarKey, ErrorEmitted> {
        let full_name =
            self.add_top_level_symbol(handler, mod_prefix, local_scope, name, name.span.clone())?;
        let var_key = self.vars.insert(Var {
            name: full_name,
            span: name.span.clone(),
        });
        if let Some(ty) = ty {
            self.var_types.insert(var_key, ty);
        }

        Ok(var_key)
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
        let var_span = self
            .vars
            .get(var_key)
            .map_or_else(empty_span, |v| v.span.clone());

        let var_expr_key = self.exprs.insert(Expr::PathByKey(var_key, var_span));

        if let Some(Expr::Range { lb, ub, .. }) = self.exprs.get(expr_key).cloned() {
            let geq_expr_key = self.exprs.insert(Expr::BinaryOp {
                op: expr::BinaryOp::GreaterThanOrEqual,
                lhs: var_expr_key,
                rhs: lb,
                span: span.clone(),
            });
            self.constraints.push(ConstraintDecl {
                expr: geq_expr_key,
                span: span.clone(),
            });
            let geq_expr_key = self.exprs.insert(Expr::BinaryOp {
                op: expr::BinaryOp::LessThanOrEqual,
                lhs: var_expr_key,
                rhs: ub,
                span: span.clone(),
            });
            self.constraints.push(ConstraintDecl {
                expr: geq_expr_key,
                span,
            });
        } else {
            let eq_expr_key = self.exprs.insert(Expr::BinaryOp {
                op: expr::BinaryOp::Equal,
                lhs: var_expr_key,
                rhs: expr_key,
                span: span.clone(),
            });
            self.constraints.push(ConstraintDecl {
                expr: eq_expr_key,
                span,
            });
        }
    }

    pub fn insert_state(
        &mut self,
        handler: &Handler,
        mod_prefix: &str,
        name: &Ident,
        ty: Option<Type>,
        expr: ExprKey,
        span: Span,
    ) -> std::result::Result<StateKey, ErrorEmitted> {
        let name = self.add_top_level_symbol(handler, mod_prefix, None, name, span.clone())?;
        let state_key = self.states.insert(State {
            name,
            expr,
            span: span.clone(),
        });
        if let Some(ty) = ty {
            self.state_types.insert(state_key, ty);
        }

        Ok(state_key)
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
            .iter_mut()
            .for_each(|(_, expr)| expr.replace_one_to_one(old_expr, new_expr));

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
    }

    pub fn replace_exprs_by_map(&mut self, expr_map: &HashMap<ExprKey, ExprKey>) {
        self.exprs
            .iter_mut()
            .for_each(|(_, expr)| expr.replace_ref_by_map(expr_map));

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

    pub(crate) fn exprs(&self) -> Exprs {
        Exprs::new(self)
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
        let expr = self
            .exprs
            .get(expr_key)
            .expect("expr key must belong to ii.expr");

        if kind == VisitorKind::DepthFirstParentsBeforeChildren {
            // Visit the parent before recursing.
            f(expr_key, expr);
        }

        match expr {
            Expr::Error(_)
            | Expr::Immediate { .. }
            | Expr::PathByKey(_, _)
            | Expr::PathByName(_, _)
            | Expr::StorageAccess(_, _)
            | Expr::ExternalStorageAccess { .. }
            | Expr::MacroCall { .. } => {}

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

            Expr::Index { expr, index, .. } => {
                self.visitor_from_key(kind, *expr, f);
                self.visitor_from_key(kind, *index, f);
            }

            Expr::Tuple { fields, .. } => {
                for (_, field) in fields {
                    self.visitor_from_key(kind, *field, f);
                }
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
            .chain(self.states.iter().map(|(_, state)| state.expr))
            .chain(
                self.directives
                    .iter()
                    .filter_map(|(solve_func, _)| solve_func.get_expr().cloned()),
            )
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum VisitorKind {
    DepthFirstChildrenBeforeParents,
    DepthFirstParentsBeforeChildren,
}

/// A state specification with an optional type.
#[derive(Clone, Debug)]
pub struct State {
    pub name: Path,
    pub expr: ExprKey,
    pub span: Span,
}

impl DisplayWithII for StateKey {
    fn fmt(&self, f: &mut Formatter, ii: &IntermediateIntent) -> fmt::Result {
        let state = &ii.states[*self];
        write!(f, "state {}", state.name)?;
        if let Some(ty) = ii.state_types.get(*self) {
            write!(f, ": {}", ii.with_ii(ty))?;
        }
        write!(f, " = {}", ii.with_ii(&state.expr))
    }
}

/// A decision variable with an optional type.
#[derive(Clone, Debug)]
pub struct Var {
    pub name: Path,
    span: Span,
}

impl DisplayWithII for VarKey {
    fn fmt(&self, f: &mut Formatter, ii: &IntermediateIntent) -> fmt::Result {
        let var = &ii.vars[*self];
        write!(f, "var {}", var.name)?;
        if let Some(ty) = ii.var_types.get(*self) {
            write!(f, ": {}", ii.with_ii(ty))?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct ConstraintDecl {
    pub expr: ExprKey,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum BlockStatement {
    Constraint(ConstraintDecl),
    If(IfDecl),
}

impl BlockStatement {
    fn fmt_with_indent(
        &self,
        f: &mut Formatter,
        ii: &IntermediateIntent,
        indent: usize,
    ) -> fmt::Result {
        let indentation = " ".repeat(4 * indent);
        match self {
            Self::Constraint(constraint) => {
                writeln!(f, "{indentation}constraint {}", ii.with_ii(constraint.expr))
            }
            Self::If(if_decl) => if_decl.fmt_with_indent(f, ii, indent),
        }
    }
}

impl DisplayWithII for ConstraintDecl {
    fn fmt(&self, f: &mut Formatter, ii: &IntermediateIntent) -> fmt::Result {
        write!(f, "constraint {}", ii.with_ii(self.expr))
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
    fn fmt_with_indent(
        &self,
        f: &mut Formatter,
        ii: &IntermediateIntent,
        indent: usize,
    ) -> fmt::Result {
        let indentation = " ".repeat(4 * indent);
        writeln!(f, "{indentation}if {} {{", ii.with_ii(self.condition))?;
        for block_statament in &self.then_block {
            block_statament.fmt_with_indent(f, ii, indent + 1)?;
        }
        if let Some(else_block) = &self.else_block {
            writeln!(f, "{indentation}}} else {{")?;
            for block_statament in else_block {
                block_statament.fmt_with_indent(f, ii, indent + 1)?;
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

impl DisplayWithII for SolveFunc {
    fn fmt(&self, f: &mut Formatter, ii: &IntermediateIntent) -> std::fmt::Result {
        write!(f, "solve ")?;
        match self {
            SolveFunc::Satisfy => write!(f, "satisfy"),
            SolveFunc::Minimize(key) => write!(f, "minimize {}", ii.with_ii(key)),
            SolveFunc::Maximize(key) => write!(f, "maximize {}", ii.with_ii(key)),
        }
    }
}

#[derive(Clone, Debug)]
pub struct StorageVar {
    pub name: Path,
    pub ty: Type,
    pub span: Span,
}

impl DisplayWithII for StorageVar {
    fn fmt(&self, f: &mut Formatter, ii: &IntermediateIntent) -> fmt::Result {
        write!(f, "{}: {},", self.name, ii.with_ii(&self.ty))
    }
}

#[derive(Clone, Debug)]
pub struct Extern {
    pub name: Ident,
    pub address: Immediate,
    pub storage_vars: Vec<StorageVar>,
    pub span: Span,
}

#[derive(Clone, Copy)]
pub struct WithII<'a, T> {
    pub thing: T,
    pub ii: &'a IntermediateIntent,
}

impl<'a, T> WithII<'a, T> {
    pub fn new(thing: T, ii: &'a IntermediateIntent) -> Self {
        WithII { thing, ii }
    }
}

pub(crate) trait DisplayWithII {
    fn fmt(&self, f: &mut fmt::Formatter, ii: &IntermediateIntent) -> fmt::Result;
}

impl<T: DisplayWithII> fmt::Display for WithII<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.thing.fmt(f, self.ii)
    }
}

impl<T: DisplayWithII> DisplayWithII for &T {
    fn fmt(&self, f: &mut fmt::Formatter, ii: &IntermediateIntent) -> fmt::Result {
        (*self).fmt(f, ii)
    }
}

/// [`Exprs`] is an iterator for all the _reachable_ expressions in the IntermediateIntent.
///
/// Items are popped off the queue and are returned next.  If they're a branch then their children
/// are queued.  The visited set is updated and used to avoid following a branch multiple
/// times.
///
/// An alternative is to use `[IntermediateIntent::visitor]` which will also iterate
/// for each reachable expression but does not implement `Interator` and instead takes a closure.

#[derive(Debug)]
pub(crate) struct Exprs<'a> {
    ii: &'a IntermediateIntent,
    queue: Vec<ExprKey>,
    visited: HashSet<ExprKey>,
}

impl<'a> Exprs<'a> {
    fn new(ii: &'a IntermediateIntent) -> Exprs {
        // We start with all the constraint, directive and state exprs.
        let queue = ii.root_set().collect();

        // We also need to avoid macro calls which are not actually expressions.  We can
        // pre-populate the visited set to avoid them.
        let visited = HashSet::from_iter(ii.removed_macro_calls.keys());

        Exprs { ii, queue, visited }
    }
}

impl<'a> Iterator for Exprs<'a> {
    type Item = ExprKey;

    fn next(&mut self) -> Option<Self::Item> {
        if self.queue.is_empty() {
            return None;
        }

        macro_rules! queue_if_new {
            ($self: ident, $key: expr) => {
                if !$self.visited.contains($key) {
                    $self.queue.push(*$key);
                }
            };
        }

        // Get the next key and mark it as visited.
        let next_key = self.queue.pop().unwrap();
        self.visited.insert(next_key);

        // Push its children to the queue.
        match self.ii.exprs.get(next_key).expect("invalid key in queue") {
            Expr::UnaryOp { expr, .. } => queue_if_new!(self, expr),

            Expr::BinaryOp { lhs, rhs, .. } => {
                queue_if_new!(self, lhs);
                queue_if_new!(self, rhs);
            }

            Expr::IntrinsicCall { args, .. } => {
                for arg in args {
                    queue_if_new!(self, arg);
                }
            }

            Expr::Select {
                condition,
                then_expr,
                else_expr,
                ..
            } => {
                queue_if_new!(self, condition);
                queue_if_new!(self, then_expr);
                queue_if_new!(self, else_expr);
            }

            Expr::Array {
                elements,
                range_expr,
                ..
            } => {
                for el in elements {
                    queue_if_new!(self, el);
                }
                queue_if_new!(self, range_expr);
            }

            Expr::Index { expr, index, .. } => {
                queue_if_new!(self, expr);
                queue_if_new!(self, index);
            }

            Expr::Tuple { fields, .. } => {
                for (_, field) in fields {
                    queue_if_new!(self, field);
                }
            }

            Expr::TupleFieldAccess { tuple, .. } => queue_if_new!(self, tuple),

            Expr::Cast { value, .. } => queue_if_new!(self, value),

            Expr::In {
                value, collection, ..
            } => {
                queue_if_new!(self, value);
                queue_if_new!(self, collection);
            }

            Expr::Range { lb, ub, .. } => {
                queue_if_new!(self, lb);
                queue_if_new!(self, ub);
            }

            Expr::Generator {
                gen_ranges,
                conditions,
                body,
                ..
            } => {
                for (_, range) in gen_ranges {
                    queue_if_new!(self, range);
                }

                for cond in conditions {
                    queue_if_new!(self, cond);
                }

                queue_if_new!(self, body);
            }

            Expr::Error(_)
            | Expr::Immediate { .. }
            | Expr::StorageAccess(..)
            | Expr::ExternalStorageAccess { .. }
            | Expr::PathByKey(_, _)
            | Expr::PathByName(_, _)
            | Expr::MacroCall { .. } => {}
        };

        // If it has an array type then it also has an associated expr in the range.
        self.ii
            .expr_types
            .get(next_key)
            .and_then(|ty| ty.get_array_range_expr())
            .iter()
            .for_each(|range| queue_if_new!(self, range));

        Some(next_key)
    }
}
