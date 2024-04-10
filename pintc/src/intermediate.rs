use crate::{
    error::{Error, ErrorEmitted, Handler, ParseError},
    expr::{self, Expr, Ident},
    span::{empty_span, Span},
    types::{EnumDecl, EphemeralDecl, FnSig, NewTypeDecl, Path, Type},
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

    pub constraints: Vec<(ExprKey, Span)>,
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

    pub top_level_symbols: BTreeMap<String, Span>,
}

impl IntermediateIntent {
    /// Helps out some `thing: T` by adding `self` as context.
    pub fn with_ii<T>(&self, thing: T) -> WithII<T> {
        WithII { thing, ii: self }
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
            self.constraints.push((geq_expr_key, span.clone()));
            let geq_expr_key = self.exprs.insert(Expr::BinaryOp {
                op: expr::BinaryOp::LessThanOrEqual,
                lhs: var_expr_key,
                rhs: ub,
                span: span.clone(),
            });
            self.constraints.push((geq_expr_key, span));
        } else {
            let eq_expr_key = self.exprs.insert(Expr::BinaryOp {
                op: expr::BinaryOp::Equal,
                lhs: var_expr_key,
                rhs: expr_key,
                span: span.clone(),
            });
            self.constraints.push((eq_expr_key, span));
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

        self.constraints.iter_mut().for_each(|(expr, _)| {
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

        self.constraints.iter_mut().for_each(|(expr, _)| {
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

    /// Removes a key `expr_key` and all of its sub expressions from `self.exprs`.
    ///
    /// It *doee not* handle removing other objects that rely on `expr_key` such as constraints. It
    /// is up to the caller to decide what to do with those.
    ///
    /// Assumes that `expr_key` and its sub expressions belongs to `self.exprs`. Panics otherwise.
    pub(crate) fn remove_expr(&mut self, expr_key: ExprKey) {
        let expr = self
            .exprs
            .get(expr_key)
            .expect("expr key must belong to ii.expr")
            .clone();

        match &expr {
            Expr::UnaryOp { expr, .. } => self.remove_expr(*expr),
            Expr::BinaryOp { lhs, rhs, .. } => {
                self.remove_expr(*lhs);
                self.remove_expr(*rhs);
            }
            Expr::FnCall { args, .. } => {
                args.iter().for_each(|expr| self.remove_expr(*expr));
            }
            Expr::If {
                condition,
                then_block,
                else_block,
                ..
            } => {
                self.remove_expr(*condition);
                self.remove_expr(*then_block);
                self.remove_expr(*else_block);
            }
            Expr::Array { elements, .. } => {
                elements.iter().for_each(|expr| self.remove_expr(*expr));
            }
            Expr::ArrayElementAccess { array, index, .. } => {
                self.remove_expr(*array);
                self.remove_expr(*index);
            }
            Expr::Tuple { fields, .. } => {
                fields.iter().for_each(|(_, expr)| self.remove_expr(*expr));
            }
            Expr::TupleFieldAccess { tuple, .. } => {
                self.remove_expr(*tuple);
            }
            Expr::Cast { value, .. } => {
                // Should we handle the `ty` field here too since it also depends on an `ExprKey`
                self.remove_expr(*value);
            }
            Expr::In {
                value, collection, ..
            } => {
                self.remove_expr(*value);
                self.remove_expr(*collection);
            }
            Expr::Generator {
                gen_ranges,
                conditions,
                body,
                ..
            } => {
                gen_ranges
                    .iter()
                    .for_each(|(_, expr)| self.remove_expr(*expr));
                conditions.iter().for_each(|expr| self.remove_expr(*expr));
                self.remove_expr(*body);
            }
            // Nothing to do here. These do not depend on any `ExprKey`s
            Expr::Immediate { .. }
            | Expr::PathByName { .. }
            | Expr::PathByKey { .. }
            | Expr::MacroCall { .. }
            | Expr::Range { .. }
            | Expr::Error(_) => {}
        };

        self.exprs.remove(expr_key);
    }
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

/// A function (macro) to be applied and reduced where called.
// TODO: This isn't read yet but will need to be as a part of semantic analysis and optimisation.
#[allow(dead_code)]
#[derive(Debug)]
struct FnDecl {
    sig: FnSig,
    local_vars: Vec<(Var, Span)>,
    local_constraints: Vec<(Expr, Span)>,
    returned_constraint: Expr,
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
/// This maybe overkill -- implementing an interator for a tree structure is tricky.  Below we have
/// a queue and a visited set just to keep track of where we're up to.
///
/// Items are popped off the queue.  If they're a leaf they're next().  If they're a branch then
/// them then their children and queued.  Once each child is visited the parent is also returned.
/// The visited set is updated and used to avoid following a branch multiple times.
///
/// Like most iterators [`Exprs`] keeps a reference to the IntermediateIntent and so this iterator
/// is mostly useful for finding or filtering for specific exprs for further processing, e.g., in a
/// transform pass.  So perhaps, instead of this crazy impl Iterator it'd make more sense to have
/// just a `gather_by()` method which takes a predicate and returns a `Vec` of matches.  We'll see.

#[derive(Debug)]
pub(crate) struct Exprs<'a> {
    ii: &'a IntermediateIntent,
    queue: Vec<ExprKey>,
    visited: HashSet<ExprKey>,
}

impl<'a> Exprs<'a> {
    fn new(ii: &'a IntermediateIntent) -> Exprs {
        Exprs {
            ii,
            queue: ii.constraints.iter().rev().map(|c| c.0).collect(),
            visited: HashSet::new(),
        }
    }
}

impl<'a> Iterator for Exprs<'a> {
    type Item = ExprKey;

    fn next(&mut self) -> Option<Self::Item> {
        if self.queue.is_empty() {
            return None;
        }

        macro_rules! push_if_new {
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
            Expr::UnaryOp { expr, .. } => push_if_new!(self, expr),

            Expr::BinaryOp { lhs, rhs, .. } => {
                push_if_new!(self, lhs);
                push_if_new!(self, rhs);
            }

            Expr::FnCall { args, .. } => {
                for arg in args {
                    push_if_new!(self, arg);
                }
            }

            Expr::If {
                condition,
                then_block,
                else_block,
                ..
            } => {
                push_if_new!(self, condition);
                push_if_new!(self, then_block);
                push_if_new!(self, else_block);
            }

            Expr::Array {
                elements,
                range_expr,
                ..
            } => {
                for el in elements {
                    push_if_new!(self, el);
                }
                push_if_new!(self, range_expr);
            }

            Expr::ArrayElementAccess { array, index, .. } => {
                push_if_new!(self, array);
                push_if_new!(self, index);
            }

            Expr::Tuple { fields, .. } => {
                for (_, field) in fields {
                    push_if_new!(self, field);
                }
            }

            Expr::TupleFieldAccess { tuple, .. } => push_if_new!(self, tuple),

            Expr::Cast { value, .. } => push_if_new!(self, value),

            Expr::In {
                value, collection, ..
            } => {
                push_if_new!(self, value);
                push_if_new!(self, collection);
            }

            Expr::Range { lb, ub, .. } => {
                push_if_new!(self, lb);
                push_if_new!(self, ub);
            }

            Expr::Generator {
                gen_ranges,
                conditions,
                body,
                ..
            } => {
                for (_, range) in gen_ranges {
                    push_if_new!(self, range);
                }

                for cond in conditions {
                    push_if_new!(self, cond);
                }

                push_if_new!(self, body);
            }

            Expr::Error(_)
            | Expr::Immediate { .. }
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
            .for_each(|range| push_if_new!(self, range));

        Some(next_key)
    }
}
