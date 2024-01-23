use crate::{
    contract::{ContractDecl, InterfaceDecl},
    error::{CompileError, ParseError},
    expr::{self, Expr, Ident},
    intent::{Intent, Path},
    span::{empty_span, Span},
    types::{EnumDecl, FnSig, NewTypeDecl, Type},
};
use std::{
    collections::HashMap,
    fmt::{self, Formatter},
};

mod compile;
mod display;
mod transform;

type Result<T> = std::result::Result<T, CompileError>;

slotmap::new_key_type! { pub struct VarKey; }
slotmap::new_key_type! { pub struct ExprKey; }
slotmap::new_key_type! { pub struct CallKey; }

/// An in-progress intent, possibly malformed or containing redundant information.  Designed to be
/// iterated upon and to be reduced to an [Intent].
#[derive(Debug, Default)]
pub struct IntermediateIntent {
    pub vars: slotmap::SlotMap<VarKey, Var>,
    pub exprs: slotmap::SlotMap<ExprKey, Expr>,

    pub states: Vec<State>,
    pub constraints: Vec<(ExprKey, Span)>,
    pub directives: Vec<(SolveFunc, Span)>,

    pub enums: Vec<EnumDecl>,
    pub new_types: Vec<NewTypeDecl>,
    pub interfaces: Vec<InterfaceDecl>,
    pub contracts: Vec<ContractDecl>,
    pub externs: Vec<(Vec<FnSig>, Span)>,

    // CallKey is used in a secondary map in the parser context to access the actual call data.
    pub calls: slotmap::SlotMap<CallKey, Path>,

    pub top_level_symbols: HashMap<String, Span>,
}

impl IntermediateIntent {
    pub fn compile(&mut self) -> Result<Intent> {
        compile::compile(self)
    }

    pub fn flatten(self) -> Result<IntermediateIntent> {
        compile::flatten(self)
    }

    /// Helps out some `thing: T` by adding `self` as context.
    pub fn with_ii<T>(&self, thing: T) -> WithII<'_, T> {
        WithII { thing, ii: self }
    }

    pub fn insert_var(
        &mut self,
        mod_prefix: &str,
        local_scope: Option<&str>,
        name: &Ident,
        ty: Option<Type>,
    ) -> std::result::Result<VarKey, ParseError> {
        let full_name =
            self.add_top_level_symbol(mod_prefix, local_scope, name, name.span.clone())?;
        let var_key = self.vars.insert(Var {
            name: full_name,
            ty,
            span: name.span.clone(),
        });

        Ok(var_key)
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
        mod_prefix: &str,
        name: &Ident,
        ty: Option<Type>,
        expr: ExprKey,
        span: Span,
    ) -> std::result::Result<(), ParseError> {
        let name = self.add_top_level_symbol(mod_prefix, None, name, span.clone())?;

        self.states.push(State {
            name,
            ty,
            expr,
            span,
        });

        Ok(())
    }

    pub fn add_top_level_symbol(
        &mut self,
        mod_prefix: &str,
        local_scope: Option<&str>,
        name: &Ident,
        span: Span,
    ) -> std::result::Result<String, ParseError> {
        let local_scope_str = local_scope
            .map(|ls| ls.to_owned() + "::")
            .unwrap_or_default();
        let full_name = mod_prefix.to_owned() + &local_scope_str + &name.name;
        self.top_level_symbols
            .get(&full_name)
            .map(|prev_span| {
                // Name clash.
                Err(ParseError::NameClash {
                    sym: name.name.clone(),
                    span: name.span.clone(),
                    prev_span: prev_span.clone(),
                })
            })
            .unwrap_or_else(|| {
                // Not found in the symbol table.
                self.top_level_symbols.insert(full_name.clone(), span);
                Ok(full_name)
            })
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
            Expr::ForAll {
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
#[derive(Clone, Debug, PartialEq)]
pub struct State {
    pub(crate) name: Path,
    pub(crate) ty: Option<Type>,
    pub(crate) expr: ExprKey,
    pub(crate) span: Span,
}

impl DisplayWithII for &State {
    fn fmt(&self, f: &mut Formatter, ii: &IntermediateIntent) -> fmt::Result {
        write!(f, "state {}", self.name)?;
        if let Some(ty) = &self.ty {
            write!(f, ": {}", ii.with_ii(ty))?;
        }
        write!(f, " = {}", ii.with_ii(&self.expr))?;
        Ok(())
    }
}

/// A decision variable with an optional type.
#[derive(Clone, Debug)]
pub struct Var {
    pub(crate) name: Path,
    ty: Option<Type>,
    span: Span,
}

impl DisplayWithII for VarKey {
    fn fmt(&self, f: &mut Formatter, ii: &IntermediateIntent) -> fmt::Result {
        write!(f, "{}", ii.with_ii(&ii.vars[*self]))
    }
}

impl DisplayWithII for &Var {
    fn fmt(&self, f: &mut Formatter, ii: &IntermediateIntent) -> fmt::Result {
        write!(f, "var {}", self.name)?;
        if let Some(ty) = &self.ty {
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

impl DisplayWithII for SolveFunc {
    fn fmt(&self, f: &mut Formatter<'_>, ii: &IntermediateIntent) -> std::fmt::Result {
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ii: &IntermediateIntent) -> fmt::Result;
}

impl<T: DisplayWithII> fmt::Display for WithII<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.thing.fmt(f, self.ii)
    }
}

impl<T: DisplayWithII> DisplayWithII for &T {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ii: &IntermediateIntent) -> fmt::Result {
        (*self).fmt(f, ii)
    }
}
