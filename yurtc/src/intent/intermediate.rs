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
    pub fn compile(self) -> Result<Intent> {
        compile::compile(&self)
    }

    /// Helps out some `thing: T` by adding `self` as context.
    pub fn with_ii<T>(&self, thing: T) -> WithII<'_, T> {
        WithII { thing, ii: self }
    }

    pub fn insert_var(
        &mut self,
        mod_prefix: &str,
        name: &Ident,
        ty: Option<Type>,
    ) -> std::result::Result<VarKey, ParseError> {
        let full_name = self.add_top_level_symbol(mod_prefix, name, name.span.clone())?;
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
        let name = self.add_top_level_symbol(mod_prefix, name, span.clone())?;

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
        name: &Ident,
        span: Span,
    ) -> std::result::Result<String, ParseError> {
        let full_name = mod_prefix.to_owned() + &name.name;
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
}

/// A state specification with an optional type.
#[derive(Debug)]
pub struct State {
    name: Path,
    ty: Option<Type>,
    expr: ExprKey,
    span: Span,
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
