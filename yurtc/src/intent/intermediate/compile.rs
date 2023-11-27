use crate::{
    error::CompileError,
    intent::{self, Expression, Intent, Solve},
    span::{empty_span, Span},
};

use super::{Expr, ExprKey, IntermediateIntent, SolveFunc, State, Type, Var};

pub(super) fn compile(context: IntermediateIntent) -> super::Result<Intent> {
    // Perform all the verification, checks and optimisations.
    // ... TODO ...

    Ok(Intent {
        states: convert_states(&context)?,
        vars: convert_vars(&context)?,
        constraints: convert_constraints(&context)?,
        directive: convert_directive(&context)?,
    })
}

fn convert_states(context: &IntermediateIntent) -> super::Result<Vec<intent::State>> {
    context
        .states
        .iter()
        .map(
            |State {
                 name,
                 ty,
                 expr: expr_key,
                 span,
             }| {
                ty.as_ref()
                    .ok_or_else(|| CompileError::Internal {
                        msg: "Found untyped state.",
                        span: span.clone(),
                    })
                    .and_then(|ty| {
                        convert_type(ty, span).and_then(|ty| {
                            convert_expr_key(context, *expr_key, span).map(|expr| intent::State {
                                name: name.clone(),
                                ty,
                                expr,
                            })
                        })
                    })
            },
        )
        .collect()
}

fn convert_vars(context: &IntermediateIntent) -> super::Result<Vec<intent::Variable>> {
    context
        .vars
        .iter()
        .map(|(_, Var { name, ty, span })| {
            ty.as_ref()
                .ok_or_else(|| CompileError::Internal {
                    msg: "Found untyped variable.",
                    span: span.clone(),
                })
                .and_then(|ty| convert_type(ty, span))
                .map(|ty| intent::Variable {
                    name: name.clone(),
                    ty,
                })
        })
        .collect()
}

fn convert_constraints(context: &IntermediateIntent) -> super::Result<Vec<Expression>> {
    context
        .constraints
        .iter()
        .map(|(expr_key, span)| convert_expr_key(context, *expr_key, span))
        .collect()
}

fn convert_directive(context: &IntermediateIntent) -> super::Result<Solve> {
    let (directive, span) = match context.directives.first() {
        Some(tuple) => tuple,
        None => {
            return Err(CompileError::Internal {
                msg: "Missing directive during final compile.",
                span: empty_span(),
            })
        }
    };

    Ok(match directive {
        SolveFunc::Satisfy => Solve::Satisfy,
        SolveFunc::Maximize(expr_key) => {
            Solve::Maximize(convert_expr_key(context, *expr_key, span)?)
        }
        SolveFunc::Minimize(expr_key) => {
            Solve::Minimize(convert_expr_key(context, *expr_key, span)?)
        }
    })
}

fn convert_expr_key(
    context: &IntermediateIntent,
    expr_key: ExprKey,
    span: &Span,
) -> super::Result<Expression> {
    context
        .exprs
        .get(expr_key)
        .ok_or_else(|| CompileError::Internal {
            msg: "Unable to resolve expr key.",
            span: span.clone(),
        })
        .and_then(|expr| convert_expr(context, expr, span))
}

fn convert_expr(
    context: &IntermediateIntent,
    expr: &Expr,
    span: &Span,
) -> super::Result<Expression> {
    // Ugh.
    match expr {
        super::Expr::Immediate { value, .. } => Ok(Expression::Immediate(value.clone())),

        super::Expr::PathByName(path, _) => Ok(Expression::Path(path.clone())),

        super::Expr::PathByKey(var_key, _) => context
            .vars
            .get(*var_key)
            .map(|var| Expression::Path(var.name.clone()))
            .ok_or_else(|| CompileError::Internal {
                msg: "Unable to resolve expr key.",
                span: span.clone(),
            }),

        super::Expr::UnaryOp {
            op,
            expr: expr_key,
            span,
        } => convert_expr_key(context, *expr_key, span).map(|expr| Expression::UnaryOp {
            op: *op,
            expr: Box::new(expr),
        }),

        super::Expr::BinaryOp { op, lhs, rhs, span } => convert_expr_key(context, *lhs, span)
            .and_then(|lhs| {
                convert_expr_key(context, *rhs, span).map(|rhs| Expression::BinaryOp {
                    op: *op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                })
            }),

        super::Expr::Call { name, args, span } => args
            .iter()
            .map(|arg| convert_expr_key(context, *arg, span))
            .collect::<super::Result<_>>()
            .map(|args| Expression::Call {
                name: name.clone(),
                args,
            }),

        super::Expr::If {
            condition,
            then_block,
            else_block,
            span,
        } => convert_expr_key(context, *condition, span).and_then(|condition| {
            convert_expr_key(context, *then_block, span).and_then(|then_expr| {
                convert_expr_key(context, *else_block, span).map(|else_expr| Expression::If {
                    condition: Box::new(condition),
                    then_expr: Box::new(then_expr),
                    else_expr: Box::new(else_expr),
                })
            })
        }),

        // These expression variants should all be optimised away before reaching final
        // compilation from IntermediateIntent to Intent.
        super::Expr::Error(..)
        | super::Expr::Array { .. }
        | super::Expr::ArrayElementAccess { .. }
        | super::Expr::Tuple { .. }
        | super::Expr::TupleFieldAccess { .. }
        | super::Expr::Cast { .. }
        | super::Expr::In { .. }
        | super::Expr::Range { .. } => Err(CompileError::Internal {
            msg: "Found unsupported expressions in final Intent.",
            span: span.clone(),
        }),
    }
}

fn convert_type(ty: &Type, span: &Span) -> super::Result<intent::Type> {
    use crate::types::PrimitiveKind::*;
    match ty {
        Type::Primitive { kind: Bool, .. } => Ok(intent::Type::Bool),
        Type::Primitive { kind: Int, .. } => Ok(intent::Type::Int),
        Type::Primitive { kind: Real, .. } => Ok(intent::Type::Real),
        Type::Primitive { kind: String, .. } => Ok(intent::Type::String),

        Type::Error(..) | Type::Array { .. } | Type::Tuple { .. } | Type::CustomType { .. } => {
            Err(CompileError::Internal {
                msg: "Found unsupported types in final Intent.",
                span: span.clone(),
            })
        }
    }
}
