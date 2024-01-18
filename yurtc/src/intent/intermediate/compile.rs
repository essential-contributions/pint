use super::{Expr, ExprKey, IntermediateIntent, SolveFunc, State, Type, Var};
use crate::{
    error::CompileError,
    intent::{
        self,
        intermediate::transform::{scalarize, unroll_foralls},
        Expression, Intent, SolveDirective,
    },
    span::{empty_span, Span},
};

/// Converts an `IntermediateIntent` to a flattened `IntermediateIntent`. This means that all the
/// syntactic sugar of Yurt (such as enums, foralls, etc.) should be resolved into primitive
/// elements in this function.
pub(super) fn flatten(mut context: IntermediateIntent) -> super::Result<IntermediateIntent> {
    // Transformations
    unroll_foralls(&mut context)?;
    scalarize(&mut context)?;

    Ok(context)
}

/// Converts a final flattened `IntermediateIntent` into a final `Intent`
pub(super) fn compile(context: &IntermediateIntent) -> super::Result<Intent> {
    Ok(Intent {
        states: convert_states(context)?,
        vars: convert_vars(context)?,
        constraints: convert_constraints(context)?,
        directive: convert_directive(context)?,
    })
}

fn convert_states(context: &IntermediateIntent) -> super::Result<Vec<intent::StateVar>> {
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
                            convert_expr_key(context, *expr_key, span).map(|expr| {
                                intent::StateVar {
                                    name: name.clone(),
                                    ty,
                                    expr,
                                }
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

fn convert_directive(context: &IntermediateIntent) -> super::Result<SolveDirective> {
    let Some((directive, span)) = context.directives.first() else {
        return Err(CompileError::Internal {
            msg: "Missing directive during final compile.",
            span: empty_span(),
        });
    };

    match directive {
        SolveFunc::Satisfy => Ok(SolveDirective::Satisfy),
        SolveFunc::Minimize(expr_key) => match convert_expr_key(context, *expr_key, span)? {
            Expression::Path(path) => Ok(SolveDirective::Minimize(path.clone())),
            _ => Err(CompileError::Internal {
                msg: "Objective function is not a path. This is unexpected at this stage.",
                span: empty_span(),
            }),
        },
        SolveFunc::Maximize(expr_key) => match convert_expr_key(context, *expr_key, span)? {
            Expression::Path(path) => Ok(SolveDirective::Maximize(path.clone())),
            _ => Err(CompileError::Internal {
                msg: "Objective function is not a path. This is unexpected at this stage.",
                span: empty_span(),
            }),
        },
    }
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

        super::Expr::FnCall { name, args, span } => args
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
        super::Expr::Array { .. }
        | super::Expr::ArrayElementAccess { .. }
        | super::Expr::Cast { .. }
        | super::Expr::Error(..)
        | super::Expr::In { .. }
        | super::Expr::MacroCall { .. }
        | super::Expr::Range { .. }
        | super::Expr::ForAll { .. }
        | super::Expr::Tuple { .. }
        | super::Expr::TupleFieldAccess { .. } => Err(CompileError::Internal {
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

        Type::Error(..) | Type::Array { .. } | Type::Tuple { .. } | Type::Custom { .. } => {
            Err(CompileError::Internal {
                msg: "Found unsupported types in final Intent.",
                span: span.clone(),
            })
        }
    }
}
