use crate::{
    error::CompileError,
    intent::{self, Expression, Intent, Solve},
    span::{empty_span, Span},
};

use super::{Expr, IntermediateIntent, State, Type, Var};

pub(super) fn compile(context: IntermediateIntent) -> super::Result<Intent> {
    let IntermediateIntent {
        states,
        vars,
        constraints,
        directives,
        ..
    } = context;

    // Perform all the verification, checks and optimisations.
    // ... TODO ...

    Ok(Intent {
        states: convert_states(states)?,
        vars: convert_vars(vars)?,
        constraints: convert_constraints(constraints)?,
        directive: convert_directive(directives)?,
    })
}

fn convert_states(states: Vec<(State, Span)>) -> super::Result<Vec<intent::State>> {
    states
        .into_iter()
        .map(|(State { name, ty, expr }, span)| {
            ty.ok_or_else(|| CompileError::Internal {
                span: span.clone(),
                msg: "Found untyped variable in final state.",
            })
            .and_then(|ty| {
                convert_type(ty, &span).and_then(|ty| {
                    convert_expr(expr, &span).map(|expr| intent::State { name, ty, expr })
                })
            })
        })
        .collect()
}

fn convert_vars(vars: Vec<(Var, Span)>) -> super::Result<Vec<intent::Variable>> {
    vars.into_iter()
        .map(|(Var { name, ty }, span)| {
            ty.ok_or_else(|| CompileError::Internal {
                span: span.clone(),
                msg: "Found untyped variable in final variable.",
            })
            .and_then(|ty| convert_type(ty, &span))
            .map(|ty| intent::Variable { name, ty })
        })
        .collect()
}

fn convert_constraints(constraints: Vec<(Expr, Span)>) -> super::Result<Vec<Expression>> {
    constraints
        .into_iter()
        .map(|(expr, span)| convert_expr(expr, &span))
        .collect()
}

fn convert_directive(directives: Vec<(Solve, Span)>) -> super::Result<Solve> {
    directives
        .into_iter()
        .next()
        .map(|(s, _)| s)
        .ok_or_else(|| CompileError::Internal {
            span: empty_span(),
            msg: "Missing directive during final compile.",
        })
}

fn convert_expr(expr: Expr, span: &Span) -> super::Result<Expression> {
    match expr {
        super::Expr::Immediate { value, .. } => Ok(Expression::Immediate(value)),
        super::Expr::Path(path) => Ok(Expression::Path(path)),
        super::Expr::UnaryOp { op, expr, span } => {
            convert_expr(*expr, &span).map(|expr| Expression::UnaryOp {
                op,
                expr: Box::new(expr),
            })
        }
        super::Expr::BinaryOp { op, lhs, rhs, .. } => convert_expr(*lhs, span).and_then(|lhs| {
            convert_expr(*rhs, span).map(|rhs| Expression::BinaryOp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            })
        }),
        super::Expr::Call { name, args, .. } => args
            .into_iter()
            .map(|arg| convert_expr(arg, span))
            .collect::<super::Result<_>>()
            .map(|args| Expression::Call { name, args }),
        super::Expr::If {
            condition,
            then_block,
            else_block,
            ..
        } => convert_expr(*condition, span).and_then(|condition| {
            convert_expr(*then_block.0, span).and_then(|then_expr| {
                convert_expr(*else_block.0, span).map(|else_expr| Expression::If {
                    condition: Box::new(condition),
                    then_expr: Box::new(then_expr),
                    else_expr: Box::new(else_expr),
                })
            })
        }),

        // These expression variants should all be optimised away before reaching final
        // compilation from IntermediateIntent to Intent.
        super::Expr::Block(_)
        | super::Expr::Cond { .. }
        | super::Expr::Array { .. }
        | super::Expr::ArrayElementAccess { .. }
        | super::Expr::Tuple { .. }
        | super::Expr::TupleFieldAccess { .. }
        | super::Expr::Cast { .. }
        | super::Expr::In { .. } => Err(CompileError::Internal {
            span: span.clone(),
            msg: "Found unsupported expressions in final Intent.",
        }),
    }
}

fn convert_type(ty: Type, span: &Span) -> super::Result<intent::Type> {
    use crate::types::PrimitiveKind::*;
    match ty {
        Type::Primitive { kind: Bool, .. } => Ok(intent::Type::Bool),
        Type::Primitive { kind: Int, .. } => Ok(intent::Type::Int),
        Type::Primitive { kind: Real, .. } => Ok(intent::Type::Real),
        Type::Primitive { kind: String, .. } => Ok(intent::Type::String),

        Type::Array { .. } | Type::Tuple { .. } | Type::CustomType { .. } => {
            Err(CompileError::Internal {
                span: span.clone(),
                msg: "Found unsupported types in final Intent.",
            })
        }
    }
}
