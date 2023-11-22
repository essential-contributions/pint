use crate::{
    error::CompileError,
    intent::{self, Expression, Intent, Solve},
    span::Span,
};

use super::{Expr, ExprKey, IntermediateIntent, SolveFunc, State, Type, Var};

pub(super) fn compile(context: IntermediateIntent) -> super::Result<Intent> {
    let IntermediateIntent {
        states,
        vars,
        constraints,
        directives,
        exprs,
        ..
    } = context;

    // Perform all the verification, checks and optimisations.
    // ... TODO ...

    Ok(Intent {
        states: convert_states(&exprs, states)?,
        vars: convert_vars(vars)?,
        constraints: convert_constraints(constraints)?,
        directive: convert_directive(directives)?,
    })
}

fn convert_states(
    exprs: &slotmap::SlotMap<ExprKey, Expr>,
    states: Vec<State>,
) -> super::Result<Vec<intent::State>> {
    states
        .into_iter()
        .map(
            |State {
                 name,
                 ty,
                 expr: expr_key,
                 span,
             }| {
                ty.ok_or_else(|| CompileError::Internal {
                    msg: "Found untyped state.",
                    span: span.clone(),
                })
                .and_then(|ty| {
                    convert_type(ty, &span).and_then(|ty| {
                        exprs
                            .get(expr_key)
                            .ok_or_else(|| CompileError::Internal {
                                msg: "Unable to resolve expr key.",
                                span: span.clone(),
                            })
                            .and_then(|expr| {
                                convert_expr(expr, &span).map(|expr| intent::State {
                                    name,
                                    ty,
                                    expr,
                                })
                            })
                    })
                })
            },
        )
        .collect()
}

fn convert_vars(
    vars: slotmap::SlotMap<super::VarKey, Var>,
) -> super::Result<Vec<intent::Variable>> {
    vars.into_iter()
        .map(|(_, Var { name, ty, span })| {
            ty.ok_or_else(|| CompileError::Internal {
                msg: "Found untyped variable.",
                span: span.clone(),
            })
            .and_then(|ty| convert_type(ty, &span))
            .map(|ty| intent::Variable { name, ty })
        })
        .collect()
}

fn convert_constraints(_constraints: Vec<(ExprKey, Span)>) -> super::Result<Vec<Expression>> {
    todo!()
    //    constraints
    //        .into_iter()
    //        .map(|(expr, span)| convert_expr(expr, &span))
    //        .collect()
}

fn convert_directive(_directives: Vec<(SolveFunc, Span)>) -> super::Result<Solve> {
    todo!()
    //    let (directive, span) = match directives.into_iter().next() {
    //        Some(tuple) => tuple,
    //        None => {
    //            return Err(CompileError::Internal {
    //                msg: "Missing directive during final compile.",
    //                span: empty_span(),
    //            })
    //        }
    //    };
    //
    //    Ok(match directive {
    //        SolveFunc::Satisfy => Solve::Satisfy,
    //        SolveFunc::Maximize(expr) => Solve::Maximize(convert_expr(expr, &span)?),
    //        SolveFunc::Minimize(expr) => Solve::Minimize(convert_expr(expr, &span)?),
    //    })
}

fn convert_expr(expr: &Expr, span: &Span) -> super::Result<Expression> {
    // Ugh.
    let expr = expr.clone();
    match expr {
        super::Expr::Immediate { value, .. } => Ok(Expression::Immediate(value)),
        super::Expr::PathByName(path, _) => Ok(Expression::Path(path)),
        super::Expr::PathByKey(_, _) => todo!(),
        super::Expr::UnaryOp { .. } => todo!(), //{
        //     convert_expr(*expr, &span).map(|expr| Expression::UnaryOp {
        //         op,
        //         expr: Box::new(expr),
        //     })
        // }
        super::Expr::BinaryOp { .. } => todo!(), // convert_expr(*lhs, span).and_then(|lhs| {
        //            convert_expr(*rhs, span).map(|rhs| Expression::BinaryOp {
        //                op,
        //                lhs: Box::new(lhs),
        //                rhs: Box::new(rhs),
        //            })
        //        }),
        super::Expr::Call { .. } => todo!(), // args
        //            .into_iter()
        //            .map(|arg| convert_expr(arg, span))
        //            .collect::<super::Result<_>>()
        //            .map(|args| Expression::Call { name, args }),
        super::Expr::If { .. } => todo!(), //convert_expr(*condition, span).and_then(|condition| {
        //            convert_expr(*then_block, span).and_then(|then_expr| {
        //                convert_expr(*else_block, span).map(|else_expr| Expression::If {
        //                    condition: Box::new(condition),
        //                    then_expr: Box::new(then_expr),
        //                    else_expr: Box::new(else_expr),
        //                })
        //            })
        //        }),

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

fn convert_type(ty: Type, span: &Span) -> super::Result<intent::Type> {
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
