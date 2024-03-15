use crate::{
    error::CompileError,
    expr::{Expr, Ident},
    intermediate::{
        IntermediateIntent,
        SolveFunc::{self, *},
    },
    span::empty_span,
};

pub(crate) fn canonicalize(ii: &mut IntermediateIntent) -> Result<(), CompileError> {
    canonicalize_directives(ii)?;

    Ok(())
}

// TODO: add proper documentation

// This is a simple transformation as follows:
// solve maximize <expr>;
// becomes:
// let __objective: <type_of_expr>;
// constraint __objective == <expr>;
// solve maximize __objective;
fn canonicalize_directives(ii: &mut IntermediateIntent) -> Result<(), CompileError> {
    // get the directive
    let directive = ii
        .directives
        .first()
        .ok_or_else(|| CompileError::MissingSolveDirective {
            span: empty_span(), // @mohammad what do we do if there is no span? -- placeholder for now
        })?
        .clone();

    let (solve_func, span) = &directive;
    let expr_key = match solve_func {
        Satisfy => return Ok(()),
        Minimize(expr_key) | Maximize(expr_key) => expr_key,
    };

    let expr_type = ii
        .expr_types
        .get(*expr_key)
        .ok_or_else(|| CompileError::Internal {
            msg: "invalid intermediate intent expression_types slotmap key",
            span: empty_span(), // @mohammad what do we do if there is no span? -- placeholder for now
        })?
        .clone();

    let ident = Ident {
        name: "__objective".to_string(),
        span: span.clone(),
    };
    let expr_type_clone = expr_type.clone();
    let var_key = ii
        .insert_var("", None, &ident, Some(expr_type.clone()))
        .expect("todo: handle parse error as compile error");

    let new_expr_key = ii.exprs.insert(Expr::PathByKey(var_key, span.clone()));
    let _ = ii.expr_types.insert(new_expr_key, expr_type_clone);

    ii.insert_eq_or_ineq_constraint(var_key, *expr_key, span.clone());

    let updated_solve_func = match solve_func {
        Satisfy => return Ok(()),
        Minimize(_) => SolveFunc::Minimize(new_expr_key),
        Maximize(_) => SolveFunc::Maximize(new_expr_key),
    };
    let updated_directive = (updated_solve_func, span.clone());
    ii.directives[0] = updated_directive;

    Ok(())
}
