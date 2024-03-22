use crate::{
    error::CompileError, expr::Expr, intermediate::{IntermediateIntent, Program}, span::{empty_span, Spanned}, types::Type
};

// make sure no foralls
// no arrays
// no type aliases
// check there is a matching expr and expr_type for everything
// check there is a matching var and var_type for everything
// check verification pass has to be the last pass
// ideally all internal compile errors, and not user facing

// @mohammad do we return a vec of errors? Or do we short circuit on the first one?? -- yes. Make sure we have all of the internal errors everywhere

pub(crate) fn sanity_check(program: &mut Program) -> Result<(), CompileError> {
    for ii in program.iis.values() {
        let mut errors = check_expr_types(ii);
        check_exprs(ii, &mut errors);
        check_var_types(ii, &mut errors);
        check_vars(ii, &mut errors);
    }

    Ok(())
}

fn check_expr_types(ii: &IntermediateIntent) -> Vec<CompileError> {
    ii.expr_types.iter().filter_map(|(_, expr_type)| {
        match expr_type {
            Type::Error( span ) => Some(CompileError::Internal {
                msg: "error expression present in final intent expr_types slotmap",
                span: span.clone(),
            }),
            Type::Array { span, .. } => Some(CompileError::Internal {
                msg: "array present in final intent expr_types slotmap",
                span: span.clone(),
            }),
            Type::Tuple { span, ..  } => Some(CompileError::Internal {
                msg: "tuple present in final intent expr_types slotmap",
                span: span.clone(),
            }),
            Type::Custom {span, ..  } => Some(CompileError::Internal {
                msg: "custom type present in final intent expr_types slotmap",
                span: span.clone(),
            }),
            Type::Alias { span, .. } => Some(CompileError::Internal {
                msg: "type alias present in final intent expr_types slotmap",
                span: span.clone(),
            }),
            _ => None
        }
    }).collect::<Vec<CompileError>>()
}

fn check_exprs(ii: &IntermediateIntent, errors: &mut Vec<CompileError>) {
    fn check_expr(expr: &Expr) -> Option<CompileError>{
        match expr {
            Expr::Error(span) => Some(CompileError::Internal {
                msg: "error expression present in final intent exprs slotmap",
                span: span.clone(),
            }),
            Expr::MacroCall { span, .. } => Some(CompileError::Internal {
                msg: "macro call present in final intent exprs slotmap",
                span: span.clone(),
            }),
            Expr::If { span, .. } => Some(CompileError::Internal {
                msg: "if expression present in final intent exprs slotmap",
                span: span.clone(),
            }),
            Expr::Array { span, .. } => Some(CompileError::Internal {
                msg: "array present in final intent exprs slotmap",
                span: span.clone(),
            }),
            Expr::ArrayElementAccess { span, .. } => Some(CompileError::Internal {
                msg: "array element access present in final intent exprs slotmap",
                span: span.clone(),
            }),
            Expr::Tuple { span, .. } => Some(CompileError::Internal {
                msg: "tuple present in final intent exprs slotmap",
                span: span.clone(),
            }),
            Expr::TupleFieldAccess { span, .. } => Some(CompileError::Internal {
                msg: "tuple field access present in final intent exprs slotmap",
                span: span.clone(),
            }),
            Expr::Cast {span, .. } => Some(CompileError::Internal {
                msg: "cast present in final intent exprs slotmap",
                span: span.clone(),
            }),
            Expr::In { span, .. } => Some(CompileError::Internal {
                msg: "in expression in final intent exprs slotmap",
                span: span.clone(),
            }),
            Expr::Range { span, .. } => Some(CompileError::Internal {
                msg: "range present in final intent exprs slotmap",
                span: span.clone(),
            }),
            Expr::ForAll { span, .. } => Some(CompileError::Internal {
                msg: "forall present in final intent exprs slotmap",
                span: span.clone(),
            }),
            _ => None
        }
    }

    if ii.exprs.len() != ii.expr_types.len() {
        errors.push(CompileError::Internal {
            msg: "mismatched final intent exprs and expr_types slotmaps",
            span: empty_span(),
        });
    };

    let mut errors: Vec<CompileError> = Vec::new();
    for (expr_key, expr) in ii.exprs.iter() {
        if let Some(error) = check_expr(expr) {
            errors.push(error);
        }

        if ii.expr_types.get(expr_key).is_none() {
            errors.push(CompileError::Internal { 
                msg: "final intent expr_types slotmap is missing corresponding key from exprs slotmap", 
                span: ii.exprs[expr_key].span().clone() })
        }
    }
}

fn check_vars(ii: &IntermediateIntent, errors: &mut Vec<CompileError>) {
    if ii.vars.len() != ii.var_types.len() {
        errors.push(CompileError::Internal {
            msg: "mismatched final intent vars and var_types slotmaps",
            span: empty_span(),
        });
    };

    let mut errors: Vec<CompileError> = Vec::new();
    for (var_key, _) in ii.vars.iter() {
        if ii.var_types.get(var_key).is_none() {
            errors.push(CompileError::Internal { 
                msg: "final intent var_types slotmap is missing corresponding key from vars slotmap", 
                span: ii.vars[var_key].span.clone() })
        }
    }
}

fn check_var_types(ii: &IntermediateIntent, errors: &mut Vec<CompileError>) {
    errors.append(
        &mut ii.var_types.iter().filter_map(|(_, var_type)| {
        match var_type {
            Type::Error( span ) => Some(CompileError::Internal {
                msg: "error var present in final intent var_types slotmap",
                span: span.clone(),
            }),
            Type::Array { span, .. } => Some(CompileError::Internal {
                msg: "array present in final intent var_types slotmap",
                span: span.clone(),
            }),
            Type::Tuple { span, ..  } => Some(CompileError::Internal {
                msg: "tuple present in final intent var_types slotmap",
                span: span.clone(),
            }),
            Type::Custom {span, ..  } => Some(CompileError::Internal {
                msg: "custom type present in final intent var_types slotmap",
                span: span.clone(),
            }),
            Type::Alias { span, .. } => Some(CompileError::Internal {
                msg: "type alias present in final intent var_types slotmap",
                span: span.clone(),
            }),
            _ => None
        }
    }).collect::<Vec<CompileError>>());
}


// TODO: add unit tests here
// easiest way is to take a string that contains source code
// run the parser on it
// program will come out of it
// then run the checker directly on that