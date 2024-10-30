use crate::{
    error::{ErrorEmitted, Handler},
    expr::{BinaryOp, Expr, ExternalIntrinsic, IntrinsicKind, UnaryOp},
    predicate::{ConstraintDecl, Contract, ExprKey, PredKey, Variable},
    span::empty_span,
    types::{PrimitiveKind, Type},
};
use fxhash::{FxHashMap, FxHashSet};

/// In a given contract, insert constraints that enforce that all storage vector accesses are
/// within bounds.
pub(crate) fn legalize_vector_accesses(
    handler: &Handler,
    contract: &mut Contract,
) -> Result<(), ErrorEmitted> {
    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        legalize_vector_accesses_in_predicate(handler, contract, pred_key)?;
    }

    Ok(())
}

/// In a given predicate, insert constraints that enforce that all storage vector accesses are
/// within bounds.
///
/// Currently, this only assumes single dimensional storage vectors
pub(crate) fn legalize_vector_accesses_in_predicate(
    _handler: &Handler,
    contract: &mut Contract,
    pred_key: PredKey,
) -> Result<(), ErrorEmitted> {
    let int_ty = Type::Primitive {
        kind: PrimitiveKind::Int,
        span: empty_span(),
    };

    let bool_ty = Type::Primitive {
        kind: PrimitiveKind::Bool,
        span: empty_span(),
    };

    // Collect all variable variables that are initialized to index expressions to storage vector
    let variable_vector_accesses = contract
        .preds
        .get(pred_key)
        .unwrap()
        .variables()
        .filter_map(|(_, variable)| {
            let Variable {
                name: variable_var_name,
                expr: init_expr,
                ..
            } = variable;
            if let Expr::Index {
                expr: storage_vec_expr,
                index,
                ..
            } = init_expr.get(contract)
            {
                if let Expr::LocalStorageAccess {
                    name: storage_vec_name,
                    ..
                } = storage_vec_expr.get(contract)
                {
                    if contract.storage_var(storage_vec_name).1.ty.is_vector() {
                        return Some((
                            variable_var_name.clone(),
                            (storage_vec_name.clone(), *index, *storage_vec_expr),
                        ));
                    }
                }
            }
            None
        })
        .collect::<FxHashMap<String, (String, ExprKey, ExprKey)>>();

    // Collect all `Path` expressions that reference one of the variable variables collected in
    // `variable_vector_accesses` and are "primed", i.e., they are used in a `NextState` unary op
    // expression
    let primed_exprs = contract
        .exprs(pred_key)
        .filter_map(|expr| {
            if let Expr::UnaryOp {
                op: UnaryOp::NextState,
                expr: primed_expr,
                ..
            } = expr.get(contract)
            {
                if let Expr::Path(name, _) = primed_expr.get(contract) {
                    if let Some((storage_vec_name, index, storage_vec_expr)) =
                        variable_vector_accesses.get(name)
                    {
                        return Some((
                            *primed_expr,
                            (storage_vec_name.clone(), *index, *storage_vec_expr),
                        ));
                    }
                }
            }
            None
        })
        .collect::<FxHashMap<ExprKey, (String, ExprKey, ExprKey)>>();

    // Collect all `Path` expression that reference one of the variable variables collected in
    // `variable_vector_accesses` and are *not* "primed".
    //
    // The I'm doing this is probably okay but I think can fail in situations where the `Path`
    // expression key is used in a prime expression AND outside a prime expression.
    let non_primed_exprs = contract
        .exprs(pred_key)
        .filter_map(|expr| {
            if let Expr::Path(name, _) = expr.get(contract) {
                if let Some((storage_vec_name, index, storage_vec_expr)) =
                    variable_vector_accesses.get(name)
                {
                    if !primed_exprs.contains_key(&expr) {
                        return Some((storage_vec_name.clone(), *index, *storage_vec_expr));
                    }
                }
            }
            None
        })
        .collect::<FxHashSet<(String, ExprKey, ExprKey)>>();

    // This is a helper closure that creates a `variable` variable initialized the `__vec_len` of a
    // storage access expression to storage variable named `storage_vec_name` and with type
    // `storage_access_ty`
    let create_vec_len_variable_var =
        |contract: &mut Contract,
         vec_len_name: String,
         storage_vec_name: &String,
         storage_access_ty: &Type| {
            let vector_storage_access = contract.exprs.insert(
                Expr::LocalStorageAccess {
                    name: storage_vec_name.clone(),
                    mutable: false,
                    span: empty_span(),
                },
                storage_access_ty.clone(),
            );
            let intrinsic = contract.exprs.insert(
                Expr::IntrinsicCall {
                    kind: (
                        IntrinsicKind::External(ExternalIntrinsic::VecLen),
                        empty_span(),
                    ),
                    args: vec![vector_storage_access],
                    span: empty_span(),
                },
                int_ty.clone(),
            );
            if let Some(pred) = contract.preds.get_mut(pred_key) {
                pred.variables.insert(
                    Variable {
                        name: vec_len_name,
                        expr: intrinsic,
                        span: empty_span(),
                    },
                    int_ty.clone(),
                );
            }
        };

    // Keep track of all created vector length `variable` variables so that we don't create them
    // again.
    let mut vec_len_variable_var_names = FxHashSet::default();

    // Handle "non" primed vector first
    for (storage_vec_name, index, storage_vec_expr) in &non_primed_exprs {
        let vec_len_variable_var_name = "__".to_owned() + storage_vec_name + "_len";

        // Insert a new variable variable that contains the length of the vector being accessed
        if vec_len_variable_var_names.insert(vec_len_variable_var_name.clone()) {
            let ty = storage_vec_expr.get_ty(contract).clone();
            create_vec_len_variable_var(
                contract,
                vec_len_variable_var_name.clone(),
                storage_vec_name,
                &ty,
            );
        }

        // Now create insert a new constraint that ensures that the index is smaller than the
        // current length of the vector
        let vec_len_path = contract.exprs.insert(
            Expr::Path(vec_len_variable_var_name.clone(), empty_span()),
            int_ty.clone(),
        );

        let index_less_than_vec_len = contract.exprs.insert(
            Expr::BinaryOp {
                op: BinaryOp::LessThan,
                lhs: *index,
                rhs: vec_len_path,
                span: empty_span(),
            },
            bool_ty.clone(),
        );

        if let Some(pred) = contract.preds.get_mut(pred_key) {
            pred.constraints.push(ConstraintDecl {
                expr: index_less_than_vec_len,
                span: empty_span(),
            });
        }
    }

    // Now handle primed vector first
    for (storage_vec_name, index, storage_vec_expr) in primed_exprs.values() {
        let vec_len_variable_var_name = "__".to_owned() + storage_vec_name + "_len";

        // Insert a new variable variable that contains the length of the vector being accessed
        if vec_len_variable_var_names.insert(vec_len_variable_var_name.clone()) {
            let ty = storage_vec_expr.get_ty(contract).clone();
            create_vec_len_variable_var(
                contract,
                vec_len_variable_var_name.clone(),
                storage_vec_name,
                &ty,
            );
        }

        // Now create insert a new constraint that ensures that the index is smaller than the
        // future length of the vector
        let vec_len_path = contract.exprs.insert(
            Expr::Path(vec_len_variable_var_name.clone(), empty_span()),
            int_ty.clone(),
        );

        let vec_len_path_prime = contract.exprs.insert(
            Expr::UnaryOp {
                op: UnaryOp::NextState,
                expr: vec_len_path,
                span: empty_span(),
            },
            int_ty.clone(),
        );

        let index_less_than_vec_len_prime = contract.exprs.insert(
            Expr::BinaryOp {
                op: BinaryOp::LessThan,
                lhs: *index,
                rhs: vec_len_path_prime,
                span: empty_span(),
            },
            bool_ty.clone(),
        );

        if let Some(pred) = contract.preds.get_mut(pred_key) {
            pred.constraints.push(ConstraintDecl {
                expr: index_less_than_vec_len_prime,
                span: empty_span(),
            });
        }
    }

    Ok(())
}
