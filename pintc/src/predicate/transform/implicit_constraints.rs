use crate::{
    expr::{BinaryOp, Immediate},
    predicate::{ConstraintDecl, Contract, Expr, Param, PredKey},
    span::empty_span,
    types,
};

pub(super) fn constrain_params(contract: &mut Contract) {
    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        constrain_params_in_predicate(contract, pred_key);
    }
}

fn constrain_params_in_predicate(contract: &mut Contract, pred_key: PredKey) {
    let mut bool_params = Vec::default();

    // Gather all the constrainable parameter names.
    if let Some(pred) = contract.preds.get(pred_key) {
        for Param { name, ty, .. } in &pred.params {
            if ty.is_bool() {
                bool_params.push(name.name.clone());
            }
        }
    }

    // For each boolean parameter 'p' add a constraint:
    //   constraint p as int == 0 or p as int == 1;
    for bool_param_name in bool_params {
        let param_path_expr_key = contract
            .exprs
            .insert(Expr::Path(bool_param_name, empty_span()), types::r#bool());

        let cast_expr_key = contract.exprs.insert(
            Expr::Cast {
                value: param_path_expr_key,
                ty: types::int(),
                span: empty_span(),
            },
            types::int(),
        );

        let zero_expr_key = contract.exprs.insert(
            Expr::Immediate {
                value: Immediate::Int(0),
                span: empty_span(),
            },
            types::int(),
        );
        let eq_to_zero_expr_key = contract.exprs.insert(
            Expr::BinaryOp {
                op: BinaryOp::Equal,
                lhs: cast_expr_key,
                rhs: zero_expr_key,
                span: empty_span(),
            },
            types::r#bool(),
        );

        let one_expr_key = contract.exprs.insert(
            Expr::Immediate {
                value: Immediate::Int(1),
                span: empty_span(),
            },
            types::int(),
        );
        let eq_to_one_expr_key = contract.exprs.insert(
            Expr::BinaryOp {
                op: BinaryOp::Equal,
                lhs: cast_expr_key,
                rhs: one_expr_key,
                span: empty_span(),
            },
            types::r#bool(),
        );

        let or_expr_key = contract.exprs.insert(
            Expr::BinaryOp {
                op: BinaryOp::LogicalOr,
                lhs: eq_to_zero_expr_key,
                rhs: eq_to_one_expr_key,
                span: empty_span(),
            },
            types::r#bool(),
        );

        // Add this expression to the predicate constraints.
        if let Some(pred) = contract.preds.get_mut(pred_key) {
            pred.constraints.push(ConstraintDecl {
                expr: or_expr_key,
                span: empty_span(),
            });
        }
    }
}
