use crate::{
    error::{ErrorEmitted, Handler},
    //expr::{Expr, ExternalIntrinsic, Immediate, IntrinsicKind},
    predicate, //::{ConstraintDecl, Contract, ExprKey, Predicate, Variable},
               //span::{empty_span, Span, Spanned},
};

pub fn compile_contract(
    handler: &Handler,
    contract: &predicate::Contract,
) -> Result<Contract, ErrorEmitted> {
    let preds = contract
        .preds
        .iter()
        .map(|(key, pred)| compile_pred(handler, contract, pred, key))
        .collect::<Result<_, _>>()?;

    Ok(Contract { preds })
}

pub struct Contract {
    preds: Vec<Predicate>,
}

fn compile_pred(
    handler: &Handler,
    contract: &predicate::Contract,
    predicate: &predicate::Predicate,
    pred_key: predicate::PredKey,
) -> Result<Predicate, ErrorEmitted> {

    todo!()
}

pub struct Predicate;
