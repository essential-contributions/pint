mod lower;
mod unroll;
mod validate;

use crate::{
    error::{ErrorEmitted, Handler},
    predicate::Const,
};
use lower::{
    coalesce_prime_ops, lower_aliases, lower_bools, lower_casts, lower_compares_to_nil,
    lower_enums, lower_ifs, lower_imm_accesses, lower_ins, replace_const_refs,
};
use unroll::unroll_generators;
use validate::validate;

impl super::Contract {
    pub fn flatten(mut self, handler: &Handler) -> Result<Self, ErrorEmitted> {
        // Copy the const expressions themselves out of the root Pred.  This is to avoid the need to
        // borrow them below for replace_const_refs().
        let const_exprs = self
            .consts
            .iter()
            .map(|(path, Const { expr, decl_ty })| {
                (
                    path.clone(),
                    *expr,
                    expr.get(self.root_pred()).clone(),
                    decl_ty.clone(),
                )
            })
            .collect::<Vec<_>>();

        for pred in self.preds.values_mut() {
            // Transform each if declaration into a collection of constraints. We do this early so
            // that we don't have to worry about `if` declarations in any of the later passes. All
            // other passes are safe to assume that `if` declarations and their content have
            // already been converted to raw constraints.
            lower_ifs(pred);

            // Plug const decls in everywhere so they maybe lowered below.
            replace_const_refs(pred, &const_exprs);

            // Convert comparisons to `nil` into comparisons between __state_len() and 0.
            let _ = lower_compares_to_nil(handler, pred);

            // Unroll each generator into one large conjuction
            let _ = handler.scope(|handler| unroll_generators(handler, pred));

            // Lower `in` expressions into more explicit comparisons.
            let _ = lower_ins(handler, pred);

            // Do some array checks now that generators have been unrolled (and ephemerals aren't
            // going to cause problems).
            pred.check_array_lengths(handler);
            pred.check_array_indexing(handler);
            pred.check_array_compares(handler);

            // Transform each enum variant into its integer discriminant
            let _ = lower_enums(handler, pred);

            // Lower indexing or field access into immediates to the actual element or field.
            let _ = lower_imm_accesses(handler, pred);

            // Coalesce all prime ops back down to the lowest path expression.
            coalesce_prime_ops(pred);

            // Lower bools after scalarization since it creates new comparison expressions
            // which will return bools.
            lower_bools(pred);

            // This could be done straight after type checking but any error which prints the
            // type until now will have the more informative aliased description.  e.g.,
            // `Height (int)` rather than just `int`.
            lower_aliases(pred);

            // Lower casts after aliases since we're leaving `int -> real` behind, but it's
            // much easier if the `real` isn't still an alias.
            let _ = lower_casts(handler, pred);
        }

        // Ensure that the final contract is indeed final
        if !handler.has_errors() {
            let _ = validate(handler, &mut self);
        }

        if handler.has_errors() {
            return Err(handler.cancel());
        }

        Ok(self)
    }
}
