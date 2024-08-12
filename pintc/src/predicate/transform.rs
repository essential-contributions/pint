mod legalize;
mod lower;
mod unroll;
mod validate;

use crate::error::{ErrorEmitted, Handler};
use legalize::legalize_vector_accesses;
use lower::{
    coalesce_prime_ops, lower_aliases, lower_array_ranges, lower_casts, lower_compares_to_nil,
    lower_enums, lower_ifs, lower_imm_accesses, lower_ins, lower_pub_var_accesses,
    lower_storage_accesses, replace_const_refs,
};
use unroll::unroll_generators;
use validate::validate;

impl super::Contract {
    pub fn flatten(mut self, handler: &Handler) -> Result<Self, ErrorEmitted> {
        // Transform each `if` declaration into a collection of constraints. We do this early so
        // that we don't have to worry about `if` declarations in any of the later passes. All
        // other passes are safe to assume that `if` declarations and their content have
        // already been converted to raw constraints.
        lower_ifs(&mut self);

        // Plug const decls in everywhere so they maybe lowered below.
        replace_const_refs(&mut self);

        // Convert comparisons to `nil` into comparisons between __state_len() and 0.
        lower_compares_to_nil(&mut self);

        // Unroll each generator into one large conjuction
        let _ = handler.scope(|handler| unroll_generators(handler, &mut self));

        // Lower `in` expressions into more explicit comparisons.
        let _ = lower_ins(handler, &mut self);

        // Do some array checks now that generators have been unrolled (and ephemerals aren't
        // going to cause problems).  We're taking note of whether these fail to avoid superfluous
        // array related errors later.  Once we can move these checks back into the type-checker it
        // shouldn't be necessary.
        let mut array_check_failed = false;
        let _ = handler.scope(|handler| {
            for pred_key in self.preds.keys() {
                self.check_array_lengths(handler, pred_key);
                self.check_array_indexing(handler, pred_key);
                self.check_array_compares(handler, pred_key);
            }

            if handler.has_errors() {
                array_check_failed = true;
                Err(handler.cancel())
            } else {
                Ok(())
            }
        });

        // Transform each enum variant into its integer discriminant
        let _ = lower_enums(handler, &mut self);

        // Lower array types to have simple integer ranges.
        if !array_check_failed {
            let _ = lower_array_ranges(handler, &mut self);
        }

        // Lower indexing or field access into immediates to the actual element or field.
        let _ = lower_imm_accesses(handler, &mut self);

        // Coalesce all prime ops back down to the lowest path expression.
        coalesce_prime_ops(&mut self);

        // This could be done straight after type checking but any error which prints the
        // type until now will have the more informative aliased description.  e.g.,
        // `Height (int)` rather than just `int`.
        lower_aliases(&mut self);

        // Lower casts after aliases since we're leaving `int -> real` behind, but it's
        // much easier if the `real` isn't still an alias.
        let _ = lower_casts(handler, &mut self);

        // Insert OOB checks for storage vector accesses
        let _ = legalize_vector_accesses(handler, &mut self);

        // Lower all storage accesses to __storage_get and __storage_get_extern intrinsics. Also
        // add constraints on mutable keys
        let _ = lower_storage_accesses(handler, &mut self);

        // Lower accesses to pub vars to `__transient` intrinsics. Also insert any relevant
        // constraints on contract and predicate addresses.
        let _ = lower_pub_var_accesses(handler, &mut self);

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
