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
        // Transform each `if` declaration into a collection of constraints.
        lower_ifs(&mut self);
        if handler.has_errors() {
            println!("Error after lower_ifs");
            return Err(handler.cancel());
        }

        // Plug const decls in everywhere so they maybe lowered below.
        replace_const_refs(&mut self);
        if handler.has_errors() {
            println!("Error after replace_const_refs");
            return Err(handler.cancel());
        }

        // Convert comparisons to `nil` into comparisons between __state_len() and 0.
        lower_compares_to_nil(&mut self);
        if handler.has_errors() {
            println!("Error after lower_compares_to_nil");
            return Err(handler.cancel());
        }

        // Unroll each generator into one large conjunction
        let _ = handler.scope(|handler| unroll_generators(handler, &mut self));
        if handler.has_errors() {
            println!("Error after unroll_generators");
            return Err(handler.cancel());
        }

        // Lower `in` expressions into more explicit comparisons.
        let _ = lower_ins(handler, &mut self);
        if handler.has_errors() {
            println!("Error after lower_ins");
            return Err(handler.cancel());
        }

        // Do some array checks now that generators have been unrolled
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
        if handler.has_errors() {
            println!("Error during array checks");
            return Err(handler.cancel());
        }

        // Transform each enum variant into its integer discriminant
        let _ = lower_enums(handler, &mut self);
        if handler.has_errors() {
            println!("Error after lower_enums");
            return Err(handler.cancel());
        }

        // Lower array types to have simple integer ranges.
        if !array_check_failed {
            let _ = lower_array_ranges(handler, &mut self);
            if handler.has_errors() {
                println!("Error after lower_array_ranges");
                return Err(handler.cancel());
            }
        }

        // Lower indexing or field access into immediates to the actual element or field.
        let _ = lower_imm_accesses(handler, &mut self);
        if handler.has_errors() {
            println!("Error after lower_imm_accesses");
            return Err(handler.cancel());
        }

        // Coalesce all prime ops back down to the lowest path expression.
        coalesce_prime_ops(&mut self);
        if handler.has_errors() {
            println!("Error after coalesce_prime_ops");
            return Err(handler.cancel());
        }

        // Lower aliases
        lower_aliases(&mut self);
        if handler.has_errors() {
            println!("Error after lower_aliases");
            return Err(handler.cancel());
        }

        // Lower casts after aliases
        let _ = lower_casts(handler, &mut self);
        if handler.has_errors() {
            println!("Error after lower_casts");
            return Err(handler.cancel());
        }

        // Insert OOB checks for storage vector accesses
        let _ = legalize_vector_accesses(handler, &mut self);
        if handler.has_errors() {
            println!("Error after legalize_vector_accesses");
            return Err(handler.cancel());
        }

        // Lower all storage accesses
        let _ = lower_storage_accesses(handler, &mut self);
        if handler.has_errors() {
            println!("Error after lower_storage_accesses");
            return Err(handler.cancel());
        }

        // Lower accesses to pub vars
        let _ = handler.scope(|handler| lower_pub_var_accesses(handler, &mut self));
        if handler.has_errors() {
            println!("Error after lower_pub_var_accesses");
            return Err(handler.cancel());
        }

        // Ensure that the final contract is indeed final
        if !handler.has_errors() {
            // let _ = validate(handler, &mut self);
            if handler.has_errors() {
                println!("Error after validate");
                return Err(handler.cancel());
            }
        }

        if handler.has_errors() {
            println!("Error during flatten");
            return Err(handler.cancel());
        }

        Ok(self)
    }
}
