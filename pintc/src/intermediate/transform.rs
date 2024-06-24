// Useful macro for running a transform repeatedly until it no longer makes modifications.
// Declared at the top here so it's available to the other sub-modules in ::transform.

macro_rules! iterate {
    ($handler: expr, $continue_expr: expr, $msg: literal, $modified: ident) => {
        for loop_check in 0.. {
            if !$continue_expr {
                break;
            }

            $modified = true;

            if loop_check > 10_000 {
                return Err($handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: concat!("infinite loop in ", $msg),
                        span: empty_span(),
                    },
                }));
            }
        }
    };

    ($handler: expr, $continue_expr: expr, $msg: literal) => {
        let mut _modified = false;
        iterate!($handler, $continue_expr, $msg, _modified);
    };
}

mod canonicalize_solve_directive;
mod lower;
mod scalarize;
mod unroll;
mod validate;

use crate::error::{ErrorEmitted, Handler};
use canonicalize_solve_directive::canonicalize_solve_directive;
use lower::{
    lower_aliases, lower_bools, lower_casts, lower_compares_to_nil, lower_enums, lower_ifs,
    lower_imm_accesses, lower_ins, replace_const_refs,
};
use scalarize::scalarize;
use unroll::unroll_generators;
use validate::validate;

impl super::Program {
    pub fn flatten(mut self, handler: &Handler) -> Result<Self, ErrorEmitted> {
        for ii in self.iis.values_mut() {
            // Plug const decls in everywhere so they maybe lowered below.
            replace_const_refs(ii);

            // Transform each if declaration into a collection of constraints We do this early so
            // that we don't have to worry about `if` declarations in any of the later passes. All
            // other passes are safe to assume that `if` declarations and their content have
            // already been converted to raw constraints.
            lower_ifs(ii);

            let _ = lower_compares_to_nil(handler, ii);

            // Unroll each generator into one large conjuction
            let _ = handler.scope(|handler| unroll_generators(handler, ii));

            // Lower `in` expressions into more explicit comparisons.
            let _ = lower_ins(handler, ii);

            // Scalarize after lowering enums so we only have to deal with integer indices and
            // then lower array and tuple accesses into immediates.  After here there will no
            // longer be aggregate types.
            //
            // EXCEPT WE'RE NOW LOWERING ENUMS NEXT.  SCALARISING WILL BE REMOVED SOON.
            let _ = scalarize(handler, ii);

            // Transform each enum variant into its integer discriminant
            let _ = lower_enums(handler, ii);

            let _ = lower_imm_accesses(handler, ii);

            // Lower bools after scalarization since it creates new comparison expressions
            // which will return bools.
            lower_bools(ii);

            // This could be done straight after type checking but any error which prints the
            // type until now will have the more informative aliased description.  e.g.,
            // `Height (int)` rather than just `int`.
            lower_aliases(ii);

            // Lower casts after aliases since we're leaving `int -> real` behind, but it's
            // much easier if the `real` isn't still an alias.
            let _ = lower_casts(handler, ii);
        }

        // Transform the objective function, if present, into a path to a new variable that is
        // equal to the objective function expression.
        let _ = canonicalize_solve_directive(handler, &mut self);

        // Ensure that the final intermediate intents is indeed final
        if !handler.has_errors() {
            let _ = validate(handler, &mut self);
        }

        if handler.has_errors() {
            return Err(handler.cancel());
        }

        Ok(self)
    }
}
