mod canonicalize_solve_directive;
mod lower;
mod scalarize;
mod unroll;

use crate::error::{Error, Errors};
use canonicalize_solve_directive::canonicalize_solve_directive;
use lower::{lower_aliases, lower_bools, lower_casts, lower_enums, lower_imm_accesses};
use scalarize::scalarize;
use unroll::unroll_foralls;

#[macro_export]
macro_rules! transform {
    ($result:expr, $errs:ident) => {
        if let Err(error) = $result {
            $errs.push(Error::Compile { error });
        }
    };
}

impl super::Program {
    pub fn flatten(mut self) -> Result<Self, Errors> {
        let mut errors: Vec<Error> = self
            .iis
            .values_mut()
            .flat_map(|ii| {
                let mut errs = Vec::new();

                // Unroll each forall into one large conjuction
                transform!(unroll_foralls(ii), errs);

                // Transform each enum variant into its integer discriminant
                transform!(lower_enums(ii), errs);

                // Scalarize after lowering enums so we only have to deal with integer indices and
                // then lower array and tuple accesses into immediates.  After here there will no
                // longer be aggregate types.
                transform!(scalarize(ii), errs);
                transform!(lower_imm_accesses(ii), errs);

                // Lower bools after scalarization since it creates new comparison expressions
                // which will return bools.
                transform!(lower_bools(ii), errs);

                // This could be done straight after type checking but any error which prints the
                // type until now will have the more informative aliased description.  e.g.,
                // `Height (int)` rather than just `int`.
                transform!(lower_aliases(ii), errs);

                // Lower casts after aliases since we're leaving `int -> real` behind, but it's
                // much easier if the `real` isn't still an alias.
                transform!(lower_casts(ii), errs);

                errs
            })
            .collect();

        // Transform the objective function, if present, into a path to a new variable that is
        // equal to the objective function expression.
        transform!(canonicalize_solve_directive(&mut self), errors);

        errors
            .is_empty()
            .then(|| Ok(self))
            .unwrap_or_else(|| Err(Errors(errors)))
    }
}
