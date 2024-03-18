mod lower;
mod scalarize;
mod unroll;

use lower::{lower_aliases, lower_bools, lower_casts, lower_enums, lower_imm_accesses};
use scalarize::scalarize;
use unroll::unroll_foralls;

impl super::IntermediateIntent {
    pub fn flatten(mut self) -> super::Result<Self> {
        // Transformations.
        unroll_foralls(&mut self)?;
        lower_enums(&mut self)?;

        // Scalarize after lowering enums so we only have to deal with integer indices and then
        // lower array and tuple accesses into immediates.  After here there will no longer be
        // aggregate types.
        scalarize(&mut self)?;
        lower_imm_accesses(&mut self)?;

        // Lower bools after scalarization since it creates new comparison expressions which will
        // return bools.
        lower_bools(&mut self)?;

        // This could be done straight after type checking but any error which prints the type
        // until now will have the more informative aliased description.  e.g., `Height (int)`
        // rather than just `int`.
        lower_aliases(&mut self)?;

        // Lower casts after aliases since we're leaving `int -> real` behind, but it's much easier
        // if the `real` isn't still an alias.
        lower_casts(&mut self)?;

        Ok(self)
    }
}
