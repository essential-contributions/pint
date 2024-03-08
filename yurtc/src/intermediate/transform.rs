mod canonicalize;
mod lower;
mod scalarize;
mod unroll;

use canonicalize::canonicalize;
use lower::{lower_aliases, lower_bools, lower_casts, lower_enums};
use scalarize::scalarize;
use unroll::unroll_foralls;

impl super::IntermediateIntent {
    pub fn flatten(mut self) -> super::Result<Self> {
        // Transformations.
        unroll_foralls(&mut self)?;
        scalarize(&mut self)?;
        lower_enums(&mut self)?;
        lower_bools(&mut self)?;

        // This could be done straight after type checking but any error which prints the type
        // until now will have the more informative aliased description.  e.g., `Height (int)`
        // rather than just `int`.
        lower_aliases(&mut self)?;

        // Lower casts after aliases since we're leaving `int -> real` behind, but it's much easier
        // if the `real` isn't still an alias.
        lower_casts(&mut self)?;

        canonicalize(&mut self)?;

        Ok(self)
    }
}
