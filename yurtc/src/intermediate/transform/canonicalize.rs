use crate::{error::CompileError, intermediate::IntermediateIntent};

pub(crate) fn canonicalize(ii: &mut IntermediateIntent) -> Result<(), CompileError> {
    canonicalize_directives(ii)?;

    Ok(())
}

// This is a simple transformation as follows:
// solve maximize <expr>;
// becomes:
// let z: <type_of_expr>;
// constraint z == <expr>;
// solve maximize z;
fn canonicalize_directives(ii: &mut IntermediateIntent) -> Result<(), CompileError> {
    // get directive, make sure there is only one
    // compiler error otherwise
    // create expression in ii -- how?
    // remove old expression entirely, remove expr key from solve directive and update key from new directive -- how?
    dbg!(ii);

    Ok(())
}

/*directives: [
    (
        Minimize(
            ExprKey(
                6v1,
            ),
        ),
        "test.yrt":12..32,
    ),
], */
