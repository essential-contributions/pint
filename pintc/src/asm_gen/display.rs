use super::CompiledContract;
use essential_asm::Op;
use essential_types::predicate::Predicate as CompiledPredicate;
use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
};

/// TODO: improve what we print here.. maybe include the edges and what constraints/variables
/// correspond to what compute nodes
impl Display for CompiledContract {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        for (name, compiled_predicate) in self.names.iter().zip(self.contract.predicates.iter()) {
            writeln!(f, "predicate {name} {{")?;
            fmt_compiled_predicate_with_indent(self, compiled_predicate, f, 1)?;
            writeln!(f, "}}\n")?;
        }

        Ok(())
    }
}

/// Given a `CompiledPredicate`, print the contained assembly. This prints both the constraints assembly as
/// well as the state reads assembly.
pub fn fmt_compiled_predicate_with_indent(
    contract: &CompiledContract,
    compiled_predicate: &CompiledPredicate,
    f: &mut Formatter,
    indent: usize,
) -> std::fmt::Result {
    let programs = contract
        .programs
        .iter()
        .map(|program| (essential_hash::content_addr(program), program))
        .collect::<HashMap<_, _>>();

    let indent = " ".repeat(4 * indent);
    writeln!(f, "{}--- Nodes ---", indent)?;
    for (idx, node) in compiled_predicate.nodes.iter().enumerate() {
        let ops: Vec<Op> =
            essential_asm::from_bytes(programs.get(&node.program_address).unwrap().0.clone())
                .collect::<Result<_, _>>()
                .unwrap();
        let is_leaf = node.edge_start == essential_types::predicate::Edge::MAX;
        writeln!(
            f,
            "{}node {idx} ({})",
            indent,
            if is_leaf { ",leaf" } else { "" }
        )?;

        // Uncommenting this will print the nodes which depend on this node.  Can be quite useful
        // for debugging, but I'm (Toby) not uncommenting it at this stage as it adds more noise to
        // the tests.  It also is less readable -- a more useful version might be to pre-cache and
        // then print which nodes this node depends on.  A reversal of the relationship.
        //
        //writeln!(
        //    f,
        //    "{}dependants {}",
        //    indent,
        //    compiled_predicate
        //        .node_edges(idx)
        //        .unwrap_or(&[])
        //        .iter()
        //        .map(|i| format!("{i}"))
        //        .collect::<Vec<_>>()
        //        .join(" ")
        //)?;

        for op in ops {
            writeln!(f, "{}  {:?}", indent, op)?;
        }
    }

    Ok(())
}
