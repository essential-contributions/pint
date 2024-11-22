use super::CompiledContract;
use essential_asm::Op;
use essential_types::predicate::Predicate as CompiledPredicate;
use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
};

impl Display for CompiledContract {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        for ((name, c_nodes, v_nodes), compiled_predicate) in self
            .predicate_metadata
            .iter()
            .zip(self.contract.predicates.iter())
        {
            writeln!(f, "predicate {name} {{")?;
            fmt_compiled_predicate_with_indent(self, compiled_predicate, f, 1, c_nodes, v_nodes)?;
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
    c_nodes: &[usize],
    v_nodes: &[usize],
) -> std::fmt::Result {
    let programs = contract
        .programs
        .iter()
        .map(|program| (essential_hash::content_addr(program), program))
        .collect::<HashMap<_, _>>();

    let indent = " ".repeat(4 * indent);
    writeln!(f, "{}--- Constraints ---", indent)?;
    for (idx, node) in c_nodes.iter().enumerate() {
        let ops: Vec<Op> = essential_asm::from_bytes(
            programs
                .get(&compiled_predicate.nodes[*node].program_address)
                .unwrap()
                .0
                .clone(),
        )
        .collect::<Result<_, _>>()
        .unwrap();
        writeln!(f, "{}constraint {idx}", indent)?;
        for op in ops {
            writeln!(f, "{}  {:?}", indent, op)?;
        }
    }
    writeln!(f, "{}--- State Reads ---", indent)?;
    for (idx, node) in v_nodes.iter().enumerate() {
        let ops: Vec<Op> = essential_asm::from_bytes(
            programs
                .get(&compiled_predicate.nodes[*node].program_address)
                .unwrap()
                .0
                .clone(),
        )
        .collect::<Result<_, _>>()
        .unwrap();
        writeln!(f, "{}state read {idx}", indent)?;
        for op in ops {
            writeln!(f, "{}  {:?}", indent, op)?;
        }
    }

    Ok(())
}
