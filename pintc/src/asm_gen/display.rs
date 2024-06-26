use super::CompiledProgram;
use crate::predicate::ProgramKind;
use essential_types::intent::Intent as CompiledPredicate;
use state_asm::{Constraint, Op as StateRead};
use std::fmt::{Display, Formatter};

impl Display for CompiledProgram {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self.kind {
            ProgramKind::Stateless => {
                fmt_compiled_predicate_with_indent(self.root_predicate(), f, 0)?
            }
            ProgramKind::Stateful => {
                for (name, compiled_predicate) in self.names.iter().zip(self.predicates.iter()) {
                    if name == Self::ROOT_PRED_NAME {
                        fmt_compiled_predicate_with_indent(compiled_predicate, f, 0)?;
                    } else {
                        writeln!(f, "predicate {name} {{")?;
                        fmt_compiled_predicate_with_indent(compiled_predicate, f, 1)?;
                        writeln!(f, "}}\n")?;
                    }
                }
            }
        }

        Ok(())
    }
}

/// Given a `CompiledPredicate`, print the contained assembly. This prints both the constraints assembly as
/// well as the state reads assembly.
pub fn fmt_compiled_predicate_with_indent(
    compiled_predicate: &CompiledPredicate,
    f: &mut Formatter,
    indent: usize,
) -> std::fmt::Result {
    let indent = " ".repeat(4 * indent);
    writeln!(f, "{}--- Constraints ---", indent)?;
    for (idx, constraint) in compiled_predicate.constraints.iter().enumerate() {
        let ops: Vec<Constraint> = constraint_asm::from_bytes(constraint.iter().copied())
            .collect::<Result<_, _>>()
            .unwrap();
        writeln!(f, "{}constraint {idx}", indent)?;
        for op in ops {
            writeln!(f, "{}  {:?}", indent, op)?;
        }
    }
    writeln!(f, "{}--- State Reads ---", indent)?;
    for (idx, state_read) in compiled_predicate.state_read.iter().enumerate() {
        let ops: Vec<StateRead> = state_asm::from_bytes(state_read.iter().copied())
            .collect::<Result<_, _>>()
            .unwrap();
        writeln!(f, "{}state read {idx}", indent)?;
        for op in ops {
            writeln!(f, "{}  {:?}", indent, op)?;
        }
    }

    Ok(())
}
