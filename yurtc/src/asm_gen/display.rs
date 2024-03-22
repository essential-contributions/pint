use super::Intents;
use crate::intermediate::ProgramKind;
use constraint_asm::Op;
use state_asm::StateReadOp;
use std::fmt::{Display, Formatter};

impl Display for Intents {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self.kind {
            ProgramKind::Stateless => fmt_intent_with_indent(self.root_intent(), f, 0)?,
            ProgramKind::Stateful => {
                for (name, intent) in &self.intents {
                    if name == Self::ROOT_INTENT_NAME {
                        fmt_intent_with_indent(intent, f, 0)?;
                    } else {
                        writeln!(f, "intent {name} {{")?;
                        fmt_intent_with_indent(intent, f, 1)?;
                        writeln!(f, "}}\n")?;
                    }
                }
            }
        }

        Ok(())
    }
}

/// Given an `Intent`, print the contained assembly. This prints both the constraints assembly as
/// well as the state reads assembly.
pub fn fmt_intent_with_indent(
    intent: &essential_types::intent::Intent,
    f: &mut Formatter,
    indent: usize,
) -> std::fmt::Result {
    let indent = " ".repeat(4 * indent);
    writeln!(f, "{}--- Constraints ---", indent)?;
    for (idx, constraint) in intent.constraints.iter().enumerate() {
        let ops: Vec<Op> = serde_json::from_str(&String::from_utf8_lossy(constraint)).unwrap();
        writeln!(f, "{}constraint {idx}", indent)?;
        for op in ops {
            writeln!(f, "{}  {:?}", indent, op)?;
        }
    }
    writeln!(f, "{}--- State Reads ---", indent)?;
    for (idx, state_read) in intent.state_read.iter().enumerate() {
        let ops: Vec<StateReadOp> =
            serde_json::from_str(&String::from_utf8_lossy(state_read)).unwrap();
        writeln!(f, "{}state read {idx}", indent)?;
        for op in ops {
            writeln!(f, "{}  {:?}", indent, op)?;
        }
    }

    Ok(())
}
