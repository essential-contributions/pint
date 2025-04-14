use super::*;

use std::fmt::{Display, Formatter};

impl Display for Contract {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (pred_idx, pred) in self.preds.iter().enumerate() {
            writeln!(f, "predicate {pred_idx}")?;
            display_pred(f, pred)?;
        }

        Ok(())
    }
}

fn display_pred(f: &mut Formatter<'_>, pred: &Predicate) -> std::fmt::Result {
    let mut numberer = Numberer::default();

    for (idx, node_key) in pred.constraints.iter().enumerate() {
        writeln!(f, "  constraint {idx}:")?;
        display_node(f, &pred.nodes, &mut numberer, node_key)?;
    }

    Ok(())
}

fn display_node(
    f: &mut Formatter<'_>,
    nodes: &slotmap::SlotMap<NodeKey, Node>,
    numberer: &mut Numberer,
    node_key: &NodeKey,
) -> Result<i64, std::fmt::Error> {
    let node = &nodes[*node_key];

    let node_num = match &node.value {
        Value::Data(items) => {
            let data_num = numberer.next(*node_key);

            write!(f, "    ${data_num} = DATA")?;
            for item in items {
                write!(f, " {item}")?;
            }

            data_num
        }

        Value::Operation(op) => {
            let op_num = numberer.next(*node_key);

            write!(f, "    ${op_num} = ASM {}", op_to_str(op))?;

            op_num
        }

        Value::OperationBlock(ops) => todo!(),

        Value::Application { op, args } => {
            let args_nums = args
                .iter()
                .map(|arg_node_key| display_node(f, nodes, numberer, arg_node_key))
                .collect::<Result<Vec<_>, _>>()?;

            let op_num = display_node(f, nodes, numberer, op)?;
            let app_num = numberer.next(*node_key);

            write!(f, "    ${app_num} = EXEC ${op_num}")?;
            for arg_num in args_nums {
                write!(f, " ${arg_num}")?;
            }

            app_num
        }

        Value::Ptr(idx) => {
            let ptr_num = numberer.next(*node_key);
            write!(f, "    ${ptr_num} = PTR +{idx}")?;

            ptr_num
        }

        Value::Gep { ptr, offs } => todo!(),
        Value::MemCpy(node_key) => todo!(),

        Value::MemMove(move_key) => {
            let node_num = display_node(f, nodes, numberer, move_key)?;
            let move_num = numberer.next(*node_key);

            write!(f, "    ${move_num} = MOVE ${node_num}")?;

            move_num
        }

        Value::If {
            pred,
            then_val,
            else_val,
        } => todo!(),
        Value::Loop { count, value } => todo!(),
    };

    writeln!(f, "  [{} / {}]", node.size, node.loc)?;

    Ok(node_num)
}

#[derive(Default)]
struct Numberer {
    node_map: fxhash::FxHashMap<NodeKey, i64>,
    next_num: i64,
}

impl Numberer {
    fn next(&mut self, node_key: NodeKey) -> i64 {
        *self.node_map.entry(node_key).or_insert_with(|| {
            let n = self.next_num;
            self.next_num += 1;
            n
        })
    }
}

fn op_to_str(op: &essential_asm::Op) -> &str {
    match op {
        AsmOp::Stack(stack) => todo!(),

        AsmOp::Pred(pred) => match pred {
            essential_asm::Pred::Eq => "EQ",
            essential_asm::Pred::EqRange => "EQRA",
            essential_asm::Pred::Gt => "GT",
            essential_asm::Pred::Lt => "LT",
            essential_asm::Pred::Gte => "GTE",
            essential_asm::Pred::Lte => "LTE",
            essential_asm::Pred::And => "AND",
            essential_asm::Pred::Or => "OR",
            essential_asm::Pred::Not => "NOT",
            essential_asm::Pred::EqSet => "EQST",
            essential_asm::Pred::BitAnd => "BAND",
            essential_asm::Pred::BitOr => "BOR",
        },

        AsmOp::Alu(alu) => match alu {
            essential_asm::Alu::Add => "ADD",
            essential_asm::Alu::Sub => "SUB",
            essential_asm::Alu::Mul => "MUL",
            essential_asm::Alu::Div => "DIV",
            essential_asm::Alu::Mod => "MOD",
            essential_asm::Alu::Shl => "SHL",
            essential_asm::Alu::Shr => "SHR",
            essential_asm::Alu::ShrI => "SHRI",
        }

        AsmOp::Access(access) => todo!(),
        AsmOp::Crypto(crypto) => todo!(),
        AsmOp::TotalControlFlow(total_control_flow) => todo!(),
        AsmOp::Memory(memory) => todo!(),
        AsmOp::ParentMemory(parent_memory) => todo!(),
        AsmOp::StateRead(state_read) => todo!(),
        AsmOp::Compute(compute) => todo!(),
    }
}
