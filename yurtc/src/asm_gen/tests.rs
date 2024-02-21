use crate::{asm_gen::intent_to_asm, parser::parse_project};
use constraint_asm::*;
use essential_types::slots::*;
use state_asm::*;
use std::io::Write;

#[cfg(test)]
fn check(actual: &str, expect: expect_test::Expect) {
    expect.assert_eq(actual);
}

#[cfg(test)]
fn compile(code: &str) -> essential_types::intent::Intent {
    let mut tmpfile = tempfile::NamedTempFile::new().unwrap();
    write!(tmpfile.as_file_mut(), "{}", code).unwrap();
    let intent = parse_project(tmpfile.path()).unwrap().compile().unwrap();
    intent_to_asm(&intent).unwrap()
}

#[cfg(test)]
fn constraints_ops(intent: &essential_types::intent::Intent) -> String {
    intent
        .constraints
        .iter()
        .map(|constraint| serde_json::from_slice::<Vec<Op>>(constraint).unwrap())
        .enumerate()
        .fold(String::new(), |acc, (idx, constraint)| {
            format!(
                "{acc}constraint {idx}:\n{}",
                constraint
                    .iter()
                    .fold(String::new(), |acc, op| format!("{}    {:?}\n", acc, op))
            )
        })
}

#[cfg(test)]
fn state_read_ops(intent: &essential_types::intent::Intent) -> String {
    intent
        .state_read
        .iter()
        .map(|state_read| serde_json::from_slice::<Vec<StateReadOp>>(state_read).unwrap())
        .enumerate()
        .fold(String::new(), |acc, (idx, state_read)| {
            format!(
                "{acc}state read {idx}:\n{}",
                state_read
                    .iter()
                    .fold(String::new(), |acc, op| format!("{}    {:?}\n", acc, op))
            )
        })
}

#[test]
fn bool_literals() {
    check(
        &constraints_ops(&compile(
            r#"
            constraint true;
            constraint false;
            solve satisfy;
            "#,
        )),
        expect_test::expect![[r#"
            constraint 0:
                Push(1)
            constraint 1:
                Push(0)
        "#]],
    );
}

#[test]
fn int_literals() {
    let intent = compile(
        r#"
        let x: int = 4;
        let y: int = 0x333;
        solve satisfy;
        "#,
    );

    check(
        &constraints_ops(&intent),
        expect_test::expect![[r#"
            constraint 0:
                Push(0)
                Access(DecisionVar)
                Push(4)
                Pred(Eq)
            constraint 1:
                Push(1)
                Access(DecisionVar)
                Push(819)
                Pred(Eq)
        "#]],
    );

    assert_eq!(intent.slots.decision_variables, 2);
    assert!(intent.slots.state.is_empty());
}

#[test]
fn unary_not() {
    check(
        &constraints_ops(&compile(
            r#"
            let t: bool = !true;
            constraint !t;
            solve satisfy;
            "#,
        )),
        expect_test::expect![[r#"
            constraint 0:
                Push(0)
                Access(DecisionVar)
                Push(1)
                Pred(Not)
                Pred(Eq)
            constraint 1:
                Push(0)
                Access(DecisionVar)
                Pred(Not)
        "#]],
    );
}

#[test]
fn binary_ops() {
    check(
        &constraints_ops(&compile(
            r#"
            let x: int; let y: int; let z: int;
            let b0: bool; let b1: bool;
            constraint x + y == z;
            constraint x - y == z;
            constraint x * y == z;
            constraint x / y == z;
            constraint x % y == z;
            constraint x != y;
            constraint x == y;
            constraint x <= y;
            constraint x < y;
            constraint x >= y;
            constraint x > y;
            constraint x > y;
            constraint b0 && b1;
            constraint b0 || b1;
            solve satisfy;
            "#,
        )),
        expect_test::expect![[r#"
            constraint 0:
                Push(0)
                Access(DecisionVar)
                Push(1)
                Access(DecisionVar)
                Alu(Add)
                Push(2)
                Access(DecisionVar)
                Pred(Eq)
            constraint 1:
                Push(0)
                Access(DecisionVar)
                Push(1)
                Access(DecisionVar)
                Alu(Sub)
                Push(2)
                Access(DecisionVar)
                Pred(Eq)
            constraint 2:
                Push(0)
                Access(DecisionVar)
                Push(1)
                Access(DecisionVar)
                Alu(Mul)
                Push(2)
                Access(DecisionVar)
                Pred(Eq)
            constraint 3:
                Push(0)
                Access(DecisionVar)
                Push(1)
                Access(DecisionVar)
                Alu(Div)
                Push(2)
                Access(DecisionVar)
                Pred(Eq)
            constraint 4:
                Push(0)
                Access(DecisionVar)
                Push(1)
                Access(DecisionVar)
                Alu(Mod)
                Push(2)
                Access(DecisionVar)
                Pred(Eq)
            constraint 5:
                Push(0)
                Access(DecisionVar)
                Push(1)
                Access(DecisionVar)
                Pred(Eq)
                Pred(Not)
            constraint 6:
                Push(0)
                Access(DecisionVar)
                Push(1)
                Access(DecisionVar)
                Pred(Eq)
            constraint 7:
                Push(0)
                Access(DecisionVar)
                Push(1)
                Access(DecisionVar)
                Pred(Lte)
            constraint 8:
                Push(0)
                Access(DecisionVar)
                Push(1)
                Access(DecisionVar)
                Pred(Lt)
            constraint 9:
                Push(0)
                Access(DecisionVar)
                Push(1)
                Access(DecisionVar)
                Pred(Gte)
            constraint 10:
                Push(0)
                Access(DecisionVar)
                Push(1)
                Access(DecisionVar)
                Pred(Gt)
            constraint 11:
                Push(0)
                Access(DecisionVar)
                Push(1)
                Access(DecisionVar)
                Pred(Gt)
            constraint 12:
                Push(3)
                Access(DecisionVar)
                Push(4)
                Access(DecisionVar)
                Pred(And)
            constraint 13:
                Push(3)
                Access(DecisionVar)
                Push(4)
                Access(DecisionVar)
                Pred(Or)
        "#]],
    );
}

#[test]
fn state_read() {
    let intent = compile(
        r#"
        state x: int = storage::get(1);
        state y: int = storage::get(0x0000000000000002000000000000000200000000000000020000000000000002);
        constraint x == y;
        solve satisfy;
        "#,
    );

    check(
        &state_read_ops(&intent),
        expect_test::expect![[r#"
            state read 0:
                Constraint(Push(1))
                Memory(Alloc)
                Constraint(Push(0))
                Constraint(Push(0))
                Constraint(Push(0))
                Constraint(Push(1))
                Constraint(Push(1))
                State(StateReadWordRange)
                ControlFlow(Halt)
            state read 1:
                Constraint(Push(1))
                Memory(Alloc)
                Constraint(Push(2))
                Constraint(Push(2))
                Constraint(Push(2))
                Constraint(Push(2))
                Constraint(Push(1))
                State(StateReadWordRange)
                ControlFlow(Halt)
        "#]],
    );

    check(
        &constraints_ops(&intent),
        expect_test::expect![[r#"
            constraint 0:
                Push(0)
                Push(0)
                Access(State)
                Push(1)
                Push(0)
                Access(State)
                Pred(Eq)
        "#]],
    );

    assert_eq!(intent.slots.decision_variables, 0);
    assert_eq!(
        intent.slots.state,
        vec![
            StateSlot {
                index: 0u32,
                amount: 1,
                program_index: 0u16,
            },
            StateSlot {
                index: 1u32,
                amount: 1,
                program_index: 1u16,
            }
        ]
    );
}

#[test]
fn next_state() {
    let intent = compile(
        r#"
        let diff: int = 5;
        state x: int = storage::get(3);
        constraint x' - x == 5;
        solve satisfy;
        "#,
    );

    check(
        &state_read_ops(&intent),
        expect_test::expect![[r#"
            state read 0:
                Constraint(Push(1))
                Memory(Alloc)
                Constraint(Push(0))
                Constraint(Push(0))
                Constraint(Push(0))
                Constraint(Push(3))
                Constraint(Push(1))
                State(StateReadWordRange)
                ControlFlow(Halt)
        "#]],
    );

    check(
        &constraints_ops(&intent),
        expect_test::expect![[r#"
            constraint 0:
                Push(0)
                Access(DecisionVar)
                Push(5)
                Pred(Eq)
            constraint 1:
                Push(0)
                Push(1)
                Access(State)
                Push(0)
                Push(0)
                Access(State)
                Alu(Sub)
                Push(5)
                Pred(Eq)
        "#]],
    );

    assert_eq!(intent.slots.decision_variables, 1);
    assert_eq!(
        intent.slots.state,
        vec![StateSlot {
            index: 0u32,
            amount: 1,
            program_index: 0u16,
        },]
    );
}
