use crate::{
    asm_gen::{program_to_intents, Intents},
    error::Handler,
    parser::parse_project,
};
use essential_types::slots::*;
use std::io::Write;

#[cfg(test)]
fn check(actual: &str, expect: expect_test::Expect) {
    expect.assert_eq(actual);
}

/// Compile some code into `Intents`. Panics if anything fails.
#[cfg(test)]
fn compile(code: &str) -> Intents {
    let mut tmpfile = tempfile::NamedTempFile::new().unwrap();
    write!(tmpfile.as_file_mut(), "{}", code).unwrap();
    let handler = Handler::default();
    let program = parse_project(&handler, tmpfile.path())
        .unwrap()
        .compile(&handler)
        .unwrap();
    program_to_intents(&handler, &program).unwrap()
}

#[test]
fn bool_literals() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            constraint true;
            constraint false;
            solve satisfy;
            "#,
            )
        ),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Push(1)
            constraint 1
              Push(0)
            --- State Reads ---
        "#]],
    );
}

#[test]
fn int_literals() {
    let intents = &compile(
        r#"
        let x: int = 4;
        let y: int = 0x333;
        solve satisfy;
        "#,
    );

    check(
        &format!("{intents}"),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Push(0)
              Access(DecisionVar)
              Push(4)
              Pred(Eq)
            constraint 1
              Push(1)
              Access(DecisionVar)
              Push(819)
              Pred(Eq)
            --- State Reads ---
        "#]],
    );

    // Single top-level intent named `Intents::ROOT_INTENT_NAME`
    assert_eq!(intents.intents.len(), 1);
    let intent = intents.root_intent();
    assert_eq!(intent.slots.decision_variables, 2);
    assert!(intent.slots.state.is_empty());
}

#[test]
fn unary_not() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            let t: bool = !true;
            constraint !t;
            solve satisfy;
            "#,
            )
        ),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Push(0)
              Access(DecisionVar)
              Push(1)
              Pred(Not)
              Pred(Eq)
            constraint 1
              Push(0)
              Access(DecisionVar)
              Pred(Not)
            --- State Reads ---
        "#]],
    );
}

#[test]
fn binary_ops() {
    check(
        &format!(
            "{}",
            compile(
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
            ),
        ),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Push(0)
              Access(DecisionVar)
              Push(1)
              Access(DecisionVar)
              Alu(Add)
              Push(2)
              Access(DecisionVar)
              Pred(Eq)
            constraint 1
              Push(0)
              Access(DecisionVar)
              Push(1)
              Access(DecisionVar)
              Alu(Sub)
              Push(2)
              Access(DecisionVar)
              Pred(Eq)
            constraint 2
              Push(0)
              Access(DecisionVar)
              Push(1)
              Access(DecisionVar)
              Alu(Mul)
              Push(2)
              Access(DecisionVar)
              Pred(Eq)
            constraint 3
              Push(0)
              Access(DecisionVar)
              Push(1)
              Access(DecisionVar)
              Alu(Div)
              Push(2)
              Access(DecisionVar)
              Pred(Eq)
            constraint 4
              Push(0)
              Access(DecisionVar)
              Push(1)
              Access(DecisionVar)
              Alu(Mod)
              Push(2)
              Access(DecisionVar)
              Pred(Eq)
            constraint 5
              Push(0)
              Access(DecisionVar)
              Push(1)
              Access(DecisionVar)
              Pred(Eq)
              Pred(Not)
            constraint 6
              Push(0)
              Access(DecisionVar)
              Push(1)
              Access(DecisionVar)
              Pred(Eq)
            constraint 7
              Push(0)
              Access(DecisionVar)
              Push(1)
              Access(DecisionVar)
              Pred(Lte)
            constraint 8
              Push(0)
              Access(DecisionVar)
              Push(1)
              Access(DecisionVar)
              Pred(Lt)
            constraint 9
              Push(0)
              Access(DecisionVar)
              Push(1)
              Access(DecisionVar)
              Pred(Gte)
            constraint 10
              Push(0)
              Access(DecisionVar)
              Push(1)
              Access(DecisionVar)
              Pred(Gt)
            constraint 11
              Push(0)
              Access(DecisionVar)
              Push(1)
              Access(DecisionVar)
              Pred(Gt)
            constraint 12
              Push(3)
              Access(DecisionVar)
              Push(4)
              Access(DecisionVar)
              Pred(And)
            constraint 13
              Push(3)
              Access(DecisionVar)
              Push(4)
              Access(DecisionVar)
              Pred(Or)
            --- State Reads ---
        "#]],
    );
}

#[test]
fn state_read() {
    let intents = compile(
        r#"
        state x: int = storage_lib::get(0x0000000000000000000000000000000000000000000000000000000000000001);
        state y: int = storage_lib::get(0x0000000000000002000000000000000200000000000000020000000000000002);
        constraint x == y;
        constraint x' == y';
        solve satisfy;
        "#,
    );

    check(
        &format!("{intents}"),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Push(0)
              Push(0)
              Access(State)
              Push(1)
              Push(0)
              Access(State)
              Pred(Eq)
            constraint 1
              Push(0)
              Push(1)
              Access(State)
              Push(1)
              Push(1)
              Access(State)
              Pred(Eq)
            --- State Reads ---
            state read 0
              Constraint(Push(0))
              Constraint(Push(0))
              Constraint(Push(0))
              Constraint(Push(1))
              Constraint(Push(1))
              Memory(Alloc)
              Constraint(Push(1))
              State(StateReadWordRange)
              ControlFlow(Halt)
            state read 1
              Constraint(Push(2))
              Constraint(Push(2))
              Constraint(Push(2))
              Constraint(Push(2))
              Constraint(Push(1))
              Memory(Alloc)
              Constraint(Push(1))
              State(StateReadWordRange)
              ControlFlow(Halt)
        "#]],
    );

    // Single top-level intent named `Intents::ROOT_INTENT_NAME`
    assert_eq!(intents.intents.len(), 1);
    let intent = intents.root_intent();
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
fn state_read_extern() {
    let intents = &compile(
        r#"
        state x: int = storage_lib::get_extern(
            0x0000000000000001000000000000000200000000000000030000000000000004,
            0x0000000000000011000000000000002200000000000000330000000000000044,
        );
        state y: int = storage_lib::get_extern(
            0x0000000000000005000000000000000600000000000000070000000000000008,
            0x0000000000000055000000000000006600000000000000770000000000000088,
        );
        constraint x == y;
        constraint x' == y';
        solve satisfy;
        "#,
    );

    check(
        &format!("{intents}"),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Push(0)
              Push(0)
              Access(State)
              Push(1)
              Push(0)
              Access(State)
              Pred(Eq)
            constraint 1
              Push(0)
              Push(1)
              Access(State)
              Push(1)
              Push(1)
              Access(State)
              Pred(Eq)
            --- State Reads ---
            state read 0
              Constraint(Push(1))
              Constraint(Push(2))
              Constraint(Push(3))
              Constraint(Push(4))
              Constraint(Push(17))
              Constraint(Push(34))
              Constraint(Push(51))
              Constraint(Push(68))
              Constraint(Push(1))
              Memory(Alloc)
              Constraint(Push(1))
              State(StateReadWordRangeExtern)
              ControlFlow(Halt)
            state read 1
              Constraint(Push(5))
              Constraint(Push(6))
              Constraint(Push(7))
              Constraint(Push(8))
              Constraint(Push(85))
              Constraint(Push(102))
              Constraint(Push(119))
              Constraint(Push(136))
              Constraint(Push(1))
              Memory(Alloc)
              Constraint(Push(1))
              State(StateReadWordRangeExtern)
              ControlFlow(Halt)
        "#]],
    );

    // Single top-level intent named `Intents::ROOT_INTENT_NAME`
    assert_eq!(intents.intents.len(), 1);
    let intent = intents.root_intent();
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
    let intents = &compile(
        r#"
        let diff: int = 5;
        state x: int = storage_lib::get(0x0000000000000000000000000000000000000000000000000000000000000003);
        constraint x' - x == 5;
        solve satisfy;
        "#,
    );

    check(
        &format!("{intents}"),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Push(0)
              Access(DecisionVar)
              Push(5)
              Pred(Eq)
            constraint 1
              Push(0)
              Push(1)
              Access(State)
              Push(0)
              Push(0)
              Access(State)
              Alu(Sub)
              Push(5)
              Pred(Eq)
            --- State Reads ---
            state read 0
              Constraint(Push(0))
              Constraint(Push(0))
              Constraint(Push(0))
              Constraint(Push(3))
              Constraint(Push(1))
              Memory(Alloc)
              Constraint(Push(1))
              State(StateReadWordRange)
              ControlFlow(Halt)
        "#]],
    );

    // Single top-level intent named `Intents::ROOT_INTENT_NAME`
    assert_eq!(intents.intents.len(), 1);
    let intent = intents.root_intent();
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

#[test]
fn b256() {
    let intents = &compile(
        r#"
        let b0 = 0x0000000000000005000000000000000600000000000000070000000000000008;
        let b1 = 0xF000000000000000500000000000000060000000000000007000000000000000;
        solve satisfy;
        "#,
    );

    check(
        &format!("{intents}"),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Push(0)
              Access(DecisionVar)
              Push(1)
              Access(DecisionVar)
              Push(2)
              Access(DecisionVar)
              Push(3)
              Access(DecisionVar)
              Push(5)
              Push(6)
              Push(7)
              Push(8)
              Pred(Eq4)
            constraint 1
              Push(4)
              Access(DecisionVar)
              Push(5)
              Access(DecisionVar)
              Push(6)
              Access(DecisionVar)
              Push(7)
              Access(DecisionVar)
              Push(-1152921504606846976)
              Push(5764607523034234880)
              Push(6917529027641081856)
              Push(8070450532247928832)
              Pred(Eq4)
            --- State Reads ---
        "#]],
    );
}

#[test]
fn sender() {
    let intents = &compile(
        r#"
storage {
    supply: int,
    map1: (int => int),
    map2: (b256 => int),
}

intent Simple {
    state supply = storage::supply;
    state x = storage::map1[69];
    state y = storage::map2[0x2222222222222222222222222222222222222222222222222222222222222222];

    constraint supply' == 42;
    constraint x' == 98;
    constraint y' == 44;
}
        "#,
    );

    check(
        &format!("{intents}"),
        expect_test::expect![[r#"
            intent ::Simple {
                --- Constraints ---
                constraint 0
                  Push(0)
                  Push(1)
                  Access(State)
                  Push(42)
                  Pred(Eq)
                constraint 1
                  Push(1)
                  Push(1)
                  Access(State)
                  Push(98)
                  Pred(Eq)
                constraint 2
                  Push(2)
                  Push(1)
                  Access(State)
                  Push(44)
                  Pred(Eq)
                --- State Reads ---
                state read 0
                  Constraint(Push(0))
                  Constraint(Push(0))
                  Constraint(Push(0))
                  Constraint(Push(0))
                  Constraint(Push(1))
                  Memory(Alloc)
                  Constraint(Push(1))
                  State(StateReadWordRange)
                  ControlFlow(Halt)
                state read 1
                  Constraint(Push(1))
                  Constraint(Push(69))
                  Constraint(Push(2))
                  Constraint(Crypto(Sha256))
                  Constraint(Push(1))
                  Memory(Alloc)
                  Constraint(Push(1))
                  State(StateReadWordRange)
                  ControlFlow(Halt)
                state read 2
                  Constraint(Push(2))
                  Constraint(Push(2459565876494606882))
                  Constraint(Push(2459565876494606882))
                  Constraint(Push(2459565876494606882))
                  Constraint(Push(2459565876494606882))
                  Constraint(Push(5))
                  Constraint(Crypto(Sha256))
                  Constraint(Push(1))
                  Memory(Alloc)
                  Constraint(Push(1))
                  State(StateReadWordRange)
                  ControlFlow(Halt)
            }

        "#]],
    );
}

#[test]
fn storage_access() {}
