use crate::{
    asm_gen::{program_to_intents, Intents},
    error::Handler,
    parser::parse_project,
};
use std::io::Write;

mod intrinsics;
mod transient;

#[cfg(test)]
pub(super) fn check(actual: &str, expect: expect_test::Expect) {
    expect.assert_eq(actual);
}

/// Compile some code into `Intents`. Panics if anything fails.
#[cfg(test)]
pub(super) fn compile(code: &str) -> Intents {
    let mut tmpfile = tempfile::NamedTempFile::new().unwrap();
    write!(tmpfile.as_file_mut(), "{}", code).unwrap();
    let handler = Handler::default();
    let deps = Default::default();
    let program = parse_project(&handler, &deps, tmpfile.path())
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
              Stack(Push(1))
            constraint 1
              Stack(Push(0))
            --- State Reads ---
        "#]],
    );
}

#[test]
fn int_literals() {
    let intents = &compile(
        r#"
        var x: int = 4;
        var y: int = 0x333;
        solve satisfy;
        "#,
    );

    check(
        &format!("{intents}"),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Stack(Push(0))
              Access(DecisionVar)
              Stack(Push(4))
              Pred(Eq)
            constraint 1
              Stack(Push(1))
              Access(DecisionVar)
              Stack(Push(819))
              Pred(Eq)
            --- State Reads ---
        "#]],
    );

    // Single top-level intent named `Intents::ROOT_INTENT_NAME`
    assert_eq!(intents.intents.len(), 1);
}

#[test]
fn unary_not() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            var t: bool = !true;
            constraint !t;
            solve satisfy;
            "#,
            )
        ),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Stack(Push(0))
              Access(DecisionVar)
              Stack(Push(1))
              Pred(Not)
              Pred(Eq)
            constraint 1
              Stack(Push(0))
              Access(DecisionVar)
              Pred(Not)
            --- State Reads ---
        "#]],
    );
}

#[test]
fn select() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            var z = true ? 42 : 69;
            solve satisfy;
            "#,
            )
        ),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Stack(Push(0))
              Access(DecisionVar)
              Stack(Push(69))
              Stack(Push(42))
              Stack(Push(1))
              Stack(Select)
              Pred(Eq)
            --- State Reads ---
        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            var c: bool; var x: int; var y: int;
            var z = c ? x : y;
            solve satisfy;
            "#,
            )
        ),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Stack(Push(3))
              Access(DecisionVar)
              Stack(Push(2))
              Access(DecisionVar)
              Stack(Push(1))
              Access(DecisionVar)
              Stack(Push(0))
              Access(DecisionVar)
              Stack(Select)
              Pred(Eq)
            --- State Reads ---
        "#]],
    );
}

#[test]
fn select_range() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            var z = true ? 0x0000000000000001000000000000000200000000000000030000000000000004
                         : 0x0000000000000005000000000000000600000000000000070000000000000008;
            solve satisfy;
            "#,
            )
        ),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Stack(Push(0))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(5))
              Stack(Push(6))
              Stack(Push(7))
              Stack(Push(8))
              Stack(Push(1))
              Stack(Push(2))
              Stack(Push(3))
              Stack(Push(4))
              Stack(Push(4))
              Stack(Push(1))
              Stack(SelectRange)
              Stack(Push(4))
              Pred(EqRange)
            --- State Reads ---
        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            var z = true ? { 0, 1 } : { 2, 3 };
            solve satisfy;
            "#,
            )
        ),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Stack(Push(0))
              Access(DecisionVar)
              Stack(Push(1))
              Access(DecisionVar)
              Stack(Push(2))
              Stack(Push(3))
              Stack(Push(0))
              Stack(Push(1))
              Stack(Push(2))
              Stack(Push(1))
              Stack(SelectRange)
              Stack(Push(2))
              Pred(EqRange)
            --- State Reads ---
        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            var c: bool; var x: int[3]; var y: int[3];
            var z = c ? x : y;
            solve satisfy;
            "#,
            )
        ),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Stack(Push(7))
              Access(DecisionVar)
              Stack(Push(8))
              Access(DecisionVar)
              Stack(Push(9))
              Access(DecisionVar)
              Stack(Push(4))
              Access(DecisionVar)
              Stack(Push(5))
              Access(DecisionVar)
              Stack(Push(6))
              Access(DecisionVar)
              Stack(Push(1))
              Access(DecisionVar)
              Stack(Push(2))
              Access(DecisionVar)
              Stack(Push(3))
              Access(DecisionVar)
              Stack(Push(3))
              Stack(Push(0))
              Access(DecisionVar)
              Stack(SelectRange)
              Stack(Push(3))
              Pred(EqRange)
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
            var x: int; var y: int; var z: int;
            var b0: bool; var b1: bool;
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
            solve satisfy;
            "#,
            ),
        ),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Stack(Push(0))
              Access(DecisionVar)
              Stack(Push(1))
              Access(DecisionVar)
              Alu(Add)
              Stack(Push(2))
              Access(DecisionVar)
              Pred(Eq)
            constraint 1
              Stack(Push(0))
              Access(DecisionVar)
              Stack(Push(1))
              Access(DecisionVar)
              Alu(Sub)
              Stack(Push(2))
              Access(DecisionVar)
              Pred(Eq)
            constraint 2
              Stack(Push(0))
              Access(DecisionVar)
              Stack(Push(1))
              Access(DecisionVar)
              Alu(Mul)
              Stack(Push(2))
              Access(DecisionVar)
              Pred(Eq)
            constraint 3
              Stack(Push(0))
              Access(DecisionVar)
              Stack(Push(1))
              Access(DecisionVar)
              Alu(Div)
              Stack(Push(2))
              Access(DecisionVar)
              Pred(Eq)
            constraint 4
              Stack(Push(0))
              Access(DecisionVar)
              Stack(Push(1))
              Access(DecisionVar)
              Alu(Mod)
              Stack(Push(2))
              Access(DecisionVar)
              Pred(Eq)
            constraint 5
              Stack(Push(0))
              Access(DecisionVar)
              Stack(Push(1))
              Access(DecisionVar)
              Pred(Eq)
              Pred(Not)
            constraint 6
              Stack(Push(0))
              Access(DecisionVar)
              Stack(Push(1))
              Access(DecisionVar)
              Pred(Eq)
            constraint 7
              Stack(Push(0))
              Access(DecisionVar)
              Stack(Push(1))
              Access(DecisionVar)
              Pred(Lte)
            constraint 8
              Stack(Push(0))
              Access(DecisionVar)
              Stack(Push(1))
              Access(DecisionVar)
              Pred(Lt)
            constraint 9
              Stack(Push(0))
              Access(DecisionVar)
              Stack(Push(1))
              Access(DecisionVar)
              Pred(Gte)
            constraint 10
              Stack(Push(0))
              Access(DecisionVar)
              Stack(Push(1))
              Access(DecisionVar)
              Pred(Gt)
            constraint 11
              Stack(Push(0))
              Access(DecisionVar)
              Stack(Push(1))
              Access(DecisionVar)
              Pred(Gt)
            --- State Reads ---
        "#]],
    );
}

#[test]
fn short_circuit_and() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            var a: bool;
            var b: bool;
            constraint a && b;
            solve satisfy;
            "#,
            ),
        ),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Stack(Push(0))
              Stack(Push(4))
              Stack(Push(0))
              Access(DecisionVar)
              Pred(Not)
              TotalControlFlow(JumpForwardIf)
              Stack(Pop)
              Stack(Push(1))
              Access(DecisionVar)
            --- State Reads ---
        "#]],
    );
}

#[test]
fn short_circuit_or() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            var a: bool;
            var b: bool;
            constraint a || b;
            solve satisfy;
            "#,
            ),
        ),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Stack(Push(1))
              Stack(Push(4))
              Stack(Push(0))
              Access(DecisionVar)
              TotalControlFlow(JumpForwardIf)
              Stack(Pop)
              Stack(Push(1))
              Access(DecisionVar)
            --- State Reads ---
        "#]],
    );
}

#[test]
fn state_read() {
    let intents = compile(
        r#"
        state x: int = __storage_get([0, 0, 0, 1]);
        state y: int = __storage_get([2, 2, 2, 2]);
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
              Stack(Push(0))
              Stack(Push(0))
              Access(State)
              Stack(Push(1))
              Stack(Push(0))
              Access(State)
              Pred(Eq)
            constraint 1
              Stack(Push(0))
              Stack(Push(1))
              Access(State)
              Stack(Push(1))
              Stack(Push(1))
              Access(State)
              Pred(Eq)
            --- State Reads ---
            state read 0
              Constraint(Stack(Push(0)))
              Constraint(Stack(Push(0)))
              Constraint(Stack(Push(0)))
              Constraint(Stack(Push(1)))
              Constraint(Stack(Push(1)))
              StateSlots(AllocSlots)
              Constraint(Stack(Push(4)))
              Constraint(Stack(Push(1)))
              Constraint(Stack(Push(0)))
              KeyRange
              ControlFlow(Halt)
            state read 1
              Constraint(Stack(Push(2)))
              Constraint(Stack(Push(2)))
              Constraint(Stack(Push(2)))
              Constraint(Stack(Push(2)))
              Constraint(Stack(Push(1)))
              StateSlots(AllocSlots)
              Constraint(Stack(Push(4)))
              Constraint(Stack(Push(1)))
              Constraint(Stack(Push(0)))
              KeyRange
              ControlFlow(Halt)
        "#]],
    );

    // Single top-level intent named `Intents::ROOT_INTENT_NAME`
    assert_eq!(intents.intents.len(), 1);
}

#[test]
fn state_read_extern() {
    let intents = &compile(
        r#"
        state x: int = __storage_get_extern(
            0x0000000000000001000000000000000200000000000000030000000000000004,
            [11, 22, 33, 44],
        );
        state y: int = __storage_get_extern(
            0x0000000000000005000000000000000600000000000000070000000000000008,
            [55, 66, 77, 88],
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
              Stack(Push(0))
              Stack(Push(0))
              Access(State)
              Stack(Push(1))
              Stack(Push(0))
              Access(State)
              Pred(Eq)
            constraint 1
              Stack(Push(0))
              Stack(Push(1))
              Access(State)
              Stack(Push(1))
              Stack(Push(1))
              Access(State)
              Pred(Eq)
            --- State Reads ---
            state read 0
              Constraint(Stack(Push(1)))
              Constraint(Stack(Push(2)))
              Constraint(Stack(Push(3)))
              Constraint(Stack(Push(4)))
              Constraint(Stack(Push(11)))
              Constraint(Stack(Push(22)))
              Constraint(Stack(Push(33)))
              Constraint(Stack(Push(44)))
              Constraint(Stack(Push(1)))
              StateSlots(AllocSlots)
              Constraint(Stack(Push(4)))
              Constraint(Stack(Push(1)))
              Constraint(Stack(Push(0)))
              KeyRangeExtern
              ControlFlow(Halt)
            state read 1
              Constraint(Stack(Push(5)))
              Constraint(Stack(Push(6)))
              Constraint(Stack(Push(7)))
              Constraint(Stack(Push(8)))
              Constraint(Stack(Push(55)))
              Constraint(Stack(Push(66)))
              Constraint(Stack(Push(77)))
              Constraint(Stack(Push(88)))
              Constraint(Stack(Push(1)))
              StateSlots(AllocSlots)
              Constraint(Stack(Push(4)))
              Constraint(Stack(Push(1)))
              Constraint(Stack(Push(0)))
              KeyRangeExtern
              ControlFlow(Halt)
        "#]],
    );

    // Single top-level intent named `Intents::ROOT_INTENT_NAME`
    assert_eq!(intents.intents.len(), 1);
}

#[test]
fn next_state() {
    let intents = &compile(
        r#"
        var diff: int = 5;
        state x: int = __storage_get([0, 0, 0, 3]);
        constraint x' - x == 5;
        solve satisfy;
        "#,
    );

    check(
        &format!("{intents}"),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Stack(Push(0))
              Access(DecisionVar)
              Stack(Push(5))
              Pred(Eq)
            constraint 1
              Stack(Push(0))
              Stack(Push(1))
              Access(State)
              Stack(Push(0))
              Stack(Push(0))
              Access(State)
              Alu(Sub)
              Stack(Push(5))
              Pred(Eq)
            --- State Reads ---
            state read 0
              Constraint(Stack(Push(0)))
              Constraint(Stack(Push(0)))
              Constraint(Stack(Push(0)))
              Constraint(Stack(Push(3)))
              Constraint(Stack(Push(1)))
              StateSlots(AllocSlots)
              Constraint(Stack(Push(4)))
              Constraint(Stack(Push(1)))
              Constraint(Stack(Push(0)))
              KeyRange
              ControlFlow(Halt)
        "#]],
    );

    // Single top-level intent named `Intents::ROOT_INTENT_NAME`
    assert_eq!(intents.intents.len(), 1);
}

#[test]
fn b256() {
    let intents = &compile(
        r#"
        var b0 = 0x0000000000000005000000000000000600000000000000070000000000000008;
        var b1 = 0xF000000000000000500000000000000060000000000000007000000000000000;
        solve satisfy;
        "#,
    );

    check(
        &format!("{intents}"),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Stack(Push(0))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(5))
              Stack(Push(6))
              Stack(Push(7))
              Stack(Push(8))
              Stack(Push(4))
              Pred(EqRange)
            constraint 1
              Stack(Push(1))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(-1152921504606846976))
              Stack(Push(5764607523034234880))
              Stack(Push(6917529027641081856))
              Stack(Push(8070450532247928832))
              Stack(Push(4))
              Pred(EqRange)
            --- State Reads ---
        "#]],
    );
}

#[test]
fn storage_access_basic_types() {
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
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(State)
                  Stack(Push(42))
                  Pred(Eq)
                constraint 1
                  Stack(Push(1))
                  Stack(Push(1))
                  Access(State)
                  Stack(Push(98))
                  Pred(Eq)
                constraint 2
                  Stack(Push(2))
                  Stack(Push(1))
                  Access(State)
                  Stack(Push(44))
                  Pred(Eq)
                --- State Reads ---
                state read 0
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRange
                  ControlFlow(Halt)
                state read 1
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(69)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRange
                  ControlFlow(Halt)
                state read 2
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(2459565876494606882)))
                  Constraint(Stack(Push(2459565876494606882)))
                  Constraint(Stack(Push(2459565876494606882)))
                  Constraint(Stack(Push(2459565876494606882)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(5)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRange
                  ControlFlow(Halt)
            }

        "#]],
    );
}

#[test]
fn storage_access_b256_values() {
    let intents = &compile(
        r#"
storage {
    addr1: b256,
    addr2: b256,
    map1: (int => b256),
    map2: (b256 => b256),
}

intent Simple {
    state addr1 = storage::addr1;
    state addr2 = storage::addr2;
    state x = storage::map1[69];
    state y = storage::map2[0x0000000000000001000000000000000200000000000000030000000000000004];

    constraint addr1' == 0x0000000000000005000000000000000600000000000000070000000000000008;
    constraint addr2' == 0x0000000000000011000000000000002200000000000000330000000000000044;
    constraint x' == 0x0000000000000055000000000000006600000000000000770000000000000088;
    constraint y' == 0x0000000000000155000000000000026600000000000003770000000000000488;
}
        "#,
    );

    check(
        &format!("{intents}"),
        expect_test::expect![[r#"
            intent ::Simple {
                --- Constraints ---
                constraint 0
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(State)
                  Stack(Push(5))
                  Stack(Push(6))
                  Stack(Push(7))
                  Stack(Push(8))
                  Stack(Push(4))
                  Pred(EqRange)
                constraint 1
                  Stack(Push(1))
                  Stack(Push(1))
                  Access(State)
                  Stack(Push(17))
                  Stack(Push(34))
                  Stack(Push(51))
                  Stack(Push(68))
                  Stack(Push(4))
                  Pred(EqRange)
                constraint 2
                  Stack(Push(2))
                  Stack(Push(1))
                  Access(State)
                  Stack(Push(85))
                  Stack(Push(102))
                  Stack(Push(119))
                  Stack(Push(136))
                  Stack(Push(4))
                  Pred(EqRange)
                constraint 3
                  Stack(Push(3))
                  Stack(Push(1))
                  Access(State)
                  Stack(Push(341))
                  Stack(Push(614))
                  Stack(Push(887))
                  Stack(Push(1160))
                  Stack(Push(4))
                  Pred(EqRange)
                --- State Reads ---
                state read 0
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRange
                  ControlFlow(Halt)
                state read 1
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRange
                  ControlFlow(Halt)
                state read 2
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(69)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRange
                  ControlFlow(Halt)
                state read 3
                  Constraint(Stack(Push(3)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(3)))
                  Constraint(Stack(Push(4)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(5)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRange
                  ControlFlow(Halt)
            }

        "#]],
    );
}

#[test]
fn storage_access_tuples() {
    let intents = &compile(
        r#"
storage {
    u: { b256, int },
    t: { b256, { int, int } },
    w: { addr: b256, inner: { x: int, int } },
}

intent Foo {
    state u = storage::u;
    state u0 = storage::u.0;
    state u1 = storage::u.1;

    state t = storage::t;
    state t0 = storage::t .0;
    state t10 = storage::t.1.0;
    state t11 = storage::t.1.1;

    state w = storage::w;
    state w0 = storage::w .addr;
    state w10 = storage::w.inner.x;
    state w11 = storage::w.inner.1;
}
        "#,
    );

    check(
        &format!("{intents}"),
        expect_test::expect![[r#"
            intent ::Foo {
                --- Constraints ---
                --- State Reads ---
                state read 0
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(2)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(0)))
                  KeyRange
                  ControlFlow(Halt)
                state read 1
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Pop))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRange
                  ControlFlow(Halt)
                state read 2
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Pop))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRange
                  ControlFlow(Halt)
                state read 3
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(3)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(3)))
                  Constraint(Stack(Push(0)))
                  KeyRange
                  ControlFlow(Halt)
                state read 4
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Pop))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRange
                  ControlFlow(Halt)
                state read 5
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Pop))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Pop))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRange
                  ControlFlow(Halt)
                state read 6
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Pop))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Pop))
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRange
                  ControlFlow(Halt)
                state read 7
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(3)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(3)))
                  Constraint(Stack(Push(0)))
                  KeyRange
                  ControlFlow(Halt)
                state read 8
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Pop))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRange
                  ControlFlow(Halt)
                state read 9
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Pop))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Pop))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRange
                  ControlFlow(Halt)
                state read 10
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Pop))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Pop))
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRange
                  ControlFlow(Halt)
            }

        "#]],
    );
}

#[test]
fn storage_access_tuples_in_maps() {
    let intents = &compile(
        r#"
storage {
    w: b256,
    map_to_tuples: ( int => { b256, { int, int } } ),
}

intent Foo {
    state map_to_tuples_69 = storage::map_to_tuples[69];
    state map_to_tuples_69_0 = storage::map_to_tuples[69].0;
    state map_to_tuples_69_1_0 = storage::map_to_tuples[69].1.0;
    state map_to_tuples_69_1_1 = storage::map_to_tuples[69].1.1;
}
        "#,
    );

    check(
        &format!("{intents}"),
        expect_test::expect![[r#"
            intent ::Foo {
                --- Constraints ---
                --- State Reads ---
                state read 0
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(69)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(3)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(3)))
                  Constraint(Stack(Push(3)))
                  Constraint(Stack(Push(0)))
                  KeyRange
                  ControlFlow(Halt)
                state read 1
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(69)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(0)))
                  Constraint(Alu(Add))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(3)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRange
                  ControlFlow(Halt)
                state read 2
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(69)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(1)))
                  Constraint(Alu(Add))
                  Constraint(Stack(Push(0)))
                  Constraint(Alu(Add))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(3)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRange
                  ControlFlow(Halt)
                state read 3
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(69)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(1)))
                  Constraint(Alu(Add))
                  Constraint(Stack(Push(1)))
                  Constraint(Alu(Add))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(3)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRange
                  ControlFlow(Halt)
            }

        "#]],
    );
}

#[test]
fn storage_access_tuples_extern() {
    let intents = &compile(
        r#"
interface Foo {
    storage {
        u: { b256, int },
        t: { b256, { int, int } },
        w: { addr: b256, inner: { x: int, int } },
    }
}

intent Bar {
    interface FooInstance = Foo(0x1111111111111111111111111111111111111111111111111111111111111111);

    state foo_u = FooInstance::storage::u;
    state foo_u0 = FooInstance::storage::u.0;
    state foo_u1 = FooInstance::storage::u.1;

    state foo_t = FooInstance::storage::t;
    state foo_t0 = FooInstance::storage::t .0;
    state foo_t10 = FooInstance::storage::t.1.0;
    state foo_t11 = FooInstance::storage::t.1.1;

    state foo_w = FooInstance::storage::w;
    state foo_w0 = FooInstance::storage::w .addr;
    state foo_w10 = FooInstance::storage::w.inner.x;
    state foo_w11 = FooInstance::storage::w.inner.1;
}
        "#,
    );

    check(
        &format!("{intents}"),
        expect_test::expect![[r#"
            intent ::Bar {
                --- Constraints ---
                --- State Reads ---
                state read 0
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(2)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(0)))
                  KeyRangeExtern
                  ControlFlow(Halt)
                state read 1
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Pop))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRangeExtern
                  ControlFlow(Halt)
                state read 2
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Pop))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRangeExtern
                  ControlFlow(Halt)
                state read 3
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(3)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(3)))
                  Constraint(Stack(Push(0)))
                  KeyRangeExtern
                  ControlFlow(Halt)
                state read 4
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Pop))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRangeExtern
                  ControlFlow(Halt)
                state read 5
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Pop))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Pop))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRangeExtern
                  ControlFlow(Halt)
                state read 6
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Pop))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Pop))
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRangeExtern
                  ControlFlow(Halt)
                state read 7
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(3)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(3)))
                  Constraint(Stack(Push(0)))
                  KeyRangeExtern
                  ControlFlow(Halt)
                state read 8
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Pop))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRangeExtern
                  ControlFlow(Halt)
                state read 9
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Pop))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Pop))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRangeExtern
                  ControlFlow(Halt)
                state read 10
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Pop))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Pop))
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRangeExtern
                  ControlFlow(Halt)
            }

        "#]],
    );
}

#[test]
fn storage_access_complex_maps() {
    let intents = &compile(
        r#"
storage {
    map_in_map: (int => (b256 => int)),
    map_in_map_in_map: (int => (b256 => (int => b256))),
}

intent Simple {
    state map_in_map_entry = storage::map_in_map[9][0x0000000000000001000000000000000200000000000000030000000000000004];
    state map_in_map_in_map_entry = storage::map_in_map_in_map[88][0x0000000000000008000000000000000700000000000000060000000000000005][999];

    constraint map_in_map_entry' == 42;
    constraint map_in_map_in_map_entry' == 0x000000000000000F000000000000000F000000000000000F000000000000000F;
}
        "#,
    );

    check(
        &format!("{intents}"),
        expect_test::expect![[r#"
            intent ::Simple {
                --- Constraints ---
                constraint 0
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(State)
                  Stack(Push(42))
                  Pred(Eq)
                constraint 1
                  Stack(Push(1))
                  Stack(Push(1))
                  Access(State)
                  Stack(Push(15))
                  Stack(Push(15))
                  Stack(Push(15))
                  Stack(Push(15))
                  Stack(Push(4))
                  Pred(EqRange)
                --- State Reads ---
                state read 0
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(9)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(2)))
                  Constraint(Stack(Push(3)))
                  Constraint(Stack(Push(4)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(6)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRange
                  ControlFlow(Halt)
                state read 1
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(88)))
                  Constraint(Stack(Push(8)))
                  Constraint(Stack(Push(7)))
                  Constraint(Stack(Push(6)))
                  Constraint(Stack(Push(5)))
                  Constraint(Stack(Push(999)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(7)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRange
                  ControlFlow(Halt)
            }

        "#]],
    );
}

#[test]
fn storage_external_access() {
    let intents = &compile(
        r#"
interface Extern1 {
    storage {
        x: int,
        map: (int => (bool => b256)),
    }
}

interface Extern2 {
    storage {
        w: int,
        map: (b256 => (int => bool)),
    }
}

intent Foo {
    interface Extern1Instance = Extern1(0x1233683A8F6B8AF1707FF76F40FC5EE714872F88FAEBB8F22851E93F56770128);
    interface Extern2Instance = Extern2(0x0C15A3534349FC710174299BA8F0347284955B35A28C01CF45A910495FA1EF2D);

    state x = Extern1Instance::storage::x;
    state y = Extern1Instance::storage::map[3][true];
    state w = Extern2Instance::storage::w;
    state z = Extern2Instance::storage::map[0x1111111111111111111111111111111111111111111111111111111111111111][69];

    constraint x' - x == 1;
    constraint y == 0x2222222222222222222222222222222222222222222222222222222222222222;
    constraint w' - w == 3;
    constraint z' == false;
}
        "#,
    );

    check(
        &format!("{intents}"),
        expect_test::expect![[r#"
            intent ::Foo {
                --- Constraints ---
                constraint 0
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(State)
                  Stack(Push(0))
                  Stack(Push(0))
                  Access(State)
                  Alu(Sub)
                  Stack(Push(1))
                  Pred(Eq)
                constraint 1
                  Stack(Push(1))
                  Stack(Push(0))
                  Access(State)
                  Stack(Push(2459565876494606882))
                  Stack(Push(2459565876494606882))
                  Stack(Push(2459565876494606882))
                  Stack(Push(2459565876494606882))
                  Stack(Push(4))
                  Pred(EqRange)
                constraint 2
                  Stack(Push(2))
                  Stack(Push(1))
                  Access(State)
                  Stack(Push(2))
                  Stack(Push(0))
                  Access(State)
                  Alu(Sub)
                  Stack(Push(3))
                  Pred(Eq)
                constraint 3
                  Stack(Push(3))
                  Stack(Push(1))
                  Access(State)
                  Stack(Push(0))
                  Pred(Eq)
                --- State Reads ---
                state read 0
                  Constraint(Stack(Push(1311506517218527985)))
                  Constraint(Stack(Push(8106469911493893863)))
                  Constraint(Stack(Push(1479203267986307314)))
                  Constraint(Stack(Push(2905359692873531688)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRangeExtern
                  ControlFlow(Halt)
                state read 1
                  Constraint(Stack(Push(1311506517218527985)))
                  Constraint(Stack(Push(8106469911493893863)))
                  Constraint(Stack(Push(1479203267986307314)))
                  Constraint(Stack(Push(2905359692873531688)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(3)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(3)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRangeExtern
                  ControlFlow(Halt)
                state read 2
                  Constraint(Stack(Push(870781680972594289)))
                  Constraint(Stack(Push(104754439867348082)))
                  Constraint(Stack(Push(-8893101603254697521)))
                  Constraint(Stack(Push(5019561167004233517)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRangeExtern
                  ControlFlow(Halt)
                state read 3
                  Constraint(Stack(Push(870781680972594289)))
                  Constraint(Stack(Push(104754439867348082)))
                  Constraint(Stack(Push(-8893101603254697521)))
                  Constraint(Stack(Push(5019561167004233517)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(69)))
                  Constraint(Stack(Push(1)))
                  StateSlots(AllocSlots)
                  Constraint(Stack(Push(6)))
                  Constraint(Stack(Push(1)))
                  Constraint(Stack(Push(0)))
                  KeyRangeExtern
                  ControlFlow(Halt)
            }

        "#]],
    );
}
