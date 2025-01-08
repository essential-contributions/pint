use crate::{
    asm_gen::{compile_contract, CompiledContract},
    error::Handler,
    parser::parse_project,
};
use std::io::Write;

mod external_calls;
mod intrinsics;
mod multi_predicates;

#[cfg(test)]
pub(super) fn check(actual: &str, expect: expect_test::Expect) {
    expect.assert_eq(actual);
}

/// Compile some code into `CompiledContract`. Panics if anything fails.
#[cfg(test)]
pub(super) fn compile(code: &str) -> CompiledContract {
    use crate::predicate::CompileOptions;

    let mut tmpfile = tempfile::NamedTempFile::new().unwrap();
    write!(tmpfile.as_file_mut(), "{}", code).unwrap();
    let handler = Handler::default();
    let deps = Default::default();
    let contract = parse_project(&handler, &deps, tmpfile.path())
        .unwrap()
        .compile(
            &handler,
            CompileOptions {
                skip_optimize: false,
                print_flat: false,
            },
        )
        .unwrap();
    compile_contract(&handler, Default::default(), &contract).unwrap()
}

/// Compile some code into `CompiledContract`. Panics if compilation is successful.
#[cfg(test)]
pub(super) fn compile_with_error(code: &str) -> String {
    use crate::{error::ReportableError, predicate::CompileOptions};

    let mut tmpfile = tempfile::NamedTempFile::new().unwrap();
    write!(tmpfile.as_file_mut(), "{}", code).unwrap();
    let handler = Handler::default();
    let deps = Default::default();
    let contract = parse_project(&handler, &deps, tmpfile.path())
        .unwrap()
        .compile(
            &handler,
            CompileOptions {
                skip_optimize: false,
                print_flat: false,
            },
        )
        .unwrap();
    let _ = compile_contract(&handler, Default::default(), &contract);

    if handler.has_errors() {
        let errors = handler.consume().0;
        errors.iter().fold(String::new(), |acc, error| {
            format!("{}{}", acc, error.display_raw())
        })
    } else {
        panic!("compilation was successful");
    }
}

#[test]
fn bool_literals() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test(a: bool[2]) {
              constraint a == [true, false];
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::test {
                --- Nodes ---
                node 0
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 1
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(2))
                  Access(PredicateData)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(2))
                  Pred(EqRange)
            }

        "#]],
    );
}

#[test]
fn int_literals() {
    let compiled_contract = &compile(
        r#"
        predicate test(x: int, y: int) {
            constraint x == 4;
            constraint y == 0x333;
        }
        "#,
    );

    check(
        &format!("{compiled_contract}"),
        expect_test::expect![[r#"
            predicate ::test {
                --- Nodes ---
                node 0
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 1
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(819))
                  Pred(Eq)
                node 2
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(4))
                  Pred(Eq)
            }

        "#]],
    );

    // Single top-level predicate named `CompiledContract::ROOT_PRED_NAME`
    assert_eq!(compiled_contract.contract.predicates.len(), 1);
}

#[test]
fn unary_not() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test(t: bool) {
                constraint t == !true;
                constraint !t;
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::test {
                --- Nodes ---
                node 0
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 1
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Pred(Not)
                node 2
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(0))
                  Pred(Eq)
            }

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
            predicate test(y: bool, z: int) {
                constraint z == (y ? 42 : 69);
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::test {
                --- Nodes ---
                node 0
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 1
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(69))
                  Stack(Push(42))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Select)
                  Pred(Eq)
            }

        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test(
                c: bool,
                x: int,
                y: int,
                z: int,
            ) {
                constraint z == (c ? x : y);
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::test {
                --- Nodes ---
                node 0
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 1
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Select)
                  Pred(Eq)
            }

        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            storage { x: int }
            predicate test(y: int) {
                let x = storage::x;
                constraint y == (x == nil ? 11 : x);
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::test {
                --- Nodes ---
                node 0
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 1
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 2
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(7))
                  Stack(Push(0))
                  Memory(Load)
                  Stack(Push(0))
                  Pred(Eq)
                  TotalControlFlow(JumpForwardIf)
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(2))
                  Stack(Push(1))
                  TotalControlFlow(JumpForwardIf)
                  Stack(Push(11))
                  Pred(Eq)
            }

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
            predicate test(y: bool, z: b256) {
                constraint z == (y ? 0x0000000000000001000000000000000200000000000000030000000000000004
                             : 0x0000000000000005000000000000000600000000000000070000000000000008);
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::test {
                --- Nodes ---
                node 0
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 1
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(PredicateData)
                  Stack(Push(5))
                  Stack(Push(6))
                  Stack(Push(7))
                  Stack(Push(8))
                  Stack(Push(1))
                  Stack(Push(2))
                  Stack(Push(3))
                  Stack(Push(4))
                  Stack(Push(4))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(SelectRange)
                  Stack(Push(4))
                  Pred(EqRange)
            }

        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test(y: bool, z: {int, int}) {
                constraint z == (y ? { 0, 1 } : { 2, 3 });
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::test {
                --- Nodes ---
                node 0
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 1
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(2))
                  Access(PredicateData)
                  Stack(Push(2))
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(SelectRange)
                  Stack(Push(2))
                  Pred(EqRange)
            }

        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test(c: bool, x: int[3], y: int[3], z: int[3]) {
                constraint z == (c ? x : y);
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::test {
                --- Nodes ---
                node 0
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 1
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(3))
                  Access(PredicateData)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(3))
                  Access(PredicateData)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(3))
                  Access(PredicateData)
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(SelectRange)
                  Stack(Push(3))
                  Pred(EqRange)
            }

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
            predicate test(
                x: int, y: int, z: int,
                b0: bool, b1: bool,
            ) {
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
            }
            "#,
            ),
        ),
        expect_test::expect![[r#"
            predicate ::test {
                --- Nodes ---
                node 0
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 1
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Pred(Gt)
                node 2
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Pred(Gte)
                node 3
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Pred(Lt)
                node 4
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Pred(Lte)
                node 5
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Pred(Eq)
                node 6
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Pred(Eq)
                  Pred(Not)
                node 7
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Alu(Mod)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Pred(Eq)
                node 8
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Alu(Div)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Pred(Eq)
                node 9
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Alu(Mul)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Pred(Eq)
                node 10
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Alu(Sub)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Pred(Eq)
                node 11
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Alu(Add)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Pred(Eq)
            }

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
            predicate test(a: bool, b: bool) {
                constraint a && b;
            }
            "#,
            ),
        ),
        expect_test::expect![[r#"
            predicate ::test {
                --- Nodes ---
                node 0
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 1
                  Stack(Push(0))
                  Stack(Push(6))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Pred(Not)
                  TotalControlFlow(JumpForwardIf)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
            }

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
            predicate test(a: bool, b: bool) {
                constraint a || b;
            }
            "#,
            ),
        ),
        expect_test::expect![[r#"
            predicate ::test {
                --- Nodes ---
                node 0
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 1
                  Stack(Push(1))
                  Stack(Push(6))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  TotalControlFlow(JumpForwardIf)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
            }

        "#]],
    );
}

#[test]
fn next_state() {
    let compiled_contract = &compile(
        r#"
        storage { x: int }
        predicate test(diff: int) {
            constraint diff == 5;
            let x: int = storage::x;
            constraint x' - x == 5;
        }
        "#,
    );

    check(
        &format!("{compiled_contract}"),
        expect_test::expect![[r#"
            predicate ::test {
                --- Nodes ---
                node 0
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 1
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 2
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 3
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(5))
                  Pred(Eq)
                node 4
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(3))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Alu(Sub)
                  Stack(Push(5))
                  Pred(Eq)
            }

        "#]],
    );

    // Single top-level predicate named `CompiledContract::ROOT_PRED_NAME`
    assert_eq!(compiled_contract.contract.predicates.len(), 1);
}

#[test]
fn b256() {
    let compiled_contract = &compile(
        r#"
        predicate test(b0: b256, b1: b256) {
            constraint b0 == 0x0000000000000005000000000000000600000000000000070000000000000008;
            constraint b1 == 0xF000000000000000500000000000000060000000000000007000000000000000;
        }
        "#,
    );

    check(
        &format!("{compiled_contract}"),
        expect_test::expect![[r#"
            predicate ::test {
                --- Nodes ---
                node 0
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 1
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(PredicateData)
                  Stack(Push(-1152921504606846976))
                  Stack(Push(5764607523034234880))
                  Stack(Push(6917529027641081856))
                  Stack(Push(8070450532247928832))
                  Stack(Push(4))
                  Pred(EqRange)
                node 2
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(PredicateData)
                  Stack(Push(5))
                  Stack(Push(6))
                  Stack(Push(7))
                  Stack(Push(8))
                  Stack(Push(4))
                  Pred(EqRange)
            }

        "#]],
    );
}

#[test]
fn storage_access_basic_types() {
    let compiled_contract = &compile(
        r#"
storage {
    supply: int,
    map1: (int => int),
    map2: (b256 => int),
}

predicate Simple() {
    let supply = storage::supply;
    let x = storage::map1[69];
    let y = storage::map2[0x2222222222222222222222222222222222222222222222222222222222222222];

    constraint supply' == 42;
    constraint x' == 98;
    constraint y' == 44;
}
        "#,
    );

    check(
        &format!("{compiled_contract}"),
        expect_test::expect![[r#"
            predicate ::Simple {
                --- Nodes ---
                node 0
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(2))
                  Stack(Push(2459565876494606882))
                  Stack(Push(2459565876494606882))
                  Stack(Push(2459565876494606882))
                  Stack(Push(2459565876494606882))
                  Stack(Push(5))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 1
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(69))
                  Stack(Push(2))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 2
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 3
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 4
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(44))
                  Pred(Eq)
                node 5
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(42))
                  Pred(Eq)
                node 6
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(98))
                  Pred(Eq)
            }

        "#]],
    );
}

#[test]
fn storage_access_b256_values() {
    let compiled_contract = &compile(
        r#"
storage {
    addr1: b256,
    addr2: b256,
    map1: (int => b256),
    map2: (b256 => b256),
}

predicate Simple() {
    let addr1 = storage::addr1;
    let addr2 = storage::addr2;
    let x = storage::map1[69];
    let y = storage::map2[0x0000000000000001000000000000000200000000000000030000000000000004];

    constraint addr1' == 0x0000000000000005000000000000000600000000000000070000000000000008;
    constraint addr2' == 0x0000000000000011000000000000002200000000000000330000000000000044;
    constraint x' == 0x0000000000000055000000000000006600000000000000770000000000000088;
    constraint y' == 0x0000000000000155000000000000026600000000000003770000000000000488;
}
        "#,
    );

    check(
        &format!("{compiled_contract}"),
        expect_test::expect![[r#"
            predicate ::Simple {
                --- Nodes ---
                node 0
                  Stack(Push(6))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(3))
                  Stack(Push(1))
                  Stack(Push(2))
                  Stack(Push(3))
                  Stack(Push(4))
                  Stack(Push(5))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(4))
                  Memory(StoreRange)
                  Stack(Push(5))
                  Memory(Free)
                node 1
                  Stack(Push(6))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(2))
                  Stack(Push(69))
                  Stack(Push(2))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(4))
                  Memory(StoreRange)
                  Stack(Push(5))
                  Memory(Free)
                node 2
                  Stack(Push(6))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(4))
                  Memory(StoreRange)
                  Stack(Push(5))
                  Memory(Free)
                node 3
                  Stack(Push(6))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(4))
                  Memory(StoreRange)
                  Stack(Push(5))
                  Memory(Free)
                node 4
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 5
                  Stack(Push(1))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(341))
                  Stack(Push(614))
                  Stack(Push(887))
                  Stack(Push(1160))
                  Stack(Push(4))
                  Pred(EqRange)
                node 6
                  Stack(Push(1))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(85))
                  Stack(Push(102))
                  Stack(Push(119))
                  Stack(Push(136))
                  Stack(Push(4))
                  Pred(EqRange)
                node 7
                  Stack(Push(1))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(5))
                  Stack(Push(6))
                  Stack(Push(7))
                  Stack(Push(8))
                  Stack(Push(4))
                  Pred(EqRange)
                node 8
                  Stack(Push(1))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(17))
                  Stack(Push(34))
                  Stack(Push(51))
                  Stack(Push(68))
                  Stack(Push(4))
                  Pred(EqRange)
            }

        "#]],
    );
}

#[test]
fn storage_access_tuples() {
    let compiled_contract = &compile(
        r#"
storage {
    u: { b256, int },
    t: { b256, { int, int } },
    w: { addr: b256, inner: { x: int, int } },
}

predicate Foo() {
    let u = storage::u;
    let u0 = storage::u.0;
    let u1 = storage::u.1;

    let t = storage::t;
    let t0 = storage::t.0;
    let t10 = storage::t.1.0;
    let t11 = storage::t.1.1;

    let w = storage::w;
    let w0 = storage::w.addr;
    let w10 = storage::w.inner.x;
    let w11 = storage::w.inner.1;

    constraint u == { 0x0000000000000000000000000000000000000000000000000000000000000000, 0 };
    constraint u0 == 0x0000000000000000000000000000000000000000000000000000000000000000;
    constraint u1 == 0;
    constraint t == { 0x0000000000000000000000000000000000000000000000000000000000000000, { 0, 0 } };
    constraint t0 == 0x0000000000000000000000000000000000000000000000000000000000000000;
    constraint t10 == 0;
    constraint t11 == 0;
    constraint w == { addr: 0x0000000000000000000000000000000000000000000000000000000000000000 , inner: { 0, 0 } };
    constraint w0 == 0x0000000000000000000000000000000000000000000000000000000000000000;
    constraint w10 == 0;
    constraint w11 == 0;
}
        "#,
    );

    check(
        &format!("{compiled_contract}"),
        expect_test::expect![[r#"
            predicate ::Foo {
                --- Nodes ---
                node 0
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(2))
                  Stack(Push(2))
                  Stack(Push(2))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 1
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(2))
                  Stack(Push(1))
                  Stack(Push(2))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 2
                  Stack(Push(6))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(2))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(4))
                  Memory(StoreRange)
                  Stack(Push(5))
                  Memory(Free)
                node 3
                  Stack(Push(12))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(2))
                  Stack(Push(3))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(6))
                  Stack(Push(6))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(3))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(5))
                  Memory(Load)
                  Alu(Add)
                  Memory(Store)
                  Stack(Push(6))
                  Memory(StoreRange)
                  Stack(Push(7))
                  Memory(Free)
                node 4
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(2))
                  Stack(Push(2))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 5
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(2))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 6
                  Stack(Push(6))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(2))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(4))
                  Memory(StoreRange)
                  Stack(Push(5))
                  Memory(Free)
                node 7
                  Stack(Push(12))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(2))
                  Stack(Push(3))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(6))
                  Stack(Push(6))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(3))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(5))
                  Memory(Load)
                  Alu(Add)
                  Memory(Store)
                  Stack(Push(6))
                  Memory(StoreRange)
                  Stack(Push(7))
                  Memory(Free)
                node 8
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(2))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 9
                  Stack(Push(6))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(2))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(4))
                  Memory(StoreRange)
                  Stack(Push(5))
                  Memory(Free)
                node 10
                  Stack(Push(9))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(2))
                  Stack(Push(2))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(4))
                  Stack(Push(5))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(3))
                  Memory(Load)
                  Alu(Add)
                  Memory(Store)
                  Stack(Push(5))
                  Memory(StoreRange)
                  Stack(Push(6))
                  Memory(Free)
                node 11
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 12
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Pred(Eq)
                node 13
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Pred(Eq)
                node 14
                  Stack(Push(1))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(4))
                  Pred(EqRange)
                node 15
                  Stack(Push(1))
                  Stack(Push(6))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(6))
                  Pred(EqRange)
                node 16
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Pred(Eq)
                node 17
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Pred(Eq)
                node 18
                  Stack(Push(1))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(4))
                  Pred(EqRange)
                node 19
                  Stack(Push(1))
                  Stack(Push(6))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(6))
                  Pred(EqRange)
                node 20
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Pred(Eq)
                node 21
                  Stack(Push(1))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(4))
                  Pred(EqRange)
                node 22
                  Stack(Push(1))
                  Stack(Push(5))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(5))
                  Pred(EqRange)
            }

        "#]],
    );
}

#[test]
fn storage_access_tuples_in_maps() {
    let compiled_contract = &compile(
        r#"
storage {
    w: b256,
    map_to_tuples: ( int => { b256, { int, int } } ),
}

predicate Foo() {
    let map_to_tuples_69 = storage::map_to_tuples[69];
    let map_to_tuples_69_0 = storage::map_to_tuples[69].0;
    let map_to_tuples_69_1_0 = storage::map_to_tuples[69].1.0;
    let map_to_tuples_69_1_1 = storage::map_to_tuples[69].1.1;

    constraint map_to_tuples_69 == { 0x0000000000000000000000000000000000000000000000000000000000000000, { 0, 0 } };
    constraint map_to_tuples_69_0 == 0x0000000000000000000000000000000000000000000000000000000000000000;
    constraint map_to_tuples_69_1_0 == 0;
    constraint map_to_tuples_69_1_1 == 0;
}
        "#,
    );

    check(
        &format!("{compiled_contract}"),
        expect_test::expect![[r#"
            predicate ::Foo {
                --- Nodes ---
                node 0
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(69))
                  Stack(Push(2))
                  Stack(Push(3))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 1
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(69))
                  Stack(Push(1))
                  Stack(Push(3))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 2
                  Stack(Push(6))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(69))
                  Stack(Push(0))
                  Stack(Push(3))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(4))
                  Memory(StoreRange)
                  Stack(Push(5))
                  Memory(Free)
                node 3
                  Stack(Push(12))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(69))
                  Stack(Push(0))
                  Stack(Push(3))
                  Stack(Push(3))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(6))
                  Stack(Push(6))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(3))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(5))
                  Memory(Load)
                  Alu(Add)
                  Memory(Store)
                  Stack(Push(6))
                  Memory(StoreRange)
                  Stack(Push(7))
                  Memory(Free)
                node 4
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 5
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Pred(Eq)
                node 6
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Pred(Eq)
                node 7
                  Stack(Push(1))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(4))
                  Pred(EqRange)
                node 8
                  Stack(Push(1))
                  Stack(Push(6))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(6))
                  Pred(EqRange)
            }

        "#]],
    );
}

#[test]
fn storage_access_tuples_extern() {
    let compiled_contract = &compile(
        r#"
interface Foo {
    storage {
        u: { b256, int },
        t: { b256, { int, int } },
        w: { addr: b256, inner: { x: int, int } },
    }
}

predicate Bar() {
    let addr = 0x1111111111111111111111111111111111111111111111111111111111111111;

    let foo_u = Foo@[addr]::storage::u;
    let foo_u0 = Foo@[addr]::storage::u.0;
    let foo_u1 = Foo@[addr]::storage::u.1;

    let foo_t = Foo@[addr]::storage::t;
    let foo_t0 = Foo@[addr]::storage::t.0;
    let foo_t10 = Foo@[addr]::storage::t.1.0;
    let foo_t11 = Foo@[addr]::storage::t.1.1;

    let foo_w = Foo@[addr]::storage::w;
    let foo_w0 = Foo@[addr]::storage::w.addr;
    let foo_w10 = Foo@[addr]::storage::w.inner.x;
    let foo_w11 = Foo@[addr]::storage::w.inner.1;

    constraint foo_u == { 0x0000000000000000000000000000000000000000000000000000000000000000, 0 };
    constraint foo_u0 == 0x0000000000000000000000000000000000000000000000000000000000000000;
    constraint foo_u1 == 0;
    constraint foo_t == { 0x0000000000000000000000000000000000000000000000000000000000000000, { 0, 0 } };
    constraint foo_t0 == 0x0000000000000000000000000000000000000000000000000000000000000000;
    constraint foo_t10 == 0;
    constraint foo_t11 == 0;
    constraint foo_w == { addr: 0x0000000000000000000000000000000000000000000000000000000000000000, inner: { x: 0, 0 } };
    constraint foo_w0 == 0x0000000000000000000000000000000000000000000000000000000000000000;
    constraint foo_w10 == 0;
    constraint foo_w11 == 0;
}
        "#,
    );

    check(
        &format!("{compiled_contract}"),
        expect_test::expect![[r#"
            predicate ::Bar {
                --- Nodes ---
                node 0
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(2))
                  Stack(Push(2))
                  Stack(Push(2))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRangeExtern)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 1
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(2))
                  Stack(Push(1))
                  Stack(Push(2))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRangeExtern)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 2
                  Stack(Push(6))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(2))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRangeExtern)
                  Stack(Push(2))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(4))
                  Memory(StoreRange)
                  Stack(Push(5))
                  Memory(Free)
                node 3
                  Stack(Push(12))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(2))
                  Stack(Push(3))
                  Stack(Push(0))
                  StateRead(KeyRangeExtern)
                  Stack(Push(6))
                  Stack(Push(6))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(3))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(5))
                  Memory(Load)
                  Alu(Add)
                  Memory(Store)
                  Stack(Push(6))
                  Memory(StoreRange)
                  Stack(Push(7))
                  Memory(Free)
                node 4
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1))
                  Stack(Push(2))
                  Stack(Push(2))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRangeExtern)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 5
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(2))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRangeExtern)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 6
                  Stack(Push(6))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(2))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRangeExtern)
                  Stack(Push(2))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(4))
                  Memory(StoreRange)
                  Stack(Push(5))
                  Memory(Free)
                node 7
                  Stack(Push(12))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(2))
                  Stack(Push(3))
                  Stack(Push(0))
                  StateRead(KeyRangeExtern)
                  Stack(Push(6))
                  Stack(Push(6))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(3))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(5))
                  Memory(Load)
                  Alu(Add)
                  Memory(Store)
                  Stack(Push(6))
                  Memory(StoreRange)
                  Stack(Push(7))
                  Memory(Free)
                node 8
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(2))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRangeExtern)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 9
                  Stack(Push(6))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(2))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRangeExtern)
                  Stack(Push(2))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(4))
                  Memory(StoreRange)
                  Stack(Push(5))
                  Memory(Free)
                node 10
                  Stack(Push(9))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(2))
                  Stack(Push(2))
                  Stack(Push(0))
                  StateRead(KeyRangeExtern)
                  Stack(Push(4))
                  Stack(Push(5))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(3))
                  Memory(Load)
                  Alu(Add)
                  Memory(Store)
                  Stack(Push(5))
                  Memory(StoreRange)
                  Stack(Push(6))
                  Memory(Free)
                node 11
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 12
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Pred(Eq)
                node 13
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Pred(Eq)
                node 14
                  Stack(Push(1))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(4))
                  Pred(EqRange)
                node 15
                  Stack(Push(1))
                  Stack(Push(6))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(6))
                  Pred(EqRange)
                node 16
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Pred(Eq)
                node 17
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Pred(Eq)
                node 18
                  Stack(Push(1))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(4))
                  Pred(EqRange)
                node 19
                  Stack(Push(1))
                  Stack(Push(6))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(6))
                  Pred(EqRange)
                node 20
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Pred(Eq)
                node 21
                  Stack(Push(1))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(4))
                  Pred(EqRange)
                node 22
                  Stack(Push(1))
                  Stack(Push(5))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(5))
                  Pred(EqRange)
            }

        "#]],
    );
}

#[test]
fn storage_access_arrays() {
    let compiled_contract = &compile(
        r#"
storage {
    x: int,
    v: int[2][3],
}

predicate Foo() {
    let v = storage::v;
    let v1 = storage::v[1];
    let v12 = storage::v[1][2];

    constraint v == [[0, 0, 0], [0, 0, 0]];
    constraint v1 == [0, 0, 0];
    constraint v12 == 0;
}
        "#,
    );

    check(
        &format!("{compiled_contract}"),
        expect_test::expect![[r#"
            predicate ::Foo {
                --- Nodes ---
                node 0
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(5))
                  Stack(Push(2))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 1
                  Stack(Push(9))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(3))
                  Stack(Push(2))
                  Stack(Push(3))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(6))
                  Stack(Push(3))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(3))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(5))
                  Memory(Load)
                  Alu(Add)
                  Memory(Store)
                  Stack(Push(3))
                  Memory(StoreRange)
                  Stack(Push(4))
                  Memory(Free)
                node 2
                  Stack(Push(18))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(2))
                  Stack(Push(6))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(12))
                  Stack(Push(6))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(3))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(5))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(7))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(9))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(11))
                  Memory(Load)
                  Alu(Add)
                  Memory(Store)
                  Stack(Push(6))
                  Memory(StoreRange)
                  Stack(Push(7))
                  Memory(Free)
                node 3
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 4
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Pred(Eq)
                node 5
                  Stack(Push(1))
                  Stack(Push(3))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(3))
                  Pred(EqRange)
                node 6
                  Stack(Push(1))
                  Stack(Push(6))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(6))
                  Pred(EqRange)
            }

        "#]],
    );
}

#[test]
fn storage_access_arrays_in_maps() {
    let compiled_contract = &compile(
        r#"
storage {
    x: int,
    map_to_arrays: ( int => int[3] ),
}

predicate Foo() {
    let map_to_arrays_69 = storage::map_to_arrays[69];
    let map_to_arrays_69_2 = storage::map_to_arrays[69][2];
    constraint map_to_arrays_69 == [0, 0, 0];
    constraint map_to_arrays_69_2 == 0;
}
        "#,
    );

    check(
        &format!("{compiled_contract}"),
        expect_test::expect![[r#"
            predicate ::Foo {
                --- Nodes ---
                node 0
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(69))
                  Stack(Push(2))
                  Stack(Push(3))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 1
                  Stack(Push(9))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(69))
                  Stack(Push(0))
                  Stack(Push(3))
                  Stack(Push(3))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(6))
                  Stack(Push(3))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(3))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(5))
                  Memory(Load)
                  Alu(Add)
                  Memory(Store)
                  Stack(Push(3))
                  Memory(StoreRange)
                  Stack(Push(4))
                  Memory(Free)
                node 2
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 3
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Pred(Eq)
                node 4
                  Stack(Push(1))
                  Stack(Push(3))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(3))
                  Pred(EqRange)
            }

        "#]],
    );
}

#[test]
fn storage_access_arrays_extern() {
    let compiled_contract = &compile(
        r#"
interface Foo {
    storage {
        v: int[2][3],
        map_to_arrays: ( int => int[3] ),
    }
}

predicate Bar() {
    let addr = 0xEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE;

    let foo_v = Foo@[addr]::storage::v;
    let foo_v1 = Foo@[addr]::storage::v[1];
    let foo_v12 = Foo@[addr]::storage::v[1][2];

    let foo_map_to_arrays_69 = Foo@[addr]::storage::map_to_arrays[69];
    let foo_map_to_arrays_69_1 = Foo@[addr]::storage::map_to_arrays[69][1];

    constraint foo_v == [[0, 0, 0], [0, 0, 0]];
    constraint foo_v1 == [0, 0, 0];
    constraint foo_v12 == 0;
    constraint foo_map_to_arrays_69 == [0, 0, 0];
    constraint foo_map_to_arrays_69_1 == 0;
}
        "#,
    );

    check(
        &format!("{compiled_contract}"),
        expect_test::expect![[r#"
            predicate ::Bar {
                --- Nodes ---
                node 0
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(-1229782938247303442))
                  Stack(Push(-1229782938247303442))
                  Stack(Push(-1229782938247303442))
                  Stack(Push(-1229782938247303442))
                  Stack(Push(1))
                  Stack(Push(69))
                  Stack(Push(1))
                  Stack(Push(3))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRangeExtern)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 1
                  Stack(Push(9))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(-1229782938247303442))
                  Stack(Push(-1229782938247303442))
                  Stack(Push(-1229782938247303442))
                  Stack(Push(-1229782938247303442))
                  Stack(Push(1))
                  Stack(Push(69))
                  Stack(Push(0))
                  Stack(Push(3))
                  Stack(Push(3))
                  Stack(Push(0))
                  StateRead(KeyRangeExtern)
                  Stack(Push(6))
                  Stack(Push(3))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(3))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(5))
                  Memory(Load)
                  Alu(Add)
                  Memory(Store)
                  Stack(Push(3))
                  Memory(StoreRange)
                  Stack(Push(4))
                  Memory(Free)
                node 2
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(-1229782938247303442))
                  Stack(Push(-1229782938247303442))
                  Stack(Push(-1229782938247303442))
                  Stack(Push(-1229782938247303442))
                  Stack(Push(0))
                  Stack(Push(5))
                  Stack(Push(2))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRangeExtern)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 3
                  Stack(Push(9))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(-1229782938247303442))
                  Stack(Push(-1229782938247303442))
                  Stack(Push(-1229782938247303442))
                  Stack(Push(-1229782938247303442))
                  Stack(Push(0))
                  Stack(Push(3))
                  Stack(Push(2))
                  Stack(Push(3))
                  Stack(Push(0))
                  StateRead(KeyRangeExtern)
                  Stack(Push(6))
                  Stack(Push(3))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(3))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(5))
                  Memory(Load)
                  Alu(Add)
                  Memory(Store)
                  Stack(Push(3))
                  Memory(StoreRange)
                  Stack(Push(4))
                  Memory(Free)
                node 4
                  Stack(Push(18))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(-1229782938247303442))
                  Stack(Push(-1229782938247303442))
                  Stack(Push(-1229782938247303442))
                  Stack(Push(-1229782938247303442))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(2))
                  Stack(Push(6))
                  Stack(Push(0))
                  StateRead(KeyRangeExtern)
                  Stack(Push(12))
                  Stack(Push(6))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(3))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(5))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(7))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(9))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(11))
                  Memory(Load)
                  Alu(Add)
                  Memory(Store)
                  Stack(Push(6))
                  Memory(StoreRange)
                  Stack(Push(7))
                  Memory(Free)
                node 5
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 6
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Pred(Eq)
                node 7
                  Stack(Push(1))
                  Stack(Push(3))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(3))
                  Pred(EqRange)
                node 8
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Pred(Eq)
                node 9
                  Stack(Push(1))
                  Stack(Push(3))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(3))
                  Pred(EqRange)
                node 10
                  Stack(Push(1))
                  Stack(Push(6))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(6))
                  Pred(EqRange)
            }

        "#]],
    );
}

#[test]
fn storage_access_complex_maps() {
    let compiled_contract = &compile(
        r#"
storage {
    map_in_map: (int => (b256 => int)),
    map_in_map_in_map: (int => (b256 => (int => b256))),
}

predicate Simple() {
    let map_in_map_entry = storage::map_in_map[9][0x0000000000000001000000000000000200000000000000030000000000000004];
    let map_in_map_in_map_entry = storage::map_in_map_in_map[88][0x0000000000000008000000000000000700000000000000060000000000000005][999];

    constraint map_in_map_entry' == 42;
    constraint map_in_map_in_map_entry' == 0x000000000000000F000000000000000F000000000000000F000000000000000F;
}
        "#,
    );

    check(
        &format!("{compiled_contract}"),
        expect_test::expect![[r#"
            predicate ::Simple {
                --- Nodes ---
                node 0
                  Stack(Push(6))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(88))
                  Stack(Push(8))
                  Stack(Push(7))
                  Stack(Push(6))
                  Stack(Push(5))
                  Stack(Push(999))
                  Stack(Push(7))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(4))
                  Memory(StoreRange)
                  Stack(Push(5))
                  Memory(Free)
                node 1
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(9))
                  Stack(Push(1))
                  Stack(Push(2))
                  Stack(Push(3))
                  Stack(Push(4))
                  Stack(Push(6))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 2
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 3
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(42))
                  Pred(Eq)
                node 4
                  Stack(Push(1))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(15))
                  Stack(Push(15))
                  Stack(Push(15))
                  Stack(Push(15))
                  Stack(Push(4))
                  Pred(EqRange)
            }

        "#]],
    );
}

#[test]
fn storage_external_access() {
    let compiled_contract = &compile(
        r#"
interface Extern1 {
    storage {
        x: int,
        m: (int => (bool => b256)),
    }
}

interface Extern2 {
    storage {
        w: int,
        m: (b256 => (int => bool)),
    }
}

predicate Foo() {
    let x = Extern1@[0x1233683A8F6B8AF1707FF76F40FC5EE714872F88FAEBB8F22851E93F56770128]::storage::x;
    let y = Extern1@[0x1233683A8F6B8AF1707FF76F40FC5EE714872F88FAEBB8F22851E93F56770128]::storage::m[3][true];
    let w = Extern2@[0x0C15A3534349FC710174299BA8F0347284955B35A28C01CF45A910495FA1EF2D]::storage::w;
    let z = Extern2@[0x0C15A3534349FC710174299BA8F0347284955B35A28C01CF45A910495FA1EF2D]::storage::m[0x1111111111111111111111111111111111111111111111111111111111111111][69];

    constraint x' - x == 1;
    constraint y == 0x2222222222222222222222222222222222222222222222222222222222222222;
    constraint w' - w == 3;
    constraint z' == false;
}
        "#,
    );

    check(
        &format!("{compiled_contract}"),
        expect_test::expect![[r#"
            predicate ::Foo {
                --- Nodes ---
                node 0
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(870781680972594289))
                  Stack(Push(104754439867348082))
                  Stack(Push(-8893101603254697521))
                  Stack(Push(5019561167004233517))
                  Stack(Push(1))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(69))
                  Stack(Push(6))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRangeExtern)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 1
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(870781680972594289))
                  Stack(Push(104754439867348082))
                  Stack(Push(-8893101603254697521))
                  Stack(Push(5019561167004233517))
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRangeExtern)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 2
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(870781680972594289))
                  Stack(Push(104754439867348082))
                  Stack(Push(-8893101603254697521))
                  Stack(Push(5019561167004233517))
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRangeExtern)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 3
                  Stack(Push(6))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1311506517218527985))
                  Stack(Push(8106469911493893863))
                  Stack(Push(1479203267986307314))
                  Stack(Push(2905359692873531688))
                  Stack(Push(1))
                  Stack(Push(3))
                  Stack(Push(1))
                  Stack(Push(3))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRangeExtern)
                  Stack(Push(2))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(4))
                  Memory(StoreRange)
                  Stack(Push(5))
                  Memory(Free)
                node 4
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1311506517218527985))
                  Stack(Push(8106469911493893863))
                  Stack(Push(1479203267986307314))
                  Stack(Push(2905359692873531688))
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRangeExtern)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 5
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1311506517218527985))
                  Stack(Push(8106469911493893863))
                  Stack(Push(1479203267986307314))
                  Stack(Push(2905359692873531688))
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRangeExtern)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 6
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 7
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(3))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Alu(Sub)
                  Stack(Push(3))
                  Pred(Eq)
                node 8
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Pred(Eq)
                node 9
                  Stack(Push(1))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(2459565876494606882))
                  Stack(Push(2459565876494606882))
                  Stack(Push(2459565876494606882))
                  Stack(Push(2459565876494606882))
                  Stack(Push(4))
                  Pred(EqRange)
                node 10
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(3))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Alu(Sub)
                  Stack(Push(1))
                  Pred(Eq)
            }

        "#]],
    );
}

#[test]
fn nil() {
    check(
        &format!(
            "{}",
            compile(
                r#"
storage {
    x: int,
    w: b256,
    t: { b256, int },
    a: int[2][3],
}

predicate Foo() {
    let x = storage::x;
    let w = storage::w;
    let t = storage::t;
    let a = storage::a;

    // `x` is set in the pre state db and in the solution
    constraint x != nil;
    constraint nil != x';

    // `w` is set in the pre state db but unset in the solution
    constraint w != nil;
    constraint nil == w';

    // `t` is not set in the pre state db but is set in the solution
    constraint t == nil;
    constraint t' != nil;

    // `a` is not set in the pre state db and is partially set in the solution.
    constraint a == nil;
    constraint a' != nil;
}
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::Foo {
                --- Nodes ---
                node 0
                  Stack(Push(18))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(2))
                  Stack(Push(6))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(12))
                  Stack(Push(6))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(3))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(5))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(7))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(9))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(11))
                  Memory(Load)
                  Alu(Add)
                  Memory(Store)
                  Stack(Push(6))
                  Memory(StoreRange)
                  Stack(Push(7))
                  Memory(Free)
                node 1
                  Stack(Push(18))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(2))
                  Stack(Push(6))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(12))
                  Stack(Push(6))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(3))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(5))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(7))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(9))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(11))
                  Memory(Load)
                  Alu(Add)
                  Memory(Store)
                  Stack(Push(6))
                  Memory(StoreRange)
                  Stack(Push(7))
                  Memory(Free)
                node 2
                  Stack(Push(9))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(2))
                  Stack(Push(2))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(4))
                  Stack(Push(5))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(3))
                  Memory(Load)
                  Alu(Add)
                  Memory(Store)
                  Stack(Push(5))
                  Memory(StoreRange)
                  Stack(Push(6))
                  Memory(Free)
                node 3
                  Stack(Push(9))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(2))
                  Stack(Push(2))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(4))
                  Stack(Push(5))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(3))
                  Memory(Load)
                  Alu(Add)
                  Memory(Store)
                  Stack(Push(5))
                  Memory(StoreRange)
                  Stack(Push(6))
                  Memory(Free)
                node 4
                  Stack(Push(6))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(4))
                  Memory(StoreRange)
                  Stack(Push(5))
                  Memory(Free)
                node 5
                  Stack(Push(6))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(4))
                  Memory(StoreRange)
                  Stack(Push(5))
                  Memory(Free)
                node 6
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 7
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Memory(Load)
                  Memory(Store)
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(2))
                  Memory(Free)
                node 8
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 9
                  Stack(Push(0))
                  Memory(Load)
                  Stack(Push(0))
                  Pred(Eq)
                  Pred(Not)
                node 10
                  Stack(Push(0))
                  Memory(Load)
                  Stack(Push(0))
                  Pred(Eq)
                node 11
                  Stack(Push(0))
                  Memory(Load)
                  Stack(Push(0))
                  Pred(Eq)
                  Pred(Not)
                node 12
                  Stack(Push(0))
                  Memory(Load)
                  Stack(Push(0))
                  Pred(Eq)
                node 13
                  Stack(Push(0))
                  Memory(Load)
                  Stack(Push(0))
                  Pred(Eq)
                node 14
                  Stack(Push(0))
                  Memory(Load)
                  Stack(Push(0))
                  Pred(Eq)
                  Pred(Not)
                node 15
                  Stack(Push(0))
                  Memory(Load)
                  Stack(Push(0))
                  Pred(Eq)
                  Pred(Not)
                node 16
                  Stack(Push(0))
                  Memory(Load)
                  Stack(Push(0))
                  Pred(Eq)
                  Pred(Not)
            }

        "#]],
    );
}

#[test]
fn nested_match() {
    check(
        &compile(
            r#"
union inner = x | y(int) | z;
union outer = a(inner) | b(int);

predicate test(k: int, l: outer, m: outer) {
    match l {
        outer::b(i) => {
            match m {
                outer::a(n) => {
                    if k == 0 {
                        match n {
                            inner::y(j) => {
                                constraint i == j;
                            }
                            else => {}
                        }
                    }
                }
                else => {}
            }
        }
        else => {}
    }
}
            "#,
        )
        .to_string(),
        expect_test::expect![[r#"
            predicate ::test {
                --- Nodes ---
                node 0
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 1
                  Stack(Push(1))
                  Stack(Push(52))
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(1))
                  Pred(Eq)
                  Pred(Not)
                  TotalControlFlow(JumpForwardIf)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(41))
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(0))
                  Pred(Eq)
                  Pred(Not)
                  TotalControlFlow(JumpForwardIf)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(30))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(0))
                  Pred(Eq)
                  Pred(Not)
                  TotalControlFlow(JumpForwardIf)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(17))
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(1))
                  Alu(Add)
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(1))
                  Pred(Eq)
                  Pred(Not)
                  TotalControlFlow(JumpForwardIf)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Alu(Add)
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(1))
                  Alu(Add)
                  Stack(Push(1))
                  Alu(Add)
                  Stack(Push(1))
                  Access(PredicateData)
                  Pred(Eq)
            }

        "#]],
    );
}

// Disabled until we implement program graphs and then do ASM gen for maps.
//
// #[test]
// fn map_fixed_a() {
//     check(
//         &compile(
//             r#"
// predicate test(ary: int[10]) {
//     let evens: int[10] = map x in ary {
//         x % 2 == 1 ? x + 1 : x
//     };
// }
// "#,
//         )
//         .to_string(),
//         expect_test::expect![[r#"
//             predicate ::test {
//                 --- Constraints ---
//                 constraint 0
//                   Access(MutKeys)
//                   Stack(Push(0))
//                   Pred(EqSet)
//                 --- State Reads ---
//             }
//
//         "#]],
//     );
// }
//
// #[test]
// fn map_fixed_b() {
//     check(
//         &compile(
//             r#"
// predicate test(ary: int[10]) {
//     let evens: int[10] = map x in ary {
//         x % 2 == 1 ? x + 1 : x
//     };
//
//     constraint evens[4] + evens[0] == 10;
// }
// "#,
//         )
//         .to_string(),
//         expect_test::expect![[r#"
//         "#]],
//     );
// }
//
// #[test]
// fn map_fixed_c() {
//     check(
//         &compile(
//             r#"
// predicate test(ary: int[10]) {
//     let zero_map: bool[10] = map y in ary { y == 0 };
// }
// "#,
//         )
//         .to_string(),
//         expect_test::expect![[r#"
//             predicate ::test {
//                 --- Constraints ---
//                 constraint 0
//                   Access(MutKeys)
//                   Stack(Push(0))
//                   Pred(EqSet)
//                 --- State Reads ---
//             }
//
//         "#]],
//     );
// }
//
// #[test]
// fn map_fixed_d() {
//     check(
//         &compile(
//             r#"
// predicate test(ary: int[10]) {
//     let zero_map: bool[10] = map y in ary { y == 0 };
//
//     constraint !zero_map[3] && zero_map[7];
// }
// "#,
//         )
//         .to_string(),
//         expect_test::expect![[r#"
//         "#]],
//     );
// }
