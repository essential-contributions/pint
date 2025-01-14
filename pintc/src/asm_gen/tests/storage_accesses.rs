use super::{check, compile};

#[test]
fn inlined_storage_accesses() {
    check(
        &format!(
            "{}",
            compile(
                r#"
                storage {
                    x: int,
                    y: int,
                }

                predicate foo() {
                    constraint storage::x + storage::y == storage::x' + storage::y';
                }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::foo {
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
                  Stack(Push(1))
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
                node 2 (leaf)
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 3 (leaf)
                  Stack(Push(12))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(4))
                  StateRead(KeyRange)
                  Stack(Push(6))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(10))
                  StateRead(KeyRange)
                  Stack(Push(12))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Alu(Add)
                  Stack(Push(1))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(3))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Alu(Add)
                  Pred(Eq)
            }

        "#]],
    );
}
