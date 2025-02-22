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
                    constraint storage::x! + storage::y! == storage::x'! + storage::y'!;
                }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::foo {
                --- Nodes ---
                node 0 (post)
                  Stack(Push(4))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(2))
                  Stack(Reserve)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(1))
                  Memory(Load)
                  Stack(Push(1))
                  Pred(Eq)
                  Stack(Push(2))
                  Memory(StoreRange)
                  Stack(Pop)
                  Stack(Pop)
                  Stack(Push(2))
                  Memory(Free)
                node 1 (post)
                  Stack(Push(4))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(2))
                  Stack(Reserve)
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(0))
                  StateRead(KeyRange)
                  Stack(Push(2))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(1))
                  Memory(Load)
                  Stack(Push(1))
                  Pred(Eq)
                  Stack(Push(2))
                  Memory(StoreRange)
                  Stack(Pop)
                  Stack(Pop)
                  Stack(Push(2))
                  Memory(Free)
                node 2 (pre,leaf)
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 3 (pre,leaf)
                  Stack(Push(16))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(12))
                  StateRead(KeyRange)
                  Stack(Push(13))
                  Memory(Load)
                  Stack(Push(1))
                  Pred(Eq)
                  Pred(Not)
                  TotalControlFlow(PanicIf)
                  Stack(Push(14))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(8))
                  StateRead(KeyRange)
                  Stack(Push(9))
                  Memory(Load)
                  Stack(Push(1))
                  Pred(Eq)
                  Pred(Not)
                  TotalControlFlow(PanicIf)
                  Stack(Push(10))
                  Stack(Push(1))
                  Memory(LoadRange)
                  Alu(Add)
                  Stack(Push(0))
                  Stack(Dup)
                  Stack(Push(1))
                  Alu(Add)
                  Memory(Load)
                  Pred(Not)
                  TotalControlFlow(PanicIf)
                  Memory(Load)
                  Stack(Push(2))
                  Stack(Dup)
                  Stack(Push(1))
                  Alu(Add)
                  Memory(Load)
                  Pred(Not)
                  TotalControlFlow(PanicIf)
                  Memory(Load)
                  Alu(Add)
                  Pred(Eq)
            }

        "#]],
    );
}
