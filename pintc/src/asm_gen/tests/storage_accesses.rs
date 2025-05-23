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
                node 0 (,leaf)
                  Stack(Push(3))
                  Stack(Reserve)
                  Stack(Pop)
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Push(0))
                  Stack(Store)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Load)
                  StateRead(KeyRange)
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(1))
                  Alu(Add)
                  Memory(Load)
                  Stack(Push(1))
                  Pred(Eq)
                  Pred(Not)
                  TotalControlFlow(PanicIf)
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(2))
                  Alu(Add)
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Push(0))
                  Stack(Store)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Load)
                  StateRead(KeyRange)
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(1))
                  Alu(Add)
                  Memory(Load)
                  Stack(Push(1))
                  Pred(Eq)
                  Pred(Not)
                  TotalControlFlow(PanicIf)
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(2))
                  Alu(Add)
                  Stack(Push(1))
                  Memory(LoadRange)
                  Alu(Add)
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Push(0))
                  Stack(Store)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Load)
                  StateRead(PostKeyRange)
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(1))
                  Alu(Add)
                  Memory(Load)
                  Stack(Push(1))
                  Pred(Eq)
                  Pred(Not)
                  TotalControlFlow(PanicIf)
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(2))
                  Alu(Add)
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Push(0))
                  Stack(Store)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Load)
                  StateRead(PostKeyRange)
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(1))
                  Alu(Add)
                  Memory(Load)
                  Stack(Push(1))
                  Pred(Eq)
                  Pred(Not)
                  TotalControlFlow(PanicIf)
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(2))
                  Alu(Add)
                  Stack(Push(1))
                  Memory(LoadRange)
                  Alu(Add)
                  Pred(Eq)
                  Stack(Push(0))
                  Stack(Store)
                  Stack(Pop)
                  Stack(Pop)
            }

        "#]],
    );
}
