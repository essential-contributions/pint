use super::{check, compile};

#[test]
fn optional_predicate_data() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate foo(x: int?, t: { int, bool }?) {
                constraint x! == 69;
                constraint t! == { 99, true };
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::foo {
                --- Nodes ---
                node 0 (pre,leaf)
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 1 (pre,leaf)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(DupFrom)
                  Stack(Push(1))
                  Stack(DupFrom)
                  Stack(Push(2))
                  Alu(Add)
                  Stack(Push(1))
                  Access(PredicateData)
                  Pred(Not)
                  TotalControlFlow(PanicIf)
                  Stack(Push(2))
                  Access(PredicateData)
                  Stack(Push(99))
                  Stack(Push(1))
                  Stack(Push(2))
                  Pred(EqRange)
                node 2 (pre,leaf)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(DupFrom)
                  Stack(Push(1))
                  Stack(DupFrom)
                  Stack(Push(1))
                  Alu(Add)
                  Stack(Push(1))
                  Access(PredicateData)
                  Pred(Not)
                  TotalControlFlow(PanicIf)
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(69))
                  Pred(Eq)
            }

        "#]],
    );
}

#[test]
fn optional_storage() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            storage {
                x: int?,
                t: { int, bool }?
            }
            
            predicate foo() {
                constraint storage::x!! == 69;
                constraint storage::t!! == { 99, true };
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::foo {
                --- Nodes ---
                node 0 (pre,leaf)
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 1 (pre,leaf)
                  Stack(Push(1))
                  Stack(Reserve)
                  Stack(Pop)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(5))
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
                  Stack(Push(3))
                  Pred(Eq)
                  Pred(Not)
                  TotalControlFlow(PanicIf)
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(2))
                  Alu(Add)
                  Stack(Push(3))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Load)
                  Memory(Free)
                  Pred(Not)
                  TotalControlFlow(PanicIf)
                  Stack(Push(99))
                  Stack(Push(1))
                  Stack(Push(2))
                  Pred(EqRange)
                  Stack(Swap)
                  Stack(Push(1))
                  Stack(Drop)
                node 2 (pre,leaf)
                  Stack(Push(1))
                  Stack(Reserve)
                  Stack(Pop)
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(4))
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
                  Stack(Push(2))
                  Pred(Eq)
                  Pred(Not)
                  TotalControlFlow(PanicIf)
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(2))
                  Alu(Add)
                  Stack(Push(2))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Stack(Load)
                  Memory(Free)
                  Pred(Not)
                  TotalControlFlow(PanicIf)
                  Stack(Push(69))
                  Pred(Eq)
                  Stack(Swap)
                  Stack(Push(1))
                  Stack(Drop)
            }

        "#]],
    );
}
