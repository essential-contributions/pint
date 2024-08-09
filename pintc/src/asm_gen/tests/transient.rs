use super::{check, compile};

#[test]
fn local_transient() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test {
                pub var x: int;
                pub var y: b256;
                pub var z: bool;
                var a = x;
                var b = y;
                var c = z;
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::test {
                --- Constraints ---
                constraint 0
                  Stack(Push(0))
                  Access(DecisionVar)
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(ThisPathway)
                  Access(Transient)
                  Pred(Eq)
                constraint 1
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVarRange)
                  Stack(Push(1))
                  Stack(Push(1))
                  Access(ThisPathway)
                  Access(Transient)
                  Stack(Push(4))
                  Pred(EqRange)
                constraint 2
                  Stack(Push(2))
                  Access(DecisionVar)
                  Stack(Push(2))
                  Stack(Push(1))
                  Access(ThisPathway)
                  Access(Transient)
                  Pred(Eq)
                constraint 3
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                --- State Reads ---
            }

        "#]],
    );
}

#[test]
fn extern_transient() {
    check(
        &format!(
            "{}",
            compile(
                r#"
interface Foo { predicate Bar { pub var x: int; pub var y: b256;} }
predicate Bar {
    interface FooInstance = Foo(0x0000000000000000000000000000000000000000000000000000000000000000);
    predicate BarInstance1 = FooInstance::Bar(0x1111111111111111111111111111111111111111111111111111111111111111);
    predicate BarInstance2 = FooInstance::Bar(0x1111111111111111111111111111111111111111111111111111111111111111);
    var x = BarInstance1::x;
    var y = BarInstance2::y;
}
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::Bar {
                --- Constraints ---
                constraint 0
                  Stack(Push(2))
                  Access(DecisionVar)
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(0))
                  Access(DecisionVar)
                  Access(Transient)
                  Pred(Eq)
                constraint 1
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVarRange)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(1))
                  Access(DecisionVar)
                  Access(Transient)
                  Stack(Push(4))
                  Pred(EqRange)
                constraint 2
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                --- State Reads ---
            }

        "#]],
    );
}
