use super::{check, compile};

#[test]
fn local_pub_var() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test(a: int, b: b256, c: bool) {
                pub var x: int;
                pub var y: b256;
                pub var z: bool;
                constraint a == x;
                constraint b == y;
                constraint c == z;
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::test {
                --- Constraints ---
                constraint 0
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVar)
                  Access(ThisPathway)
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PubVar)
                  Pred(Eq)
                constraint 1
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVar)
                  Access(ThisPathway)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(PubVar)
                  Stack(Push(4))
                  Pred(EqRange)
                constraint 2
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVar)
                  Access(ThisPathway)
                  Stack(Push(2))
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PubVar)
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
fn extern_pub_var() {
    check(
        &format!(
            "{}",
            compile(
                r#"
interface Foo { predicate Bar { pub var x: int; pub var y: b256;} }
predicate Bar(x: int, y: b256) {
    interface FooInstance = Foo(0x0000000000000000000000000000000000000000000000000000000000000000);
    predicate BarInstance1 = FooInstance::Bar(0x1111111111111111111111111111111111111111111111111111111111111111);
    predicate BarInstance2 = FooInstance::Bar(0x1111111111111111111111111111111111111111111111111111111111111111);
    constraint x == BarInstance1::x;
    constraint y == BarInstance2::y;
}
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::Bar {
                --- Constraints ---
                constraint 0
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVar)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVar)
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PubVar)
                  Pred(Eq)
                constraint 1
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVar)
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVar)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(PubVar)
                  Stack(Push(4))
                  Pred(EqRange)
                constraint 2
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                constraint 3
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVar)
                  Access(PredicateAt)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(8))
                  Pred(EqRange)
                constraint 4
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVar)
                  Access(PredicateAt)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(8))
                  Pred(EqRange)
                --- State Reads ---
            }

        "#]],
    );
}

#[test]
fn sibling_predicates() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate Foo() {
                pub var x: int;
            }
            predicate Bar() {
                predicate FooI = Foo();
                constraint FooI::x == 0;
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::Foo {
                --- Constraints ---
                constraint 0
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                --- State Reads ---
            }

            predicate ::Bar {
                --- Constraints ---
                constraint 0
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVar)
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PubVar)
                  Stack(Push(0))
                  Pred(Eq)
                constraint 1
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                constraint 2
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVar)
                  Access(PredicateAt)
                  Access(ThisContractAddress)
                  Stack(Push(-5015437933321959706))
                  Stack(Push(-4022861202447085451))
                  Stack(Push(-8440564760172310971))
                  Stack(Push(-2049596813535960749))
                  Stack(Push(8))
                  Pred(EqRange)
                --- State Reads ---
            }

        "#]],
    );
}
