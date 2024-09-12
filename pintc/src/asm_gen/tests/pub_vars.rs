use super::{check, compile};

#[test]
fn local_pub_var() {
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
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVarRange)
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
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVarRange)
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
fn extern_pub_var() {
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
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVarRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVarRange)
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
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVarRange)
                  Access(Transient)
                  Stack(Push(4))
                  Pred(EqRange)
                constraint 2
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                constraint 3
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVarRange)
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
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVarRange)
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
            predicate Foo {
                pub var x: int;
            }
            predicate Bar {
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
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVarRange)
                  Access(Transient)
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
                  Access(DecisionVarRange)
                  Access(PredicateAt)
                  Access(ThisContractAddress)
                  Stack(Push(4276813116036094451))
                  Stack(Push(-532024731742872239))
                  Stack(Push(5138777131991909664))
                  Stack(Push(-6622441995027729991))
                  Stack(Push(8))
                  Pred(EqRange)
                --- State Reads ---
            }

        "#]],
    );
}
