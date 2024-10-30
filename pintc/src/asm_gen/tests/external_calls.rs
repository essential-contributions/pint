use super::{check, compile};

#[test]
fn local_predicate_calls() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate Foo(x: int) {
            }
            predicate Bar(foo_x: int) {
                constraint Foo[[]](foo_x);
                constraint foo_x == 0;
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
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVar)
                  Access(ThisContractAddress)
                  Stack(Push(-5015437933321959706))
                  Stack(Push(-4022861202447085451))
                  Stack(Push(-8440564760172310971))
                  Stack(Push(-2049596813535960749))
                  Stack(Push(80))
                  Crypto(Sha256)
                  Access(PredicateExists)
                constraint 1
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVar)
                  Stack(Push(0))
                  Pred(Eq)
                constraint 2
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                --- State Reads ---
            }

        "#]],
    );
}

#[test]
fn external_predicate_calls() {
    check(
        &format!(
            "{}",
            compile(
                r#"
interface Foo { predicate Bar (x: int, y: b256); }
predicate Bar(x: int, y: b256, bar_1: { x: int, y: b256 }, bar_2: { x: int, y: b256 }) {
    let c_addr = 0x0000000000000000000000000000000000000000000000000000000000000000;
    let p_addr = 0x1111111111111111111111111111111111111111111111111111111111111111;

    constraint Foo[[c_addr]]::Bar[[p_addr]](bar_1.x, bar_1.y);
    constraint Foo[[c_addr]]::Bar[[p_addr]](bar_2.x, bar_2.y);

    constraint x == bar_1.x;
    constraint y == bar_2.y;
}
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::Bar {
                --- Constraints ---
                constraint 0
                  Stack(Push(1))
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(0))
                  Alu(Add)
                  Stack(Push(1))
                  Access(DecisionVar)
                  Stack(Push(4))
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(1))
                  Alu(Add)
                  Stack(Push(4))
                  Access(DecisionVar)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(4))
                  Stack(Push(0))
                  Access(State)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(4))
                  Stack(Push(0))
                  Access(State)
                  Stack(Push(120))
                  Crypto(Sha256)
                  Access(PredicateExists)
                constraint 1
                  Stack(Push(1))
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(0))
                  Alu(Add)
                  Stack(Push(1))
                  Access(DecisionVar)
                  Stack(Push(4))
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(1))
                  Alu(Add)
                  Stack(Push(4))
                  Access(DecisionVar)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(4))
                  Stack(Push(0))
                  Access(State)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(4))
                  Stack(Push(0))
                  Access(State)
                  Stack(Push(120))
                  Crypto(Sha256)
                  Access(PredicateExists)
                constraint 2
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVar)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(0))
                  Alu(Add)
                  Stack(Push(1))
                  Access(DecisionVar)
                  Pred(Eq)
                constraint 3
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVar)
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(1))
                  Alu(Add)
                  Stack(Push(4))
                  Access(DecisionVar)
                  Stack(Push(4))
                  Pred(EqRange)
                constraint 4
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                --- State Reads ---
                state read 0
                  Constraint(Stack(Push(1)))
                  StateMemory(AllocSlots)
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(4)))
                  StateMemory(Store)
                  Constraint(TotalControlFlow(Halt))
                state read 1
                  Constraint(Stack(Push(1)))
                  StateMemory(AllocSlots)
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(0)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(1229782938247303441)))
                  Constraint(Stack(Push(4)))
                  StateMemory(Store)
                  Constraint(TotalControlFlow(Halt))
            }

        "#]],
    );
}
