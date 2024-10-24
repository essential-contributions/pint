use super::{check, compile};

#[test]
fn check_order() {
    // Ensure that the order in which A, B, and C are included in the final compiled contract is
    // correct and matches what the user wrote, even though the order in which the predicates
    // should be compiled is not the same (since A depends on B and C, and so, B and C should be
    // compiled to asm before A).
    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate A() {
                predicate BI = B();
                predicate CI = C();
            }
            predicate B() {
            }
            predicate C() {
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::A {
                --- Constraints ---
                constraint 0
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                constraint 1
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
                constraint 2
                  Stack(Push(1))
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

            predicate ::B {
                --- Constraints ---
                constraint 0
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                --- State Reads ---
            }

            predicate ::C {
                --- Constraints ---
                constraint 0
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                --- State Reads ---
            }

        "#]],
    );
}
