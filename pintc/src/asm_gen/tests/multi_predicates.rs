use super::{check, compile, compile_with_error};

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
            predicate A(x: int) {
                constraint B@[](x);
                constraint C@[](x);
            }
            predicate B(x: int) {
                constraint x == 1;
            }
            predicate C(x: int) {
                constraint x == 2;
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::A {
                --- Constraints ---
                constraint 0
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVar)
                  Access(ThisContractAddress)
                  Stack(Push(-7172445384416157795))
                  Stack(Push(8030251082724392970))
                  Stack(Push(-5296280123301877640))
                  Stack(Push(6993252154795660112))
                  Stack(Push(80))
                  Crypto(Sha256)
                  Access(PredicateExists)
                constraint 1
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVar)
                  Access(ThisContractAddress)
                  Stack(Push(-3487102877995749633))
                  Stack(Push(2456710878984127610))
                  Stack(Push(-4612594924742728332))
                  Stack(Push(6890225921685255628))
                  Stack(Push(80))
                  Crypto(Sha256)
                  Access(PredicateExists)
                constraint 2
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                --- State Reads ---
            }

            predicate ::B {
                --- Constraints ---
                constraint 0
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVar)
                  Stack(Push(1))
                  Pred(Eq)
                constraint 1
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                --- State Reads ---
            }

            predicate ::C {
                --- Constraints ---
                constraint 0
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVar)
                  Stack(Push(2))
                  Pred(Eq)
                constraint 1
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                --- State Reads ---
            }

        "#]],
    );
}

#[test]
fn identical_empty_predicates() {
    // Predicates with identical constraints should fail to compile
    check(
        &compile_with_error(
            r#"
            predicate Foo() {
            }

            predicate Bar() {
            }

            predicate Cab() {
            }
            "#,
        ),
        expect_test::expect![[r#"
            identical predicates found in the same contract
            @13..26: original declaration here
            @58..71: identical constraints here
            two predicates in a contract cannot have the exact same bytecode
            identical predicates found in the same contract
            @13..26: original declaration here
            @103..116: identical constraints here
            two predicates in a contract cannot have the exact same bytecode
        "#]],
    );
}

#[test]
fn identical_predicates() {
    // Predicates with identical constraints should fail to compile
    check(
        &compile_with_error(
            r#"
          predicate Beast(x: int) {
              constraint x > 2;
          }

          predicate Animal(x: int) {
              constraint x > 2;
          }
          "#,
        ),
        expect_test::expect![[r#"
            identical predicates found in the same contract
            @11..26: original declaration here
            @92..108: identical constraints here
            two predicates in a contract cannot have the exact same bytecode
        "#]],
    );
}
