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
                --- Nodes ---
                node 0 (leaf)
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 1 (leaf)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Access(ThisContractAddress)
                  Stack(Push(-3992282013715905135))
                  Stack(Push(-3163611186605046901))
                  Stack(Push(3263825038635098201))
                  Stack(Push(569080937786746125))
                  Stack(Push(80))
                  Crypto(Sha256)
                  Access(PredicateExists)
                node 2 (leaf)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Access(ThisContractAddress)
                  Stack(Push(1097050218124307716))
                  Stack(Push(-689786681852912457))
                  Stack(Push(-6083956818317571560))
                  Stack(Push(4139034240085840423))
                  Stack(Push(80))
                  Crypto(Sha256)
                  Access(PredicateExists)
            }

            predicate ::B {
                --- Nodes ---
                node 0 (leaf)
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 1 (leaf)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(1))
                  Pred(Eq)
            }

            predicate ::C {
                --- Nodes ---
                node 0 (leaf)
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                node 1 (leaf)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(2))
                  Pred(Eq)
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
            @13..26: original predicate declaration here
            @58..71: predicate with identical bytecode here
            two predicates in a contract cannot have the exact same (optimized) bytecode
            identical predicates found in the same contract
            @13..26: original predicate declaration here
            @103..116: predicate with identical bytecode here
            two predicates in a contract cannot have the exact same (optimized) bytecode
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
            @11..26: original predicate declaration here
            @92..108: predicate with identical bytecode here
            two predicates in a contract cannot have the exact same (optimized) bytecode
        "#]],
    );
}
