use super::{check, compile};

#[test]
fn this_address() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test(this_address: b256) {
                constraint this_address == __this_address();
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
                  Stack(Push(4))
                  Access(DecisionVar)
                  Access(ThisAddress)
                  Stack(Push(4))
                  Pred(EqRange)
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
fn this_contract_address() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test(this_contract_address: b256) {
                constraint this_contract_address == __this_contract_address();
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
                  Stack(Push(4))
                  Access(DecisionVar)
                  Access(ThisContractAddress)
                  Stack(Push(4))
                  Pred(EqRange)
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
fn this_pathway() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test(this_pathway: int) {
                constraint this_pathway == __this_pathway();
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
fn sha256() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test(sha256: b256) {
                constraint sha256 == __sha256(69);
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
                  Stack(Push(4))
                  Access(DecisionVar)
                  Stack(Push(69))
                  Stack(Push(1))
                  Stack(Push(8))
                  Alu(Mul)
                  Crypto(Sha256)
                  Stack(Push(4))
                  Pred(EqRange)
                constraint 1
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                --- State Reads ---
            }

        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test(x: int, sha256: b256) {
                constraint sha256 == __sha256(x);
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::test {
                --- Constraints ---
                constraint 0
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVar)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVar)
                  Stack(Push(1))
                  Stack(Push(8))
                  Alu(Mul)
                  Crypto(Sha256)
                  Stack(Push(4))
                  Pred(EqRange)
                constraint 1
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                --- State Reads ---
            }

        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test(x: b256, y: b256, sha256: b256) {
                constraint sha256 == __sha256({ 1, 3, x, { y, true } });
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::test {
                --- Constraints ---
                constraint 0
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVar)
                  Stack(Push(1))
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVar)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVar)
                  Stack(Push(1))
                  Stack(Push(11))
                  Stack(Push(8))
                  Alu(Mul)
                  Crypto(Sha256)
                  Stack(Push(4))
                  Pred(EqRange)
                constraint 1
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                --- State Reads ---
            }

        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test(t: { int, int, b256, { b256, bool } }, sha256: b256) {
                constraint sha256 == __sha256(t);
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::test {
                --- Constraints ---
                constraint 0
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVar)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(11))
                  Access(DecisionVar)
                  Stack(Push(11))
                  Stack(Push(8))
                  Alu(Mul)
                  Crypto(Sha256)
                  Stack(Push(4))
                  Pred(EqRange)
                constraint 1
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                --- State Reads ---
            }

        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test(x: int, y: int, z: int, sha256: b256) {
                constraint sha256 == __sha256([[1, 2, 3], [x, y, z]]);
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::test {
                --- Constraints ---
                constraint 0
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVar)
                  Stack(Push(1))
                  Stack(Push(2))
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVar)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVar)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVar)
                  Stack(Push(6))
                  Stack(Push(8))
                  Alu(Mul)
                  Crypto(Sha256)
                  Stack(Push(4))
                  Pred(EqRange)
                constraint 1
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                --- State Reads ---
            }

        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test(z: int[3][2], sha256: b256) {
                constraint sha256 == __sha256(z);
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::test {
                --- Constraints ---
                constraint 0
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVar)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(6))
                  Access(DecisionVar)
                  Stack(Push(6))
                  Stack(Push(8))
                  Alu(Mul)
                  Crypto(Sha256)
                  Stack(Push(4))
                  Pred(EqRange)
                constraint 1
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                --- State Reads ---
            }

        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test(z: int[3][2], sha256: b256) {
                constraint sha256 == __sha256(z[1]);
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::test {
                --- Constraints ---
                constraint 0
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVar)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(2))
                  Alu(Mul)
                  Alu(Add)
                  Stack(Push(2))
                  Access(DecisionVar)
                  Stack(Push(2))
                  Stack(Push(8))
                  Alu(Mul)
                  Crypto(Sha256)
                  Stack(Push(4))
                  Pred(EqRange)
                constraint 1
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                --- State Reads ---
            }

        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test(hash: b256) {
                pub var foo: int;
                pub var bar: { int, b256, int[3] };
                constraint hash == __sha256({ foo, bar });
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
                  Stack(Push(4))
                  Access(DecisionVar)
                  Access(ThisPathway)
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PubVar)
                  Access(ThisPathway)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(8))
                  Access(PubVar)
                  Stack(Push(9))
                  Stack(Push(8))
                  Alu(Mul)
                  Crypto(Sha256)
                  Stack(Push(4))
                  Pred(EqRange)
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
fn verify_ed25519() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test(hash0: b256, hash1: b256, hash2: b256, sig: {b256, b256}, verify: bool) {
                constraint sig == { hash0, hash1 };
                constraint verify == __verify_ed25519(69, sig, hash2);
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::test {
                --- Constraints ---
                constraint 0
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(8))
                  Access(DecisionVar)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVar)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVar)
                  Stack(Push(8))
                  Pred(EqRange)
                constraint 1
                  Stack(Push(4))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVar)
                  Stack(Push(69))
                  Stack(Push(8))
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(8))
                  Access(DecisionVar)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVar)
                  Crypto(VerifyEd25519)
                  Pred(Eq)
                constraint 2
                  Access(MutKeys)
                  Stack(Push(0))
                  Pred(EqSet)
                --- State Reads ---
            }

        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test(
                tuple: {int, int, int[2]}, 
                hash0: b256,
                hash1: b256,
                hash2: b256,
                sig: {b256, b256},
                verify: bool,
            ) {
                constraint tuple == { 1, 2, [ 1, 3 ] };
                constraint sig == { hash0, hash1 };
                constraint verify == __verify_ed25519(tuple, sig, hash2);
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
                  Stack(Push(4))
                  Access(DecisionVar)
                  Stack(Push(1))
                  Stack(Push(2))
                  Stack(Push(1))
                  Stack(Push(3))
                  Stack(Push(4))
                  Pred(EqRange)
                constraint 1
                  Stack(Push(4))
                  Stack(Push(0))
                  Stack(Push(8))
                  Access(DecisionVar)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVar)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVar)
                  Stack(Push(8))
                  Pred(EqRange)
                constraint 2
                  Stack(Push(5))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(DecisionVar)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVar)
                  Stack(Push(32))
                  Stack(Push(4))
                  Stack(Push(0))
                  Stack(Push(8))
                  Access(DecisionVar)
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVar)
                  Crypto(VerifyEd25519)
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
fn recover_secp256k1() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test(
                hash0: b256,
                hash1: b256,
                hash2: b256,
                sig: {b256, b256, int},
                recovered: {b256, int},
            ) {
                constraint sig == { hash1, hash2, 69 };
                constraint recovered == __recover_secp256k1(hash0, sig);
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::test {
                --- Constraints ---
                constraint 0
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(9))
                  Access(DecisionVar)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVar)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVar)
                  Stack(Push(69))
                  Stack(Push(9))
                  Pred(EqRange)
                constraint 1
                  Stack(Push(4))
                  Stack(Push(0))
                  Stack(Push(5))
                  Access(DecisionVar)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVar)
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(9))
                  Access(DecisionVar)
                  Crypto(RecoverSecp256k1)
                  Stack(Push(5))
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
