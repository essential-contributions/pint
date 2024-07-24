use super::{check, compile};

#[test]
fn mut_keys_len() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test {
                var mut_keys_len = __mut_keys_len();
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
                  Access(MutKeysLen)
                  Pred(Eq)
                --- State Reads ---
            }

        "#]],
    );
}

#[test]
fn mut_keys_contains() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test {
                var key: int[5];
                var mut_keys_contains = __mut_keys_contains(key);
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::test {
                --- Constraints ---
                constraint 0
                  Stack(Push(1))
                  Access(DecisionVar)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(5))
                  Access(DecisionVarRange)
                  Stack(Push(5))
                  Access(MutKeysContains)
                  Pred(Eq)
                --- State Reads ---
            }

        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test {
                var mut_keys_contains = __mut_keys_contains([69, 70, 71]);
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
                  Stack(Push(69))
                  Stack(Push(70))
                  Stack(Push(71))
                  Stack(Push(3))
                  Access(MutKeysContains)
                  Pred(Eq)
                --- State Reads ---
            }

        "#]],
    );
}

#[test]
fn this_address() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test {
                var this_address = __this_address();
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
                  Access(DecisionVarRange)
                  Access(ThisAddress)
                  Stack(Push(4))
                  Pred(EqRange)
                --- State Reads ---
            }

        "#]],
    );
}

#[test]
fn this_set_address() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test {
                var this_set_address = __this_set_address();
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
                  Access(DecisionVarRange)
                  Access(ThisContractAddress)
                  Stack(Push(4))
                  Pred(EqRange)
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
            predicate test {
                var this_pathway = __this_pathway();
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
                  Access(ThisPathway)
                  Pred(Eq)
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
            predicate test {
                var sha256 = __sha256(69);
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
                  Access(DecisionVarRange)
                  Stack(Push(69))
                  Stack(Push(1))
                  Crypto(Sha256)
                  Stack(Push(4))
                  Pred(EqRange)
                --- State Reads ---
            }

        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test {
                var x: int;
                var sha256 = __sha256(x);
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
                  Access(DecisionVarRange)
                  Stack(Push(0))
                  Access(DecisionVar)
                  Stack(Push(1))
                  Crypto(Sha256)
                  Stack(Push(4))
                  Pred(EqRange)
                --- State Reads ---
            }

        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test {
                var x: b256;
                var y: b256;
                var sha256 = __sha256({ 1, 3, x, { y, true } });
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
                  Access(DecisionVarRange)
                  Stack(Push(1))
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVarRange)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVarRange)
                  Stack(Push(1))
                  Stack(Push(11))
                  Crypto(Sha256)
                  Stack(Push(4))
                  Pred(EqRange)
                --- State Reads ---
            }

        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test {
                var t: { int, int, b256, { b256, bool } };
                var sha256 = __sha256(t);
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
                  Access(DecisionVarRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(11))
                  Access(DecisionVarRange)
                  Stack(Push(11))
                  Crypto(Sha256)
                  Stack(Push(4))
                  Pred(EqRange)
                --- State Reads ---
            }

        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test {
                var x: int;
                var y: int;
                var z: int;
                var sha256 = __sha256([[1, 2, 3], [x, y, z]]);
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
                  Access(DecisionVarRange)
                  Stack(Push(1))
                  Stack(Push(2))
                  Stack(Push(3))
                  Stack(Push(0))
                  Access(DecisionVar)
                  Stack(Push(1))
                  Access(DecisionVar)
                  Stack(Push(2))
                  Access(DecisionVar)
                  Stack(Push(6))
                  Crypto(Sha256)
                  Stack(Push(4))
                  Pred(EqRange)
                --- State Reads ---
            }

        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test {
                var z: int[3][2];
                var sha256 = __sha256(z);
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
                  Access(DecisionVarRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(6))
                  Access(DecisionVarRange)
                  Stack(Push(6))
                  Crypto(Sha256)
                  Stack(Push(4))
                  Pred(EqRange)
                --- State Reads ---
            }

        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test {
                var z: int[3][2];
                var sha256 = __sha256(z[1]);
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
                  Access(DecisionVarRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(2))
                  Alu(Mul)
                  Alu(Add)
                  Stack(Push(2))
                  Access(DecisionVarRange)
                  Stack(Push(2))
                  Crypto(Sha256)
                  Stack(Push(4))
                  Pred(EqRange)
                --- State Reads ---
            }

        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test {
                pub var foo: int;
                pub var bar: { int, b256, int[3] };
                var hash = __sha256({ foo, bar });
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
                  Access(DecisionVarRange)
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(ThisPathway)
                  Access(Transient)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(2))
                  Access(ThisPathway)
                  Access(Transient)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(1))
                  Alu(Add)
                  Stack(Push(2))
                  Access(ThisPathway)
                  Access(Transient)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(2))
                  Alu(Add)
                  Stack(Push(2))
                  Access(ThisPathway)
                  Access(Transient)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(3))
                  Alu(Add)
                  Stack(Push(2))
                  Access(ThisPathway)
                  Access(Transient)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(4))
                  Alu(Add)
                  Stack(Push(2))
                  Access(ThisPathway)
                  Access(Transient)
                  Stack(Push(9))
                  Crypto(Sha256)
                  Stack(Push(4))
                  Pred(EqRange)
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
            predicate test {
                var hash0: b256;
                var hash1: b256;
                var hash2: b256;
                var sig = { hash0, hash1 };
                var verify = __verify_ed25519(69, sig, hash2);
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
                  Access(DecisionVarRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVarRange)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVarRange)
                  Stack(Push(8))
                  Pred(EqRange)
                constraint 1
                  Stack(Push(4))
                  Access(DecisionVar)
                  Stack(Push(69))
                  Stack(Push(1))
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(8))
                  Access(DecisionVarRange)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVarRange)
                  Crypto(VerifyEd25519)
                  Pred(Eq)
                --- State Reads ---
            }

        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate test {
                var tuple = { 1, 2, [ 1, 3 ] };
                var hash0: b256;
                var hash1: b256;
                var hash2: b256;
                var sig = { hash0, hash1 };
                var verify = __verify_ed25519(tuple, sig, hash2);
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
                  Access(DecisionVarRange)
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
                  Access(DecisionVarRange)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVarRange)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVarRange)
                  Stack(Push(8))
                  Pred(EqRange)
                constraint 2
                  Stack(Push(5))
                  Access(DecisionVar)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVarRange)
                  Stack(Push(4))
                  Stack(Push(4))
                  Stack(Push(0))
                  Stack(Push(8))
                  Access(DecisionVarRange)
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVarRange)
                  Crypto(VerifyEd25519)
                  Pred(Eq)
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
            predicate test {
                var hash0: b256;
                var hash1: b256;
                var hash2: b256;
                var sig = { hash1, hash2, 69 };
                var recovered = __recover_secp256k1(hash0, sig);
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
                  Access(DecisionVarRange)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVarRange)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVarRange)
                  Stack(Push(69))
                  Stack(Push(9))
                  Pred(EqRange)
                constraint 1
                  Stack(Push(4))
                  Stack(Push(0))
                  Stack(Push(5))
                  Access(DecisionVarRange)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(DecisionVarRange)
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(9))
                  Access(DecisionVarRange)
                  Crypto(RecoverSecp256k1)
                  Stack(Push(5))
                  Pred(EqRange)
                --- State Reads ---
            }

        "#]],
    );
}
