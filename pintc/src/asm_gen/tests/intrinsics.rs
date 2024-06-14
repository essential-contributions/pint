use super::{check, compile};

#[test]
fn mut_keys_len() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            var mut_keys_len = __mut_keys_len();
            solve satisfy;
            "#,
            )
        ),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Stack(Push(0))
              Access(DecisionVar)
              Access(MutKeysLen)
              Pred(Eq)
            --- State Reads ---
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
            var key: int[5];
            var mut_keys_contains = __mut_keys_contains(key);
            solve satisfy;
            "#,
            )
        ),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Stack(Push(5))
              Access(DecisionVar)
              Stack(Push(0))
              Access(DecisionVar)
              Stack(Push(1))
              Access(DecisionVar)
              Stack(Push(2))
              Access(DecisionVar)
              Stack(Push(3))
              Access(DecisionVar)
              Stack(Push(4))
              Access(DecisionVar)
              Stack(Push(5))
              Access(MutKeysContains)
              Pred(Eq)
            --- State Reads ---
        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            var mut_keys_contains = __mut_keys_contains([69, 70, 71]);
            solve satisfy;
            "#,
            )
        ),
        expect_test::expect![[r#"
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
            var this_address = __this_address();
            solve satisfy;
            "#,
            )
        ),
        expect_test::expect![[r#"
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
            var this_set_address = __this_set_address();
            solve satisfy;
            "#,
            )
        ),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Stack(Push(0))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Access(ThisSetAddress)
              Stack(Push(4))
              Pred(EqRange)
            --- State Reads ---
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
            var this_pathway = __this_pathway();
            solve satisfy;
            "#,
            )
        ),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Stack(Push(0))
              Access(DecisionVar)
              Access(ThisPathway)
              Pred(Eq)
            --- State Reads ---
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
            var sha256 = __sha256(69);
            solve satisfy;
            "#,
            )
        ),
        expect_test::expect![[r#"
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
        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            var x: int;
            var sha256 = __sha256(x);
            solve satisfy;
            "#,
            )
        ),
        expect_test::expect![[r#"
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
        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            var x: b256;
            var y: b256;
            var sha256 = __sha256({ 1, 3, x, { y, true } });
            solve satisfy;
            "#,
            )
        ),
        expect_test::expect![[r#"
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
        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            var t: { int, int, b256, { b256, bool } };
            var sha256 = __sha256(t);
            solve satisfy;
            "#,
            )
        ),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Stack(Push(5))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(0))
              Access(DecisionVar)
              Stack(Push(1))
              Access(DecisionVar)
              Stack(Push(2))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(3))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(4))
              Access(DecisionVar)
              Stack(Push(11))
              Crypto(Sha256)
              Stack(Push(4))
              Pred(EqRange)
            --- State Reads ---
        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            var x: int;
            var y: int;
            var z: int;
            var sha256 = __sha256([[1, 2, 3], [x, y, z]]);
            solve satisfy;
            "#,
            )
        ),
        expect_test::expect![[r#"
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
        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            var z: int[3][2];
            var sha256 = __sha256(z);
            solve satisfy;
            "#,
            )
        ),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Stack(Push(6))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(0))
              Access(DecisionVar)
              Stack(Push(1))
              Access(DecisionVar)
              Stack(Push(2))
              Access(DecisionVar)
              Stack(Push(3))
              Access(DecisionVar)
              Stack(Push(4))
              Access(DecisionVar)
              Stack(Push(5))
              Access(DecisionVar)
              Stack(Push(6))
              Crypto(Sha256)
              Stack(Push(4))
              Pred(EqRange)
            --- State Reads ---
        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            var z: int[3][2];
            var sha256 = __sha256(z[1]);
            solve satisfy;
            "#,
            )
        ),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Stack(Push(6))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(2))
              Access(DecisionVar)
              Stack(Push(3))
              Access(DecisionVar)
              Stack(Push(2))
              Crypto(Sha256)
              Stack(Push(4))
              Pred(EqRange)
            --- State Reads ---
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
            var hash0: b256;
            var hash1: b256;
            var hash2: b256;
            var sig = { hash0, hash1 };
            var verify = __verify_ed25519(69, sig, hash2);
            solve satisfy;
            "#,
            )
        ),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Stack(Push(0))
              Stack(Push(12))
              Stack(Push(3))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(0))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(4))
              Pred(EqRange)
              Pred(Not)
              TotalControlFlow(JumpForwardIf)
              Stack(Pop)
              Stack(Push(4))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(1))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(4))
              Pred(EqRange)
            constraint 1
              Stack(Push(5))
              Access(DecisionVar)
              Stack(Push(69))
              Stack(Push(1))
              Stack(Push(3))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(4))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(2))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Crypto(VerifyEd25519)
              Pred(Eq)
            --- State Reads ---
        "#]],
    );

    check(
        &format!(
            "{}",
            compile(
                r#"
            var tuple = { 1, 2, [ 1, 3 ] };
            var hash0: b256;
            var hash1: b256;
            var hash2: b256;
            var sig = { hash0, hash1 };
            var verify = __verify_ed25519(tuple, sig, hash2);
            solve satisfy;
            "#,
            )
        ),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Stack(Push(0))
              Stack(Push(15))
              Stack(Push(0))
              Stack(Push(6))
              Stack(Push(0))
              Access(DecisionVar)
              Stack(Push(1))
              Pred(Eq)
              Pred(Not)
              TotalControlFlow(JumpForwardIf)
              Stack(Pop)
              Stack(Push(1))
              Access(DecisionVar)
              Stack(Push(2))
              Pred(Eq)
              Pred(Not)
              TotalControlFlow(JumpForwardIf)
              Stack(Pop)
              Stack(Push(0))
              Stack(Push(6))
              Stack(Push(2))
              Access(DecisionVar)
              Stack(Push(1))
              Pred(Eq)
              Pred(Not)
              TotalControlFlow(JumpForwardIf)
              Stack(Pop)
              Stack(Push(3))
              Access(DecisionVar)
              Stack(Push(3))
              Pred(Eq)
            constraint 1
              Stack(Push(0))
              Stack(Push(12))
              Stack(Push(7))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(4))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(4))
              Pred(EqRange)
              Pred(Not)
              TotalControlFlow(JumpForwardIf)
              Stack(Pop)
              Stack(Push(8))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(5))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(4))
              Pred(EqRange)
            constraint 2
              Stack(Push(9))
              Access(DecisionVar)
              Stack(Push(0))
              Access(DecisionVar)
              Stack(Push(1))
              Access(DecisionVar)
              Stack(Push(2))
              Access(DecisionVar)
              Stack(Push(3))
              Access(DecisionVar)
              Stack(Push(4))
              Stack(Push(7))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(8))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(6))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Crypto(VerifyEd25519)
              Pred(Eq)
            --- State Reads ---
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
            var hash0: b256;
            var hash1: b256;
            var hash2: b256;
            var sig = { hash1, hash2, 69 };
            var recovered = __recover_secp256k1(hash0, sig);
            solve satisfy;
            "#,
            )
        ),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Stack(Push(0))
              Stack(Push(6))
              Stack(Push(0))
              Stack(Push(12))
              Stack(Push(3))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(1))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(4))
              Pred(EqRange)
              Pred(Not)
              TotalControlFlow(JumpForwardIf)
              Stack(Pop)
              Stack(Push(4))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(2))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(4))
              Pred(EqRange)
              Pred(Not)
              TotalControlFlow(JumpForwardIf)
              Stack(Pop)
              Stack(Push(5))
              Access(DecisionVar)
              Stack(Push(69))
              Pred(Eq)
            constraint 1
              Stack(Push(6))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(7))
              Access(DecisionVar)
              Stack(Push(0))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(3))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(4))
              Stack(Push(0))
              Stack(Push(4))
              Access(DecisionVarRange)
              Stack(Push(5))
              Access(DecisionVar)
              Crypto(RecoverSecp256k1)
              Stack(Push(5))
              Pred(EqRange)
            --- State Reads ---
        "#]],
    );
}
