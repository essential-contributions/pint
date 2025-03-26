use super::{check, compile, compile_with_error};

#[test]
fn empty_asm_block() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate foo(y: int) {
                let x: int = asm() {};

                constraint x == y;
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::foo {
                --- Nodes ---
                node 0 ()
                node 1 (,leaf)
                  Stack(Push(0))
                  Memory(Load)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Pred(Eq)
            }

        "#]],
    );
}

#[test]
fn with_args() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate foo(y: int) {
                let x: int = asm(y, 69, y + 420, { y, y, y }) {
                };

                constraint x == y;
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::foo {
                --- Nodes ---
                node 0 ()
                  Stack(Push(1))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(1))
                  Stack(Push(0))
                  Memory(StoreRange)
                  Stack(Push(1))
                  Memory(Free)
                node 1 ()
                  Stack(Push(69))
                  Stack(Push(1))
                  Stack(Push(0))
                  Memory(StoreRange)
                  Stack(Push(1))
                  Memory(Free)
                node 2 ()
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(420))
                  Alu(Add)
                  Stack(Push(1))
                  Stack(Push(0))
                  Memory(StoreRange)
                  Stack(Push(1))
                  Memory(Free)
                node 3 ()
                  Stack(Push(2))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(3))
                  Stack(Push(0))
                  Memory(StoreRange)
                  Stack(Push(3))
                  Memory(Free)
                node 4 ()
                node 5 (,leaf)
                  Stack(Push(0))
                  Memory(Load)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Pred(Eq)
            }

        "#]],
    );
}

#[test]
fn all_ops() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            predicate foo(y: int) {
                let x: int = asm() {
                    ADD
                    ALOC
                    AND
                    BAND
                    BOR
                    DATA
                    DIV
                    DLEN
                    DSLT
                    DUP
                    DUPF
                    EQ
                    EQRA
                    EQST
                    FREE
                    GT
                    GTE
                    HLT
                    HLTIF
                    JMPIF
                    KREX
                    KRNG
                    LOD
                    LODR
                    LODS
                    LT
                    LTE
                    MKEYS
                    MOD
                    MUL
                    NOT
                    OR
                    PEX
                    PNCIF
                    POP
                    0x69
                    REP
                    REPC
                    REPE
                    RES
                    RSECP
                    SEL
                    SHA2
                    SHL
                    SHR
                    SHRI
                    SLTR
                    STO
                    STOR
                    STOS
                    SUB
                    SWAP
                    SWAPI
                    THIS
                    THISC
                    VRFYED
                };

                constraint x == y;
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::foo {
                --- Nodes ---
                node 0 ()
                  Alu(Add)
                  Memory(Alloc)
                  Pred(And)
                  Pred(BitAnd)
                  Pred(BitOr)
                  Access(PredicateData)
                  Alu(Div)
                  Access(PredicateDataLen)
                  Access(PredicateDataSlots)
                  Stack(Dup)
                  Stack(DupFrom)
                  Pred(Eq)
                  Pred(EqRange)
                  Pred(EqSet)
                  Memory(Free)
                  Pred(Gt)
                  Pred(Gte)
                  TotalControlFlow(Halt)
                  TotalControlFlow(HaltIf)
                  TotalControlFlow(JumpIf)
                  StateRead(KeyRangeExtern)
                  StateRead(KeyRange)
                  Memory(Load)
                  Memory(LoadRange)
                  Stack(Load)
                  Pred(Lt)
                  Pred(Lte)
                  Access(MutKeys)
                  Alu(Mod)
                  Alu(Mul)
                  Pred(Not)
                  Pred(Or)
                  Access(PredicateExists)
                  TotalControlFlow(PanicIf)
                  Stack(Pop)
                  Stack(Push(105))
                  Stack(Repeat)
                  Access(RepeatCounter)
                  Stack(RepeatEnd)
                  Stack(Reserve)
                  Crypto(RecoverSecp256k1)
                  Stack(Select)
                  Crypto(Sha256)
                  Alu(Shl)
                  Alu(Shr)
                  Alu(ShrI)
                  Stack(SelectRange)
                  Memory(Store)
                  Memory(StoreRange)
                  Stack(Store)
                  Alu(Sub)
                  Stack(Swap)
                  Stack(SwapIndex)
                  Access(ThisAddress)
                  Access(ThisContractAddress)
                  Crypto(VerifyEd25519)
                node 1 (,leaf)
                  Stack(Push(0))
                  Memory(Load)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Pred(Eq)
            }

        "#]],
    );
}

#[test]
fn bad_ops() {
    check(
        &compile_with_error(
            r#"
        predicate foo(y: int) {
            let x: int = asm() {
                PUSH 5
                POP
            };
            constraint x == y;
        }
          "#,
        ),
        expect_test::expect![[r#"
            bad `PUSH` instruction
            @82..86: `PUSH` is not a valid instruction in an asm block 
            try directly inserting an integer immediate instead
        "#]],
    );

    check(
        &compile_with_error(
            r#"
        predicate foo(y: int) {
            let x: int = asm() {
                LALALAND
            };
            constraint x == y;
        }
          "#,
        ),
        expect_test::expect![[r#"
            unrecognized instruction
            @82..90: this instruction is not a valid EssentialVM instruction 
        "#]],
    );
}
