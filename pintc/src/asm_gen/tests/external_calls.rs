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
                constraint Foo@[](foo_x);
                constraint foo_x == 0;
            }
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::Foo {
                --- Nodes ---
            }

            predicate ::Bar {
                --- Nodes ---
                node 0 (,leaf)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(0))
                  Pred(Eq)
                node 1 (,leaf)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Access(ThisContractAddress)
                  Stack(Push(-2360060374177730597))
                  Stack(Push(4636202025757824840))
                  Stack(Push(-1551618867720337012))
                  Stack(Push(-1727934474626789095))
                  Stack(Push(80))
                  Crypto(Sha256)
                  Access(PredicateExists)
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

    constraint Foo@[c_addr]::Bar@[p_addr](bar_1.x, bar_1.y);
    constraint Foo@[c_addr]::Bar@[p_addr](bar_2.x, bar_2.y);

    constraint x == bar_1.x;
    constraint y == bar_2.y;
}
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::Bar {
                --- Nodes ---
                node 0 (,leaf)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Push(4))
                  Access(PredicateData)
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(1))
                  Alu(Add)
                  Stack(Push(4))
                  Access(PredicateData)
                  Stack(Push(4))
                  Pred(EqRange)
                node 1 (,leaf)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Pred(Eq)
                node 2 (,leaf)
                  Stack(Push(1))
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(4))
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Push(1))
                  Alu(Add)
                  Stack(Push(4))
                  Access(PredicateData)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(120))
                  Crypto(Sha256)
                  Access(PredicateExists)
                node 3 (,leaf)
                  Stack(Push(1))
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(4))
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(1))
                  Alu(Add)
                  Stack(Push(4))
                  Access(PredicateData)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(1229782938247303441))
                  Stack(Push(120))
                  Crypto(Sha256)
                  Access(PredicateExists)
            }

        "#]],
    );
}
