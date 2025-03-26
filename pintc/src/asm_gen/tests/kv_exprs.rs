use super::{check, compile};

#[test]
fn kv_exprs_tuple() {
    check(
        &format!(
            "{}",
            compile(
                r#"
storage {
    x: { int , int, b256, int }
}

predicate foo() {
    constraint storage::x := 
        { 41, 42, 0x0000000000000005000000000000000600000000000000070000000000000008, 43 };
    constraint storage::x.3 := 43;
}
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::foo {
                --- Nodes ---
                node 0 (,leaf)
                  Stack(Push(3))
                  Stack(Reserve)
                  Stack(Pop)
                  Stack(Push(6))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(43))
                  Stack(Push(1))
                  Memory(Alloc)
                  Stack(Push(0))
                  Stack(Store)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Load)
                  Memory(StoreRange)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(3))
                  Stack(Push(0))
                  Alu(Add)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(0))
                  Alu(Add)
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(1))
                  Stack(Push(0))
                  Memory(Store)
                  Stack(Push(5))
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(3))
                  Stack(Drop)
                  Stack(Push(6))
                  Memory(Free)
                  Stack(Push(2))
                node 1 (,leaf)
                  Stack(Push(3))
                  Stack(Reserve)
                  Stack(Pop)
                  Stack(Push(24))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(41))
                  Stack(Push(42))
                  Stack(Push(5))
                  Stack(Push(6))
                  Stack(Push(7))
                  Stack(Push(8))
                  Stack(Push(43))
                  Stack(Push(7))
                  Memory(Alloc)
                  Stack(Push(0))
                  Stack(Store)
                  Stack(Push(7))
                  Stack(Push(0))
                  Stack(Load)
                  Memory(StoreRange)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Alu(Add)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(0))
                  Alu(Add)
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Alu(Add)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(1))
                  Alu(Add)
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(2))
                  Alu(Add)
                  Stack(Push(4))
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(2))
                  Alu(Add)
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(3))
                  Alu(Add)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(6))
                  Alu(Add)
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(4))
                  Stack(Push(0))
                  Memory(Store)
                  Stack(Push(23))
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(3))
                  Stack(Drop)
                  Stack(Push(24))
                  Memory(Free)
                  Stack(Push(2))
            }

        "#]],
    );
}

#[test]
fn kv_exprs_in_local_var() {
    check(
        &format!(
            "{}",
            compile(
                r#"
storage {
    x: { int , int, b256, int }
}

predicate foo() {
    let kv = storage::x := 
        { 41, 42, 0x0000000000000005000000000000000600000000000000070000000000000008, 43 };
    constraint kv;
}
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::foo {
                --- Nodes ---
                node 0 ()
                  Stack(Push(3))
                  Stack(Reserve)
                  Stack(Pop)
                  Stack(Push(24))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(41))
                  Stack(Push(42))
                  Stack(Push(5))
                  Stack(Push(6))
                  Stack(Push(7))
                  Stack(Push(8))
                  Stack(Push(43))
                  Stack(Push(7))
                  Memory(Alloc)
                  Stack(Push(0))
                  Stack(Store)
                  Stack(Push(7))
                  Stack(Push(0))
                  Stack(Load)
                  Memory(StoreRange)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Alu(Add)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(0))
                  Alu(Add)
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(1))
                  Alu(Add)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(1))
                  Alu(Add)
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(2))
                  Alu(Add)
                  Stack(Push(4))
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(2))
                  Alu(Add)
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(3))
                  Alu(Add)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(6))
                  Alu(Add)
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(4))
                  Stack(Push(24))
                  Stack(Push(0))
                  Memory(StoreRange)
                  Stack(Push(3))
                  Stack(Drop)
                  Stack(Push(24))
                  Memory(Free)
                node 1 (,leaf)
                  Stack(Push(0))
                  Stack(Push(24))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Memory(Store)
                  Stack(Push(23))
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(24))
                  Memory(Free)
                  Stack(Push(2))
            }

        "#]],
    );
}

#[test]
fn kv_exprs_concat() {
    check(
        &format!(
            "{}",
            compile(
                r#"
storage {
    x: { int , int, b256, int }
}

predicate foo() {
    constraint    storage::x.0 := 41
               ++ storage::x.1 := 42
               ++ storage::x.2 := 0x0000000000000005000000000000000600000000000000070000000000000008;
}
            "#,
            )
        ),
        expect_test::expect![[r#"
            predicate ::foo {
                --- Nodes ---
                node 0 (,leaf)
                  Stack(Push(3))
                  Stack(Reserve)
                  Stack(Pop)
                  Stack(Push(19))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(41))
                  Stack(Push(1))
                  Memory(Alloc)
                  Stack(Push(0))
                  Stack(Store)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Load)
                  Memory(StoreRange)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(0))
                  Alu(Add)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(0))
                  Alu(Add)
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Store)
                  Stack(Push(42))
                  Stack(Push(1))
                  Memory(Alloc)
                  Stack(Push(0))
                  Stack(Store)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Load)
                  Memory(StoreRange)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(0))
                  Alu(Add)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(0))
                  Alu(Add)
                  Stack(Push(1))
                  Memory(LoadRange)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Load)
                  Alu(Add)
                  Stack(Push(0))
                  Stack(Store)
                  Stack(Push(5))
                  Stack(Push(6))
                  Stack(Push(7))
                  Stack(Push(8))
                  Stack(Push(4))
                  Memory(Alloc)
                  Stack(Push(0))
                  Stack(Store)
                  Stack(Push(4))
                  Stack(Push(0))
                  Stack(Load)
                  Memory(StoreRange)
                  Stack(Push(2))
                  Stack(Push(0))
                  Stack(Push(2))
                  Stack(Push(0))
                  Alu(Add)
                  Stack(Push(4))
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(0))
                  Alu(Add)
                  Stack(Push(4))
                  Memory(LoadRange)
                  Stack(Push(1))
                  Stack(Push(0))
                  Stack(Load)
                  Alu(Add)
                  Stack(Push(0))
                  Memory(Store)
                  Stack(Push(18))
                  Stack(Push(1))
                  Memory(StoreRange)
                  Stack(Push(3))
                  Stack(Drop)
                  Stack(Push(19))
                  Memory(Free)
                  Stack(Push(2))
            }

        "#]],
    );
}
