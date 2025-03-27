use super::{check, compile};

#[test]
fn map_fixed_a() {
    check(
        &compile(
            r#"
predicate test(ary: int[10]) {
    let evens: int[10] = map x in ary {
        x % 2 == 1 ? x + 1 : x
    };

    constraint evens[4] + evens[0] == 10;
}
"#,
        )
        .to_string(),
        expect_test::expect![[r#"
            predicate ::test {
                --- Nodes ---
                node 0 ()
                  Stack(Push(3))
                  Stack(Reserve)
                  Stack(Pop)
                  Stack(Push(10))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(10))
                  Stack(Push(2))
                  Stack(Store)
                  Stack(Push(1))
                  Stack(Store)
                  Stack(Push(0))
                  Stack(Store)
                  Stack(Push(2))
                  Stack(Load)
                  Stack(Push(1))
                  Stack(Repeat)
                  Stack(Push(0))
                  Stack(Load)
                  Access(RepeatCounter)
                  Stack(Push(1))
                  Stack(Load)
                  Alu(Add)
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(0))
                  Stack(Load)
                  Access(RepeatCounter)
                  Stack(Push(1))
                  Stack(Load)
                  Alu(Add)
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(1))
                  Alu(Add)
                  Stack(Push(0))
                  Stack(Load)
                  Access(RepeatCounter)
                  Stack(Push(1))
                  Stack(Load)
                  Alu(Add)
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(2))
                  Alu(Mod)
                  Stack(Push(1))
                  Pred(Eq)
                  Stack(Select)
                  Stack(RepeatEnd)
                  Stack(Push(2))
                  Stack(Load)
                  Stack(Dup)
                  Memory(Alloc)
                  Stack(Dup)
                  Stack(Push(0))
                  Stack(Store)
                  Memory(StoreRange)
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(10))
                  Memory(LoadRange)
                  Stack(Push(10))
                  Stack(Push(0))
                  Memory(StoreRange)
                  Stack(Push(3))
                  Stack(Drop)
                  Stack(Push(10))
                  Memory(Free)
                node 1 (,leaf)
                  Stack(Push(0))
                  Stack(Push(4))
                  Alu(Add)
                  Memory(Load)
                  Stack(Push(0))
                  Stack(Push(0))
                  Alu(Add)
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(10))
                  Pred(Eq)
            }

        "#]],
    );
}

#[test]
fn map_fixed_b() {
    check(
        &compile(
            r#"
predicate test(ary: int[10]) {
    let zero_map: bool[10] = map y in ary { y == 0 };

    constraint !zero_map[3] && zero_map[7];
}
"#,
        )
        .to_string(),
        expect_test::expect![[r#"
            predicate ::test {
                --- Nodes ---
                node 0 ()
                  Stack(Push(3))
                  Stack(Reserve)
                  Stack(Pop)
                  Stack(Push(10))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(10))
                  Stack(Push(2))
                  Stack(Store)
                  Stack(Push(1))
                  Stack(Store)
                  Stack(Push(0))
                  Stack(Store)
                  Stack(Push(2))
                  Stack(Load)
                  Stack(Push(1))
                  Stack(Repeat)
                  Stack(Push(0))
                  Stack(Load)
                  Access(RepeatCounter)
                  Stack(Push(1))
                  Stack(Load)
                  Alu(Add)
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(0))
                  Pred(Eq)
                  Stack(RepeatEnd)
                  Stack(Push(2))
                  Stack(Load)
                  Stack(Dup)
                  Memory(Alloc)
                  Stack(Dup)
                  Stack(Push(0))
                  Stack(Store)
                  Memory(StoreRange)
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(10))
                  Memory(LoadRange)
                  Stack(Push(10))
                  Stack(Push(0))
                  Memory(StoreRange)
                  Stack(Push(3))
                  Stack(Drop)
                  Stack(Push(10))
                  Memory(Free)
                node 1 (,leaf)
                  Stack(Push(0))
                  Stack(Push(6))
                  Stack(Push(0))
                  Stack(Push(3))
                  Alu(Add)
                  Memory(Load)
                  Pred(Not)
                  Pred(Not)
                  TotalControlFlow(JumpIf)
                  Stack(Pop)
                  Stack(Push(0))
                  Stack(Push(7))
                  Alu(Add)
                  Memory(Load)
            }

        "#]],
    );
}

#[test]
fn map_fixed_c() {
    check(
        &compile(
            r#"
predicate test() {
    let ary = [5, 4, 3, 2, 1];
    let dbls = map x in ary { x + x };
    constraint dbls[1] == 8;
}
"#,
        )
        .to_string(),
        expect_test::expect![[r#"
            predicate ::test {
                --- Nodes ---
                node 0 ()
                  Stack(Push(3))
                  Stack(Reserve)
                  Stack(Pop)
                  Stack(Push(5))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(5))
                  Stack(Push(4))
                  Stack(Push(3))
                  Stack(Push(2))
                  Stack(Push(1))
                  Stack(Push(5))
                  Stack(Push(2))
                  Stack(Store)
                  Stack(Push(5))
                  Stack(Dup)
                  Memory(Alloc)
                  Stack(Dup)
                  Stack(Push(0))
                  Stack(Store)
                  Memory(StoreRange)
                  Stack(Push(2))
                  Stack(Load)
                  Stack(Push(1))
                  Stack(Repeat)
                  Stack(Push(0))
                  Stack(Load)
                  Access(RepeatCounter)
                  Alu(Add)
                  Memory(Load)
                  Stack(Push(0))
                  Stack(Load)
                  Access(RepeatCounter)
                  Alu(Add)
                  Memory(Load)
                  Alu(Add)
                  Stack(RepeatEnd)
                  Stack(Push(2))
                  Stack(Load)
                  Stack(Dup)
                  Memory(Alloc)
                  Stack(Dup)
                  Stack(Push(0))
                  Stack(Store)
                  Memory(StoreRange)
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(5))
                  Memory(LoadRange)
                  Stack(Push(5))
                  Stack(Push(0))
                  Memory(StoreRange)
                  Stack(Push(3))
                  Stack(Drop)
                  Stack(Push(5))
                  Memory(Free)
                node 1 (,leaf)
                  Stack(Push(0))
                  Stack(Push(1))
                  Alu(Add)
                  Memory(Load)
                  Stack(Push(8))
                  Pred(Eq)
            }

        "#]],
    );
}

#[test]
fn map_fixed_d() {
    check(
        &compile(
            r#"
storage {
    ary: int[3],
}

predicate test() {
    let a = storage::ary!;
    let b = map x in a { x * x };

    constraint b[0] + b[1] == b[2];
}
"#,
        )
        .to_string(),
        expect_test::expect![[r#"
            predicate ::test {
                --- Nodes ---
                node 0 ()
                  Stack(Push(3))
                  Stack(Reserve)
                  Stack(Pop)
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(2))
                  Stack(Push(9))
                  Memory(Alloc)
                  Stack(Push(0))
                  Stack(Store)
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Load)
                  StateRead(KeyRange)
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(6))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Alu(Add)
                  Stack(Swap)
                  Stack(Pop)
                  Alu(Add)
                  Stack(Swap)
                  Stack(Pop)
                  Alu(Add)
                  Stack(Swap)
                  Stack(Pop)
                  Stack(Push(3))
                  Pred(Eq)
                  Pred(Not)
                  TotalControlFlow(PanicIf)
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(6))
                  Alu(Add)
                  Stack(Push(3))
                  Memory(LoadRange)
                  Stack(Push(3))
                  Stack(Push(0))
                  Memory(StoreRange)
                  Stack(Push(3))
                  Stack(Drop)
                  Stack(Push(3))
                  Memory(Free)
                node 1 ()
                  Stack(Push(3))
                  Stack(Reserve)
                  Stack(Pop)
                  Stack(Push(0))
                  Stack(Push(3))
                  Stack(Push(2))
                  Stack(Store)
                  Stack(Push(0))
                  Stack(Store)
                  Stack(Push(2))
                  Stack(Load)
                  Stack(Push(1))
                  Stack(Repeat)
                  Stack(Push(0))
                  Stack(Load)
                  Access(RepeatCounter)
                  Alu(Add)
                  Memory(Load)
                  Stack(Push(0))
                  Stack(Load)
                  Access(RepeatCounter)
                  Alu(Add)
                  Memory(Load)
                  Alu(Mul)
                  Stack(RepeatEnd)
                  Stack(Push(2))
                  Stack(Load)
                  Stack(Dup)
                  Memory(Alloc)
                  Stack(Dup)
                  Stack(Push(0))
                  Stack(Store)
                  Memory(StoreRange)
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(3))
                  Memory(LoadRange)
                  Stack(Push(3))
                  Stack(Push(0))
                  Memory(StoreRange)
                  Stack(Push(3))
                  Stack(Drop)
                  Stack(Push(3))
                  Memory(Free)
                node 2 (,leaf)
                  Stack(Push(0))
                  Stack(Push(0))
                  Alu(Add)
                  Memory(Load)
                  Stack(Push(0))
                  Stack(Push(1))
                  Alu(Add)
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(0))
                  Stack(Push(2))
                  Alu(Add)
                  Memory(Load)
                  Pred(Eq)
            }

        "#]],
    );
}

#[test]
fn map_fixed_e() {
    check(
        &compile(
            r#"
storage {
    ary: int[3],
}

predicate test() {
    let a = map x in storage::ary! { x * x };

    constraint a[0] + a[1] == a[2];
}
"#,
        )
        .to_string(),
        expect_test::expect![[r#"
            predicate ::test {
                --- Nodes ---
                node 0 ()
                  Stack(Push(3))
                  Stack(Reserve)
                  Stack(Pop)
                  Stack(Push(3))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(2))
                  Stack(Push(9))
                  Memory(Alloc)
                  Stack(Push(0))
                  Stack(Store)
                  Stack(Push(3))
                  Stack(Push(0))
                  Stack(Load)
                  StateRead(KeyRange)
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(6))
                  Memory(LoadRange)
                  Stack(Push(0))
                  Alu(Add)
                  Stack(Swap)
                  Stack(Pop)
                  Alu(Add)
                  Stack(Swap)
                  Stack(Pop)
                  Alu(Add)
                  Stack(Swap)
                  Stack(Pop)
                  Stack(Push(3))
                  Pred(Eq)
                  Pred(Not)
                  TotalControlFlow(PanicIf)
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(6))
                  Alu(Add)
                  Stack(Push(3))
                  Memory(LoadRange)
                  Stack(Push(3))
                  Stack(Push(2))
                  Stack(Store)
                  Stack(Push(3))
                  Stack(Dup)
                  Memory(Alloc)
                  Stack(Dup)
                  Stack(Push(0))
                  Stack(Store)
                  Memory(StoreRange)
                  Stack(Push(2))
                  Stack(Load)
                  Stack(Push(1))
                  Stack(Repeat)
                  Stack(Push(0))
                  Stack(Load)
                  Access(RepeatCounter)
                  Alu(Add)
                  Memory(Load)
                  Stack(Push(0))
                  Stack(Load)
                  Access(RepeatCounter)
                  Alu(Add)
                  Memory(Load)
                  Alu(Mul)
                  Stack(RepeatEnd)
                  Stack(Push(2))
                  Stack(Load)
                  Stack(Dup)
                  Memory(Alloc)
                  Stack(Dup)
                  Stack(Push(0))
                  Stack(Store)
                  Memory(StoreRange)
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(3))
                  Memory(LoadRange)
                  Stack(Push(3))
                  Stack(Push(0))
                  Memory(StoreRange)
                  Stack(Push(3))
                  Stack(Drop)
                  Stack(Push(3))
                  Memory(Free)
                node 1 (,leaf)
                  Stack(Push(0))
                  Stack(Push(0))
                  Alu(Add)
                  Memory(Load)
                  Stack(Push(0))
                  Stack(Push(1))
                  Alu(Add)
                  Memory(Load)
                  Alu(Add)
                  Stack(Push(0))
                  Stack(Push(2))
                  Alu(Add)
                  Memory(Load)
                  Pred(Eq)
            }

        "#]],
    );
}

#[test]
fn map_fixed_f() {
    check(
        &compile(
            r#"
predicate test_int_array(ary: int[5][2]) {
    let sums = map x in ary {
        x[0] + x[1]
    };

    constraint sums[4] == 11;
}
"#,
        )
        .to_string(),
        expect_test::expect![[r#"
            predicate ::test_int_array {
                --- Nodes ---
                node 0 ()
                  Stack(Push(3))
                  Stack(Reserve)
                  Stack(Pop)
                  Stack(Push(5))
                  Memory(Alloc)
                  Stack(Pop)
                  Stack(Push(0))
                  Stack(Push(0))
                  Stack(Push(5))
                  Stack(Push(2))
                  Stack(Store)
                  Stack(Push(1))
                  Stack(Store)
                  Stack(Push(0))
                  Stack(Store)
                  Stack(Push(2))
                  Stack(Load)
                  Stack(Push(1))
                  Stack(Repeat)
                  Stack(Push(0))
                  Stack(Load)
                  Access(RepeatCounter)
                  Stack(Push(2))
                  Alu(Mul)
                  Stack(Push(1))
                  Stack(Load)
                  Alu(Add)
                  Stack(Push(0))
                  Alu(Add)
                  Stack(Push(1))
                  Access(PredicateData)
                  Stack(Push(0))
                  Stack(Load)
                  Access(RepeatCounter)
                  Stack(Push(2))
                  Alu(Mul)
                  Stack(Push(1))
                  Stack(Load)
                  Alu(Add)
                  Stack(Push(1))
                  Alu(Add)
                  Stack(Push(1))
                  Access(PredicateData)
                  Alu(Add)
                  Stack(RepeatEnd)
                  Stack(Push(2))
                  Stack(Load)
                  Stack(Dup)
                  Memory(Alloc)
                  Stack(Dup)
                  Stack(Push(0))
                  Stack(Store)
                  Memory(StoreRange)
                  Stack(Push(0))
                  Stack(Load)
                  Stack(Push(5))
                  Memory(LoadRange)
                  Stack(Push(5))
                  Stack(Push(0))
                  Memory(StoreRange)
                  Stack(Push(3))
                  Stack(Drop)
                  Stack(Push(5))
                  Memory(Free)
                node 1 (,leaf)
                  Stack(Push(0))
                  Stack(Push(4))
                  Alu(Add)
                  Memory(Load)
                  Stack(Push(11))
                  Pred(Eq)
            }

        "#]],
    );
}
