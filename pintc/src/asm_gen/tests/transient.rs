use super::{check, compile};

#[test]
fn local_transient() {
    check(
        &format!(
            "{}",
            compile(
                r#"
            pub var x: int;
            pub var y: b256;
            pub var z: bool;
            var a = x;
            var b = y;
            var c = z;
            solve satisfy;
            "#,
            )
        ),
        expect_test::expect![[r#"
            --- Constraints ---
            constraint 0
              Stack(Push(0))
              Access(DecisionVar)
              Stack(Push(0))
              Stack(Push(1))
              Access(ThisPathway)
              Access(Transient)
              Pred(Eq)
            constraint 1
              Stack(Push(1))
              Access(DecisionVar)
              Stack(Push(2))
              Access(DecisionVar)
              Stack(Push(3))
              Access(DecisionVar)
              Stack(Push(4))
              Access(DecisionVar)
              Stack(Push(1))
              Stack(Push(1))
              Access(ThisPathway)
              Access(Transient)
              Stack(Push(4))
              Pred(EqRange)
            constraint 2
              Stack(Push(5))
              Access(DecisionVar)
              Stack(Push(2))
              Stack(Push(1))
              Access(ThisPathway)
              Access(Transient)
              Pred(Eq)
            --- State Reads ---
        "#]],
    );
}

#[test]
fn extern_transient() {
    check(
        &format!(
            "{}",
            compile(
                r#"
interface Foo { intent Bar { pub var x: int; pub var y: b256;} }
intent Bar {
    interface FooInstance = Foo(0x0000000000000000000000000000000000000000000000000000000000000000);
    intent BarInstance1 = FooInstance::Bar(0x1111111111111111111111111111111111111111111111111111111111111111);
    intent BarInstance2 = FooInstance::Bar(0x1111111111111111111111111111111111111111111111111111111111111111);
    var x = BarInstance1::x;
    var y = BarInstance2::y;
}
            "#,
            )
        ),
        expect_test::expect![[r#"
            intent ::Bar {
                --- Constraints ---
                constraint 0
                  Stack(Push(2))
                  Access(DecisionVar)
                  Stack(Push(0))
                  Stack(Push(1))
                  Stack(Push(0))
                  Access(DecisionVar)
                  Access(Transient)
                  Pred(Eq)
                constraint 1
                  Stack(Push(3))
                  Access(DecisionVar)
                  Stack(Push(4))
                  Access(DecisionVar)
                  Stack(Push(5))
                  Access(DecisionVar)
                  Stack(Push(6))
                  Access(DecisionVar)
                  Stack(Push(1))
                  Stack(Push(1))
                  Stack(Push(1))
                  Access(DecisionVar)
                  Access(Transient)
                  Stack(Push(4))
                  Pred(EqRange)
                --- State Reads ---
            }

        "#]],
    );
}
