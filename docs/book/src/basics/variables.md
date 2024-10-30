## Local Variables

Local variables in Pint are similar to local variables in other languages with a few restrictions.
In Pint, variables **must** be initialized and are **immutable**. They basically hold values and
help you write readable code, but are not meant to be modified. This is an important property that
allows Pint to be fully declarative with _no control flow_. In Pint, the order in which you write
your statements (variable declarations, constraints, etc.) is completely irrelevant and has no
impact on the behavior of the code.

In order to declare a new variable, we use the `let` keyword:

```pint
{{#include ../../../../examples/variables.pnt:basic_let}}
```

The first `let` declares a variable named `x` and assigns its value to `5`. The second `let`
declares a variable named `y` and assigns its value to `6`. The constraint `y - x == 1` references
`x` and `y` using their names and is obviously always `true` in this case since `6 - 5 == 1`.

Note that, while `y` is annotated with type `int`, we opted not annotate `x` with a type; we're
relying on the compiler to _infer_ its type to be `int` since the initializing expression is `5`
which is an `int`.

If we were to declare `y` as follows:

```pint
let y: int = true;
```

then the compiler would emit an error because the type annotation `int` and the type of the
initializing expressions `true` do not match:

```console
Error: variable initialization type error
   ╭─[variables.pnt:4:18]
   │
 4 │     let y: int = true;
   │            ─┬─   ──┬─
   │             ╰────────── expecting type `int`
   │                    │
   │                    ╰─── initializing expression has unexpected type `bool`
───╯
```

### Shadowing

Name shadowing is not allowed in Pint. Declaring two variables with the same name will result in a
compiler error. For example, the following code fails to compile:

```pint
let y = 5;
let y: int = 6;
```

and the following error is emitted:

```console
Error: symbol `y` has already been declared
   ╭─[variables.pnt:4:9]
   │
 3 │     let y = 5;
   │     ────┬────
   │         ╰────── previous declaration of the symbol `y` here
 4 │     let y: int = 6;
   │         ┬
   │         ╰── `y` redeclared here
   │
   │ Note: `y` must be declared or imported only once in this scope
───╯
```
