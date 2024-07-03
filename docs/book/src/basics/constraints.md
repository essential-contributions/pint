## Constraints

Constraints are the building blocks of a Pint program. Simply put, a constraint is a declaration
that starts with the keyword `constraint` followed by a **Boolean** expression:

```pint
{{#include ../../../../examples/ch_3_6.pnt:simple}}
```

The above is a simple constraint that enforce that the value of `x` is exactly 0. Every proposed
solution to this program must set `x` to 0. Otherwise, the constraint will fail. If we have multiple
constraint declarations, then all of them must be satisfied for a solution to be valid. For example:

```pint
{{#include ../../../../examples/ch_3_6.pnt:two_constraints}}
```

In the above, every valid solution must set `y` to a valid between 0 and 10.

Note that the above is actually equivalent to:

```pint
{{#include ../../../../examples/ch_3_6.pnt:anded}}
```

However, you may find it easier to structure your code into multiple separate constraints.
Otherwise, you'll find yourself with a giant constraint declaration that is hard to read and
maintain.

As mentioned above, the expression of a constraint declaration must be of type Boolean. Otherwise, a
compile error will be produced. For example:

```pint
constraint x + y
```

will result in the following error:

```console
Error: constraint expression type error
    ╭─[ch_3_6.pnt:16:1]
    │
 16 │ constraint x + y;
    │ ────────┬───────
    │         ╰───────── constraint expression has unexpected type `int`
    │         │
    │         ╰───────── expecting type `bool`
────╯
Error: could not compile `ch_3_6.pnt` due to previous error
```
