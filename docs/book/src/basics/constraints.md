## Constraints

Constraints are the building blocks of a Pint contract. Simply put, a constraint is a statement that
starts with the keyword `constraint` followed by a **Boolean** expression:

```pint
{{#include ../../../../examples/ch_3_6.pnt:simple}}
```

The above is a simple constraint which ensures that the value of `x` is exactly 0. Every proposed
solution to this contract must set `x` to 0. Otherwise, the constraint will fail. If we have
multiple constraint statements, then all of them must be satisfied for a solution to be valid. For
example:

```pint
{{#include ../../../../examples/ch_3_6.pnt:two_constraints}}
```

In the above, every valid solution must set `y` to a value between 0 and 10.

Note that the above is actually equivalent to:

```pint
{{#include ../../../../examples/ch_3_6.pnt:anded}}
```

However, you may find it easier to structure your code into multiple separate constraints.
Otherwise, you'll find yourself with a giant constraint statement that is hard to read and maintain.

As mentioned above, the expression of a constraint statement must be of type boolean. Otherwise, a
compile error will be produced. For example:

```pint
constraint x + y
```

will result in the following error:

```console
Error: constraint expression type error
    ╭─[example.pnt:16:1]
    │
 16 │ constraint x + y;
    │ ────────┬───────
    │         ╰───────── constraint expression has unexpected type `int`
    │         │
    │         ╰───────── expecting type `bool`
────╯
Error: could not compile `example.pnt` due to previous error
```
