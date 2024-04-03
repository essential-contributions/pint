#### Range Expressions

Range expressions are used to refer to ranges between a lower bound value and an upper bound value:

```bnf
<range-expr> ::= <expr> ".." <expr>
```

Range expressions require that both the lower bound and the upper bound expressions have the same type. The type of a range expression is identical to the type of its bounds.

The only allowed types for the bounds of a range expression are `int` and `real`.

Range expressions can only be used in two contexts:

1. As the expression on the right-hand side of a `<let-item>`.
1. As the expression on the right-hand side of an `<in-expr>`

For example,

```pint
let x: int = 3..5;
```

is equivalent to:

```pint
let x: int;
constraint x in 3..5;
```

which is equivalent to:

```pint
let x: int;
constraint x >= 3;
constraint x <= 5;
```
