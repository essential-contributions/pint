#### "In" Expressions

"In" expressions are used to detect whether a value belongs to an [array](./arrays.md) or [range](./ranges.md). They have the following syntax:

```bnf
<in-expr> ::= <expr> "in" <expr>
```

An "in" expression returns a `bool` that indicates whether the left-hand side "belongs" to the right-hand side. For example:

- In `let x: bool = 5 in [3, 4, 5];`, `x` should be `true`.
- In, `let y: bool = 2 in 3..5;`, `x` should be `false`.

The right-hand side of an "in" expression must be an array or a range and the type of the left-hand side must match the type of the array elements or the range bounds. Otherwise, a compiler error should be emitted.

A value **belongs to an array** if and only if it is "equal" to one of its entries. Equality for various types is defined as follows:

| Type     | Equality Criterion                                       |
| -------- | -------------------------------------------------------- |
| `int`    | Identical values                                         |
| `real`   | Identical values                                         |
| `bool`   | Identical values                                         |
| `string` | Identical lengths and characters in the same order       |
| `enum`   | Identical variants                                       |
| tuple    | Identical lengths and _equal_ elements in the same order |
| array    | Identical lengths and _equal_ elements in the same order |

A value **belongs to a range** if and only if it is within the bounds of the range. This is only valid and well-defined, by the rules of `<=` and `>=`, for integer (`int`) and real (`real`) values, since ranges of any other type are not supported.

> **Note:** two values of different types cannot be compared (both equality and inequality) and should result in a compile error.
