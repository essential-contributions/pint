### Tuple Type

A tuple type represents a collection of items that may have different types. Tuple types have the following syntax:

```bnf
<tuple-ty> ::= "{" ( [ <ident> ":" ] <ty> "," ... ) "}"
```

For example, in `let t: { int, real, string };`, `{ int, real, string }` is a tuple type. `{x: int, y: real, string }` is also a tuple type where some of the fields are named.

Names of tuple fields modify the type of the tuple. That is, `{ x: int }` and `{ y: int }` are different types. However they both coerce to `{ int }`.

Note that the grammar disallows empty tuple types `{ }`.
