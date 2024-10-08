#### Expression Precedence

The precedence of Pint operators and expressions is ordered as follows, going from strong to weak. Binary Operators at the same precedence level are grouped in the order given by their associativity.

| Operator                           | Associativity        |
| ---------------------------------- | -------------------- |
| Paths, intrinsic call expressions  |                      |
| Tuple field access, array indexing | left to right        |
| Unary `-`, `!`                     |                      |
| `as`                               | left to right        |
| `in`                               | left to right        |
| `*`, `/`, `%`                      | left to right        |
| Binary `+`, Binary `-`             | left to right        |
| `==`, `!=`, `<`, `>`, `<=`, `>=`   | Requires parentheses |
| `&&`                               | left to right        |
| `\|\|`                             | left to right        |
| `..`                               | Requires parentheses |
