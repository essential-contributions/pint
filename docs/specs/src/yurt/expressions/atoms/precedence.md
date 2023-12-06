#### Expression Precedence

The precedence of Yurt operators and expressions is ordered as follows, going from strong to weak. Binary Operators at the same precedence level are grouped in the order given by their associativity.

| Operator                         | Associativity        |
| -------------------------------- | -------------------- |
| Paths                            |                      |
| Tuple field access expressions   | left to right        |
| Call expressions, array indexing |                      |
| Unary `-`, `!`                   |                      |
| `as`                             | left to right        |
| `in`                             | left to right        |
| `*`, `/`, `%`                    | left to right        |
| Binary `+`, Binary `-`           | left to right        |
| `==`, `!=`, `<`, `>`, `<=`, `>=` | Requires parentheses |
| `&&`                             | left to right        |
| `\|\|`                           | left to right        |
| `..`                             | Requires parentheses |
