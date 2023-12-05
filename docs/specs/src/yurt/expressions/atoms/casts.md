#### Cast Expressions

Cast expressions are used to cast one value to a different type. They have the following syntax:

```bnf
<cast-expr> ::= <expr> "as" <ty>
```

Executing an `as` expression casts the value on the left-hand side to the type on the right-hand side.

For example: `let x: real = 5 as real;`.

Note that there is no implicit casting in Yurt, hence the need for an explicit casting mechanism using `as`.

Any cast that does not fit an entry in the table below is a compiler error:

| Type of LHS | RHS    | Cast performed                                      |
| ----------- | ------ | --------------------------------------------------- |
| `int`       | `int`  | No-op                                               |
| `int`       | `real` | Produce the closest possible `real`                 |
| `real`      | `real` | No-op                                               |
| `enum`      | `int`  | [Enum cast](#enum-cast)                             |
| `bool`      | `int`  | [Boolean to integer cast](#boolean-to-integer-cast) |

##### Enum Cast

Casts an enum to its discriminant. For example:

```yurt
enum MyEnum = V0 | V1 | V2;

let d = MyEnum::V1 as int; // `d` is equal to `1`.
```

See [Enum Declaration Items](../../items/enums.md) for a detailed explanation of how enum discriminants are assigned.

##### Boolean to Integer Cast

- `false` casts to `0`.
- `true` casts to `1`.
