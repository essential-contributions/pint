#### Tuple Expressions and Tuple Field Access Expressions

Tuple expressions are written as:

```bnf
<tuple-expr> ::= "{" ( [ <ident> ":" ] <expr> "," ... ) "}"
```

For example: `let t = { x: 5, 3, "foo" };`. The type of this tuple can be inferred by the compiler to be `{ x: int, int, string }`.

The following is another example:

```pint
let t: { x: int, real } = { 6, 5.0 }
```

where the type of the tuple is indicated by the type annotation and has a named field `x`, but that named field is not actually used in the tuple expression. This is allowed because `{ x: int, real }` and `{ int, real }` coerce into each other.

Tuple fields can be initialized out of order only if and only if all the fields have names and their names are used in the tuple expression. For example, the following is allowed:

```pint
let t: { x: int, y: real } = { y: 5.0, x: 6 };
```

while the following are not:

```pint
let t: { x: int, real } = { 5.0, x: 6 };
let t: { x: int, y: real } = { 5.0, x: 6 };
let t: { x: int, y: real } = { 5.0, 6 }; // This is a type mismatch!
```

Tuple expressions that contain a single _unnamed_ field require the trailing `,` as in `let t = { 4.0, };`. Otherwise, the expression becomes a code block that simply evaluates to its contained expression. Tuple expressions that contain a single _named_ field do not require the trailing `,`.

Note that the grammar disallows empty tuple expressions `{ }`.

Tuple field access expressions are written as:

```bnf
<tuple-field-access-expr> ::= <expr> "." ( [0-9]+ | <ident> )
```

For example, `t.1;` refers to the second field of tuple `t`. Named field can be accessed using their names or their index. For example, if `x` is the third field of tuple `t`, then `t.2` and `t.x` are equivalent.
