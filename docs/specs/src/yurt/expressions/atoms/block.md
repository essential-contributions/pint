#### Block Expressions

Block expressions are expressions that contain a list of _statements_ followed by an expression within curly bracket `{ .. }`. Formally:

```bnf
<block-expr> ::= "{" ( <constraint-item> )* <expr> "}"
```

The type of the block expression is the type of the final expression. For example:

```yurt
let y: int;
let x: int = {
    constraint y == 2;
    y + 1 // returned final expression of type `int`
}
```

the above is equivalent to the following:

```yurt
let y: int;
let x: int;
constraint y == 2;
constraint x == y + 1;
```
