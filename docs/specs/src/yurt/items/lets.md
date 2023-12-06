### Let Declaration Items

These are typed **decision variables** whose values must be determined by a solver. They are the the "unknowns" of the program.

Variable declaration items have the following syntax:

```bnf
<let-item> ::= "let" <ident> ( ( ":" <ty> ) | ("=" <expr> ) | ( ":" <ty> "=" <expr> ) ) ";"
```

For example:

```yurt
let x: int;
let y = 5;
let z: int = 7;
```

Note that at least one of the type annotation and the initializing expression has to be present so that the type of the variable can be determined. This implies that `let x;` is not a valid variable declaration.

"Initializing" a decision variable is equivalent to imposing an equality constraint on it. For example, `let y = 5` is equivalent to:

```yurt
let y: int;
constraint y == 5;
```
