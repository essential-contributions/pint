### Solve Items

Every intent must have at most one solve item. Solve items have the following syntax:

```bnf
<solve-item> ::= "solve" "satisfy" ";"
               | "solve" "minimize" <expr> ";"
               | "solve" "maximize" <expr> ";"
```

Example solve items:

```yurt
solve satisfy;
solve maximize a + b - c;
```

The solve item determines whether the intent represents a constraint satisfaction problem or an optimization problem. If a solve item is not present, the intent is assumed to be a satisfaction problem. For optimization problems, the given expression is the one to be minimized/maximized.
