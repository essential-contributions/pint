### Constraint Items

Constraint items represent the core of any intent. Any solution to the intent must satisfy all of its constraints.

Constraint items have this syntax:

```bnf
<constraint-item> ::= "constraint" <expr> ";"
```

For example:

```yurt
constraint a + b <= c
```

The expression in a constraint item must be of type `bool`.
