#### Prime Expressions

Prime expressions are used to refer to the _future_ value of a [state variable](../../items/states.md). They have the following syntax:

```bnf
<prime-expr> ::= <expr> "'"
```

For example:

```yurt
state u = MyContract::foo();
constraint u' - u > 10;
```

The above enforces that the blockchain state value returned by `MyContract::foo()` increases by at least 10 after the execution of a valid solution for the intent.
