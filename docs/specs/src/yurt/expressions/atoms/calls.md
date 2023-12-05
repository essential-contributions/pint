#### Call Expressions

Call expressions are used to call macros or functions and have the following syntax:

```bnf
<func-call-expr> ::= <path> "(" [ <expr> "," ... ] ")"
<macro-call-expr> ::= <path> "(" [ <tok>+ ";" ... ] ")"
```

For example: `let x = foo(5, 2);` or `constraint bar@(5; [a, b])`.

For function-style macros or extern function calls the type of the expressions passed as arguments must match the argument types of the called function. For all call expressions the number of passed arguments must match the called item, though this is flexible due to Yurt's variadic macro support. The return type of the function must also be appropriate for the calling context.

See [Macro Items](../../items/macros.md) for the distinction between regular and function-style macros. When calling regular macros the call arguments may be a collection of source tokens, excluding semi-colons, delimited by semi-colons.
