## Expressions

Expressions represent values and have the following syntax:

```bnf
<expr> ::= <un-op> <expr>
         | <expr> <bin-op> <expr>
         | <block-expr>
         | "(" <expr> ")"
         | <path>
         | <bool-literal>
         | <int-literal>
         | <real-literal>
         | <string-literal>
         | <tuple-expr>
         | <tuple-field-access-expr>
         | <array-expr>
         | <array-element-access-expr>
         | <range-expr>
         | <if-expr>
         | <cond-expr>
         | <forall-expr>
         | <call-expr>
         | <cast-expr>
         | <in-expr>
         | <prime-expr>
```

Expressions can be composed from sub-expressions with operators. All unary and binary operators are described in [Operators](expressions/operators.md). All unary operators bind more tightly than all binary operators. Expressions can also be contained within parentheses.
