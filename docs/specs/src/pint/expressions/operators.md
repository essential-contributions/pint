### Operators

Operators are special functions that are distinguished by their syntax. There are two kinds of operators:

1. Unary operators which can be applied in a prefix manner without parentheses, e.g. `-x`.
1. Binary operator which can be applied in an infix manner, e.g. `3 + 4`.

```bnf
<un-op> ::= "-"   // Arithmetic negation
          | "!"   // Bitwise or logical complement

<bin-op> ::= "<"  // Less than comparison
           | ">"  // Greater than comparison
           | "<=" // Less than or equal comparison
           | ">=" // Greater than or equal comparison
           | "==" // Equality comparison
           | "!=" // Nonequality comparison
           | "+"  // Arithmetic Addition
           | "-"  // Arithmetic Subtraction
           | "*"  // Arithmetic multiplication
           | "/"  // Arithmetic division
           | "%"  // Arithmetic remainder
           | "&&" // Logical AND
           | "||" // Logical OR
```
