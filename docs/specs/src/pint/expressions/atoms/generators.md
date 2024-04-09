#### Generator Expressions

Generator expressions are used to reduce verbosity and improve readability of a Pint program. The most common generator expression is `forall` which has the following syntax:

```bnf
<forall-expr> ::= "forall" <ident> "in" <range-expr> ( "," <ident> "in" <range-expr> )*
                 [ "where" <expr> ( "," <expr> )* ]
                 "{" <expr> "}"
```

The rule `<ident> "in" <range-expr>` effectively declares a new integer index named `<ident>` that belongs to the range `<range-expr>`. This index can only be used in the body of the `forall` expression.

The `where` clause restricts the declared indices using the Boolean expressions that follow it.

A `forall` expression is equivalent to the conjunction of all the possible instances of its body, given its ranges and conditions.

For example, the following `forall` expression:

```pint
forall i in 0..3, j in 0..3 where i <= j {
    a[i] != b[j]
}
```

is equivalent to the following:

```pint
   a[0] != b[0]
&& a[0] != b[1]
&& a[0] != b[2]
&& a[1] != b[1]
&& a[1] != b[2]
&& a[2] != b[2]
```
