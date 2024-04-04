#### "If" Expressions

Pint has `if` expressions which provide selection from two alternatives based on a condition. They have this syntax:

```bnf
<if-expr> ::= "if" <expr> <block-expr> "else" <block-expr>
```

The condition `<expr>` above must be of type `bool`. The "then" and "else" block expressions must have the same type or have types that are coercible to the same type, which determines the type of the whole `if` expression.

For example:

```pint
let y = if condition > 0 {
    1
} else {
    2
};
```

Note that the `else` block is **not optional** and the `else if { .. }` syntax is not supported.

#### "Cond" Expressions

Pint provides `cond` expressions which are generalized `if` expressions with more than two branches. That is, they provide selection from multiple alternatives, each based on some condition. They have the following syntax:

```bnf
<cond-branch> ::= <expr> "=>" <expr>

<else-branch> ::= "else" "=>" <expr>

<cond-expr> ::= cond "{" ( <cond-branch> "," )* <else-branch> [ "," ] "}"
```

The `<expr>` on the left-hand side of the `=>` in `<cond-branch>` must be of type `bool`. If it evaluates to `true`, then the branch is active which means that the whole `cond` expression takes the value of the second `<expr>` in `<cond-branch>`.

The branches are evaluated in order. The first one to become active determines the value of the `cond` expression. If all branches fail, then the `cond` expression takes the value of the `<expr>` in the `<else-branch>`.

Similarly to `if` expressions, all candidate expressions must have the same type or have types that are coercible to the same type, which determines the type of the whole `cond` expression.

For example:

```pint
enum Colour = Red | Green | Blue;
let y: Colour = Colour::Blue;
let z: Colour = Colour::Green;

let x: int = cond {
    z == Red => 0,
    y == Green || z != Green => 1,
    y == Blue => 2,
    else => 3,
}; // Evaluates to `2`
```
