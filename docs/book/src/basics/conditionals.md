## Conditionals

Pint has two different ways to express conditionals. `if` expressions and `cond` expressions.
Conditionals allow you to "branch" your code depending on some condition(s). The word "branch" is
between quotations because Pint is **not** an imperative language and does not "execute" code.
Therefore, conditionals are simply a way of saying: "this expression is equal to one of the
following values, based on some condition(s)".

### `if` Expressions

An `if` expression allows you to select between two alternatives based on a condition. Every `if`
expression has a corresponding `else` where the second alternative is added. The two alternatives
must have the same type and that type determines the type of the whole `if` expression. For example:

```pint
{{#include ../../../../examples/ch_3_4_a.pnt:basic_if}}
```

All `if` expressions start with the keyword `if`, followed by a condition. In this case, the
condition checks whether or not the decision variable `number` has a value less than 5. We place the
expression that should be chosen if the condition is `true` immediately after the condition inside
curly brackets. The keyword `else` is then added followed by the expression that should be chosen if
the condition is `false` (also between curly brackets). Both options, `1` and `2`, have the same
type which is `int` and so, the type of `y` must also be `int`.

If, for example, the types of the two expressions we're selecting from do not match, the compiler
will emit a compile error. For example, if we try to compile the following code:

```pint
let number: int;

let y = if number < 5 {
    1
} else {
    2.0
};
```

we will get the following error:

```console
Error: branches in if-expression must have the same type
   ╭─[ch_3_4.pnt:4:9]
   │
 5 │     1
   │     ┬
   │     ╰── 'then' branch has the type `int`
   │
 7 │     2.0
   │     ─┬─
   │      ╰─── 'else' branch has the type `real`
───╯
```

The condition of an `if` expression must be a `bool`. Otherwise, we will get a compile error. For
example, if we try to compile the following code:

```pint
let number: int;
let y = if number {
    1
} else {
    2
};
```

we will get the following error:

```console
Error: condition in if-expression must be a boolean
   ╭─[ch_3_4.pnt:4:12]
   │
 4 │ let y = if number {
   │            ───┬──
   │               ╰──── condition must be a boolean
───╯
```

Note that Pint will **not** automatically try to convert non-Boolean types to a Boolean. You must be
explicit and always provide `if` with a `Boolean` as its condition.

### `cond` Expressions

Pint provides `cond` expressions. `cond` expressions are generalized `if` expressions that are not
limited to only two branches. They provide selection from multiple alternatives, each based on some
condition. For example:

```pint
{{#include ../../../../examples/ch_3_4_a.pnt:basic_cond}}
```

All `cond` expressions start with the keyword `cond`, followed by a comma-separated list of
statements in between curly brackets. Each statement describes a condition and an expression that
should be returned by the `cond` if that condition is correct. The branches are evaluated in order
and the first one to become active determines the value of the `cond` expression. If all branches
fail, then the `cond` expression takes the value of the expression in the `else` branch, which must
always be the last branch.

In the example above, `z` is equal to `0` if `x == 0`, equal to `1` if `x` is between `0` and `10`,
equal to `2` if `x` is between `10` and `100`, and equal to `3` otherwise.

Every `cond` expression can be rewritten using one or more `if` expressions. However, `cond` tends
to be more compact and more readable than nested `if` expressions. For example, the `cond`
expression in the example above is equivalent to:

```pint
{{#include ../../../../examples/ch_3_4_b.pnt:cond_unrolled}}
```

Similarly to `if` expressions, all candidate expressions must have the same type which determines
the type of the whole `cond` expression. Also, every condition must be a `bool` or else a compile
error will be emitted.
