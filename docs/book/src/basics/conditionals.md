## Conditionals

Pint has three different ways to express conditionals: select expressions, `cond` expressions, and
`if` statements. Conditionals allow you to "branch" your code depending on some condition(s). The
word "branch" is between quotations because Pint is **not** an imperative language and does not
"execute" code. Therefore, conditionals are simply a way of saying: "choose between some expressions
or some constraints based on some condition(s)".

### Select Expressions

A select expression allows you to select between two alternatives based on a condition. The two
alternatives must have the same type and that type determines the type of the whole select
expression. For example:

```pint
{{#include ../../../../examples/ch_3_4_a.pnt:basic_select}}
```

All select expressions start with a condition followed by the `?` symbol. In this case, the
condition checks whether or not the decision variable `number` has a value less than 5. We place the
expression that should be chosen if the condition is `true` immediately after the `?` symbol. The
symbol `:` is then added followed by the expression that should be chosen if the condition is
`false`. Both options, `1` and `2`, have the same type which is `int` and so, the type of `y` must
also be `int`.

If, for example, the types of the two expressions we're selecting from do not match, the compiler
will emit a compile error. For example, if we try to compile the following code:

```pint
var number: int;

var y = number < 5 ? 1 : 2.0;
```

we will get the following error:

```console
Error: branches of a select expression must have the same type
   ╭─[test.pnt:3:9]
   │
 3 │ var y = number < 5 ? 1 : true;
   │                      ┬   ──┬─
   │                      ╰───────── 'then' branch has the type `int`
   │                            │
   │                            ╰─── 'else' branch has the type `bool`
```

The condition of a select expression must be a `bool`. Otherwise, we will get a compile error. For
example, if we try to compile the following code:

```pint
var number: int;

var y = number ? 1 : 2;
```

we will get the following error:

```console
Error: condition for select expression must be a `bool`
   ╭─[test.pnt:3:9]
   │
 3 │ var y = number ? 1 : 2;
   │         ───┬──
   │            ╰──── invalid type `int`, expecting `bool`
───╯
```

Note that Pint will **not** automatically try to convert non-Boolean types to a Boolean. You must be
explicit and always provide a select expression with a `Boolean` as its condition.

### `cond` Expressions

Pint provides `cond` expressions. `cond` expressions are generalized select expressions that are not
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

in the example above, `z` is equal to `0` if `x == 0`, equal to `1` if `x` is between `0` and `10`,
equal to `2` if `x` is between `10` and `100`, and equal to `3` otherwise.

every `cond` expression can be rewritten using one or more select expressions. however, `cond` tends
to be more compact and more readable than nested select expressions. for example, the `cond`
expression in the example above is equivalent to:

```pint
{{#include ../../../../examples/ch_3_4_b.pnt:cond_unrolled}}
```

similarly to select expressions, all candidate expressions must have the same type which determines
the type of the whole `cond` expression. also, every condition must be a `bool` or else a compile
error will be emitted.

### `if` Statements

`if` statements are the last class of conditionals we will look at. Unlike select and `cond`
expressions, `if` statements are **not** expressions, that is, they do not hold any values. Instead,
they allow predicating _blocks of code_ based on some condition. A block of code in the context of
an `if` statement is a collection of constraints and other `if` statements. For example:

```pint
{{#include ../../../../examples/ch_3_4_a.pnt:basic_if}}
```

All `if` statements start with the keyword `if`, followed by a condition. In this case, the
condition checks whether or not the decision variable `number` has a value less than 5. The code
block that should be "active" if the condition is `true` is placed immediate after the condition
inside curly brackets. Optionally, the keyword `else` is then added followed by the code block that
should be active if the condition is `false` (also between curly brackets). In the example above,
the `if` statement can be read as follows: "if `number` is less than 5, then `x` must be equal to
`y`. Otherwise, `x` must not be equal to `y`".

Similarly to select expressions, the condition of an `if` statement must be a `bool`. Otherwise we
will get a compile error.

`if` statements can be nested and can contain an arbitrary number of constraints:

```pint
{{#include ../../../../examples/ch_3_4_a.pnt:complex_if}}
```
