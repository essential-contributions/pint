## The `match` Construct

Pint has the powerful construct `match` that allows you to inspect a value that has a union type and
conditionally execute some code based on which union variant that union value matches. The power of
`match` comes from the introduction of the union variant datum into a specific scope, and the fact
that the compiler confirms that all possible variants are handled.

Pint has both `match` expressions and `match` statements, the distinction between which is
illustrated below.

### `match` as an Expression

Consider the following example which takes an unknown US coin and determines which coin it is and
returns its value in cents:

```pint
{{#include ../../../../../examples/ch_3_5_3.pnt:coins_1}}
```

Let’s break down the `match` statement above. First we list the `match` keyword followed by an
expression, which in this case is the value `coin`. This seems very similar to a conditional
expression used with `cond`, but there’s a big difference: with `cond`, the condition needs to
evaluate to a Boolean value, but here it has to be a union variant of `Coin` because this is the
type of the value `coin`.

Next are the `match` arms. An arm has two parts: a pattern and some code. The first arm here has a
pattern that is the value `Coin::Penny` and then the `=>` operator that separates the pattern and
the code to run. The code in this case is just the value `1`. Each arm is separated from the next
with a comma.

In this particular example, the code associated with each arm is an expression, and the resultant
value of the expression in the matching arm is the value that gets returned for the entire `match`
expression.

It is also possible to include `if` statements and `constraints` in the match arm code, but you must
use curly brackets. For example, the following code requires the Boolean variable `is_lucky_penny`
to be `true` if the value `coin` is a `Coin::Penny`.

```pint
{{#include ../../../../../examples/ch_3_5_3.pnt:coins_2}}
```

#### Patterns that Bind to Values

Another useful feature of match arms is that they can bind to the value that match the pattern. This
is how we can extract values out of union variants.

As an example, let's change our union variants to hold data inside them. Namely, we want each
variant to hold an `int` that represents the number of coins available:

```pint
{{#include ../../../../../examples/ch_3_5_3.pnt:coins_union}}
```

Now, given a value `coins` of type `Coins`, we can compute the total number of cents that `coins` is
equivalent to as follows:

```pint
{{#include ../../../../../examples/ch_3_5_3.pnt:coins_3}}
```

In the `match` expression above, we add a variable called `n` for each of the patterns. In each
pattern, `n` will bind to the value that the union variant holds. For example, if `coins` is equal
to `Coin::Nickel(42)`, then we expect `coins_in_cents` to be equal to `42 * 5 == 210`.

Note that `match` expressions can be nested. Here's an example that has nested unions and nested
`match` expressions to compute a `prize` given a coin and the face it landed on in a head-or-tails
game.

```pint
{{#include ../../../../../examples/ch_3_5_3.pnt:nested}}
```

### `match` as a Declaration

In some cases, you may not need a `match` statement to return a value. Instead, you may simply want
to enforce conditional constraints based on which pattern matches the given value. Below is a
rewrite of the previous example that uses `match` declarations. The code exhibits the exact same
behavior as before but written different to showcase `match` declarations.

```pint
{{#include ../../../../../examples/ch_3_5_3.pnt:declaration}}
```

Here, the top level `match` statement is a declaration; it does not return any value. Instead, it
declares some constraints, each based on one or more conditions. .

- If `coin` matches `CoinFace::Penny(f)`, then we add another `match` declaration that includes
  different constraints based on what pattern `f` matches.
- If `coin` matches `CoinFace::Nickel(f)`, then we add an `if` declaration that also includes
  different constraints based on whether `f` is equal to `Face::Head` or not.
- If `coin` matches `CoinFace::Dime(f)`, then we add a single constraint that uses a select
  expression to compute the prize.
- If `coin` matches `CoinFace::Quarter(f)`, then we add a single constraint that relies on a `match`
  expression.

### Matches are Exhaustive

There’s one other aspect of `match` we need to discuss: the arms’ patterns must cover all
possibilities. Consider the following version of variables `coins` and `coins_in_cents` previously
declared:

```pint
var coins: Coins;
var coins_in_cents = match coins {
    Coins::Penny(n) => n,
    Coins::Nickel(n) => n * 5,
    Coins::Quarter(n) => n * 25,
};
```

We didn't handle the `Coins::Dime(n)` case, so this code will cause a bug. Luckily, it's a bug the
Pint compiler knows how to catch. If we try to compile this code, we'll get this error:

```console
Error: not all match variants are covered
    ╭─[ch_3_5_3.pnt:42:26]
    │
 42 │ ╭─▶     var coins_in_cents = match coins {
    ┆ ┆
 46 │ ├─▶     };
    │ │
    │ ╰──────────── not all variants for union `::Coins` are covered by match
    │
    │     Help: branches and/or bindings are required for variant `Coins::Dime`
────╯
Error: could not compile `ch_3_5_3.pnt` due to previous error
```

The Pint compiler knows that we didn't cover every possible case, and even knows which pattern we
forgot. Matches in Pint are _exhaustive_: we must exhaust every last possibility in order for the
code to be valid. This is true for both `match` expressions and `match` declarations!
