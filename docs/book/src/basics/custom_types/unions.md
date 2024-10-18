## Unions

Unions allow you to define a type by enumerating its possible variants. Where structs and tuples
give you a way of grouping together related fields and data, like a `User` with its `status`,
`address`, and `balance`, unions give you a way of saying a value is one of possible set of values.
For example, we may want to say that `User` is one of a set of possible account types that also
includes `Contract`. To do this, Pint allows us to encode these possibilities as a `union`.

Let’s look at a situation we might want to express in code and see why unions are useful and more
appropriate that structs in this case. Say we need to describe token ownership of one of three
available tokens on an exchange: DAI, USDC, and USDT. Because these are the only tokens we can work
with, we can enumerate them all as different _variants_:

```pint
{{#include ../../../../../examples/ch_3_5_2.pnt:simple_union}}
```

Note how the possible variants of `Token` are separated by a `|`. `Token` is now a custom data type
that we can use elsewhere in our code. Any `Token` can now be `DAI`, `USDC`, or `USDT` but never 2
or more of these at the same time. That property of `Token` makes the union data structure
appropriate because a union variant can only be one of its variants. All three tokens are still
fundamentally _tokens_, so they should be treated as the same type when the code is handling
situations that apply to any of these tokens.

### Union Values

We can now create an instance of each of the three variants of `Token` like this:

```pint
{{#include ../../../../../examples/ch_3_5_2.pnt:simple_union_instances}}
```

Note that the variants of the union are namespaced under its identifier, and we use a double colon
to separate the two. This is useful because now all three values `Token::DAI`, `Token::USDC`, and
`Token::USDT` are of the same type: `Token`. We can then, for instance, declare a variable called
`token_type` to be of type `Token` and assign it to either variants depending some token ID:

```pint
{{#include ../../../../../examples/ch_3_5_2.pnt:union_selection}}
```

Using unions has even more advantages. Thinking more about our `Token` type, at the moment we don’t
have a way to store the actual token amount; we only know what kind it is. Given what you know about
structs and tuples, you might be tempted to tackle this problem with structs as follows:

```pint
{{#include ../../../../../examples/ch_3_5_2.pnt:union_in_struct}}
```

Here, we've defined a new type called `Balance` that has two fields: a `token` field that is of type
`Token` (the union we defined previously) and a `balance` field of type `int` that represents the
balance. We have two instances of this type. The first is `alice_bal`, and it has the value
`Token::DAI` as its `token` with associated `balance` of `42`. The second instance is `bob_bal`. It
has another variant of `Token` as its `kind` value, `Token::USDC`, and has a `balance` of `96`
associated with it. We've used a struct to bundle the `token` and `balance` values together, so now
the variant is associated with the value.

However, representing the same concept using just a union is more concise: rather than a union
inside a struct, we can put data directly into each union variant. This new definition of the
`Token` union says that the variants `DAI`, `USDC`, and `USDT` will have associated `int` values:

```pint
{{#include ../../../../../examples/ch_3_5_2.pnt:int_in_union}}
```

We attach data to each variant of the union directly, so there is no need for an extra struct. Also
notice the syntax for constructing an instance of the union where the value held by a variant is
added between parentheses after the variant name.

There's another advantage to using a union rather than a struct: each variant can have different
types and amounts of associated data. Let's look at another example of a union that has a wide
variety of types embedded in its variants:

```pint
{{#include ../../../../../examples/ch_3_5_2.pnt:action_union}}
```

This union has four variants with different types:

- `Quit` has no data associated with it at all.
- `Buy` includes a `TokenBalance`.
- `Sell` and `Swap` includes tuples.

Defining a union with variants such as the ones above is similar to defining different kinds of
struct definitions, except the union variants are grouped together under the `Action` type which
makes it easier to reason about them as part of a single type.
