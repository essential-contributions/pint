## Statically-Sized Storage Types

All storage variables in a Pint contract must be declared inside a `storage` block, and there can
only be a single `storage` block in a Pint contract. The `storage` block is optional and can be
skipped if the contract does not need to manage any state, but such contracts are generally not very
useful.

Here's an example of a `storage` block:

```pint
{{#include ../../../../examples/ch_5_1.pnt:storage_block}}
```

A `storage` starts with the keyword `storage` and followed by a comma-separated list of variable
declarations inside curly brackets. Each variable declaration is an identifier annotated with a
type. In this chapter, we're only looking at storage variables that have statically-sized types,
i.e., types that have known sizes and layouts at compile time. This pretty much covers every type we
discussed so far! In the next chapter, we will introduce new storage-only types that are dynamically
sized.

Back to the example above, the declared `storage` block is fairly simple. It contains three
variables that have primitive types and one variable that has a compound type (a tuple). Unlike
decision variables, storage variables do not accept an initializer and **must** have a type
annotation.

### Accessing Storage Variables

Storage variables are not useful unless we can read them and/or _propose modifications to them_.
Recall that Pint is a declarative constraint-based language, and therefore, does not allow "writing"
directly to a storage variable. Writing directly to storage is a concept you might be familiar with
from other smart contract languages like [Solidity](https://soliditylang.org/), so you might be
asking yourself the following question: "if I can't write directly to storage variables, how will
their values ever change? How will the state of the blockchain ever change?"

The answer to these questions lies at the core of what makes Pint and _declarative blockchains_
actually **declarative**. Pint has no concept of "updating" state (or even decision variables for
that matter). Pint simply _expresses_ a desired outcome using `constraint` statements and relies on
_solutions_ to actually propose state changes.

In order to express a desired outcome for a given storage variable, two things are needed:

1. A way to read the **current** value of the variable.
1. A way to express the **future** value of the variable.

#### Reading a Storage Variable

Reading a storage variable should not be an unfamiliar concept to you if you've worked with other
smart contract languages before like [Solidity](https://soliditylang.org/). The syntax for reading a
storage variable in Pint requires the `storage` keyword again:

```pint
{{#include ../../../../examples/ch_5_1.pnt:storage_access_1}}
```

A few things to note here:

1. Storage read expressions always start with `storage::` followed by the name of the variable we're
   trying to read. The `storage::` syntax means we're switching namespaces to that of the `storage`
   block.
1. Each storage read expression is used to initialize a `state` variable. In fact, storage read
   expressions can **only** ever be used to initialize `state` variables. Using a storage read
   expression in other contexts, such as in `constraint storage::x == 5`, is illegal.
1. Fields or elements of a compound type in storage can be accessed individually, as in
   `storage::t.1`.

We haven't really explained what a `state` variable is so this is probably a good time to do so.

#### State Variables

A state variable is a special type of variables that can only hold values read from storage. A
`state` variable must **always** have an initializer and that initializer can only be a storage read
expression. Type annotations for `state` declarations are optional:

```pint
{{#include ../../../../examples/ch_5_1.pnt:state}}
```

Once a `state` variable is declared, it can be used anywhere in its scope as if it were a decision
variable:

```pint
{{#include ../../../../examples/ch_5_1.pnt:constraint}}
```

This is an example where two state variables are declared and later constrained as if they were
decision variables. One important distinction to note here is that `state` variables are not
actually _unknown_. By definition, decision variables are unknown at compile time _and_ at
solve-time and they only become known _after_ the solving process is finished (if a solution is
found). In contrast, `state` variables, while unknown at compile time, are actually known at
solve-time: right before the solving process starts, every storage read expression is evaluated by
directly inspecting the blockchain. The result is then assigned to the corresponding `state`
variable which becomes known in preparation for solving.

#### Next State

Recall that expressing a desired outcome for a given storage variable also requires a way to express
the **future** value of the variable.

In most imperative languages, statements like `x = x + 1` are commonly used to mean "_update_ the
value of `x` to be equal to the _current_ value of `x` plus `1`". Because Pint is a constraint-based
declarative language where the order of statements does not matter and there is no sequential
execution, statements like `x = x + 1` cannot be written and are not logical. Instead, Pint offers a
special syntax, reserved for `state` variables, that means "the future value of". Here's an example:

```pint
{{#include ../../../../examples/ch_5_1.pnt:next_state}}
```

Here, `bal'`, unlike `bal`, is actually unknown at solve-time. That is, `bal'` must be solved for as
if it were a decision variable and every solution must include a proposed value for `bal'`. If, for
example, the value of `bal` was read to be `100` at solve-time, a solver might propose that the next
value of `bal` should be `150` (i.e. `bal' = 150`) which would be a valid solution because `150 >=
100 + 42` (assuming all other constraints in the predicate are also satisfied).

This concludes our overview on storage which only focused on statically-sized storage types. In the
next chapter, we will cover dynamically-sized storage types which offer a lot more flexibility.
