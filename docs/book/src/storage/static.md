## Statically-Sized Storage Types

All storage variables in a Pint contract must be declared inside a `storage` block, and there can
only be a single `storage` block in a Pint contract. The `storage` block is optional and can be
skipped if the contract does not need to manage any state, but such contracts are generally not very
useful.

Here's an example of a `storage` block:

```pint
{{#include ../../../../examples/storage_static.pnt:storage_block}}
```

A `storage` starts with the keyword `storage` and followed by a comma-separated list of variable
declarations inside curly brackets. Each variable declaration is an identifier annotated with a
type. In this chapter, we're only looking at storage variables that have statically-sized types,
i.e., types that have known sizes and layouts at compile time. This pretty much covers every type we
discussed so far! In the next chapter, we will introduce new storage-only types that are dynamically
sized.

Back to the example above, the declared `storage` block is fairly simple. It contains several
variables with different primitive and compound types. Similarly to predicate parameters, storage
variables **must** have a type annotation.

### Accessing Storage Variables

Storage variables are not useful unless we can read them and/or _propose modifications to them_.
Recall that Pint is a declarative constraint-based language, and therefore, does not allow "writing"
directly to a storage variable. Writing directly to storage is a concept you might be familiar with
from other smart contract languages like [Solidity](https://soliditylang.org/), so you might be
asking yourself the following question: "if I can't write directly to storage variables, how will
their values ever change? How will the state of the blockchain ever change?"

The answer to these questions lies at the core of what makes Pint and _declarative blockchains_
actually **declarative**. Pint has no concept of "updating" state (or even predicate parameters or
local variables for that matter). Pint simply _expresses_ a desired outcome using `constraint`
statements and relies on _solutions_ to actually propose state changes.

In order to express a desired outcome for a given storage variable, two things are needed:

1. A way to read the **current** value of the variable.
1. A way to express the **future** value of the variable.

#### Reading a Storage Variable

Reading a storage variable should not be an unfamiliar concept to you if you've worked with other
smart contract languages before like [Solidity](https://soliditylang.org/). The syntax for reading a
storage variable in Pint requires the `storage` keyword again:

```pint
{{#include ../../../../examples/storage_static.pnt:storage_access_1}}
```

A few things to note here:

1. Storage read expressions always start with `storage::` followed by the name of the variable we're
   trying to read. The `storage::` syntax means we're switching namespaces to that of the `storage`
   block.
1. Each storage read expression is used in the initializer of a local variable. In fact, storage
   read expressions can **only** ever be used in initializers of local variables. Using a storage
   read expression in other contexts, such as in `constraint storage::x == 5`, is currently illegal.
1. Fields or elements of a compound type in storage can be accessed individually, as in
   `storage::t.1` and `storage::arr[2].1`.
1. Arbitrary expressions that include storage read expressions can also be used to initialize local
   variables. Variable `incremented` is an example of that.

Note that, while storage read expressions cannot be evaluated at compile time, they are actually
known at solve-time: right before the solving process starts, every storage read expression is
evaluated by directly inspecting the blockchain. The result is then used in the corresponding local
variable initializer expression which becomes known in preparation for solving.

#### Next State

Recall that expressing a desired outcome for a given storage variable also requires a way to express
the **future** value of the variable.

In most imperative languages, statements like `x = x + 1` are commonly used to mean "_update_ the
value of `x` to be equal to the _current_ value of `x` plus `1`". Because Pint is a constraint-based
declarative language where the order of statements does not matter and there is no sequential
execution, statements like `x = x + 1` cannot be written and are not logical. Instead, Pint offers a
special syntax, reserved for local variables, that means "the future value of". Here's an example:

```pint
{{#include ../../../../examples/storage_static.pnt:next_state}}
```

Here, `bal'`, unlike `bal`, is actually **unknown at solve-time**. That is, `bal'` must be solved
for as if it were a predicate parameter and every solution must include a proposed value for `bal'`.
If, for example, the value of `bal` was read to be `100` at solve-time, a solver might propose that
the next value of `bal` should be `150` (i.e. `bal' = 150`) which would be a valid solution because
`150 >= 100 + 42` (assuming all other constraints in the predicate are also satisfied).

The `'` operator can also be used for variables that have arbitrary initializers. For example:

```pint
{{#include ../../../../examples/storage_static.pnt:next_state_arbitrary_expr}}
```

Basically, when validating a solution, `bal_in_dollars'` is computed by plugging in the _new_ value
of `storage::bal` into the expression `price * storage::bal`. That is, a valid solution must satisfy
`bal_in_dollars' == price * bal'` where `bal_in_dollars` is computed in this way (this just happens
to be always true in this case!).

As you can imagine, using the `'` operator for variables that do not depend on storage accesses is a
no-op. For example

```pint
{{#include ../../../../examples/storage_static.pnt:next_state_no_op}}
```

#### "Mutable" Storage Accesses

In the previous section, you may have noticed that we added the `mut` keyword before `storage::bal`
in the declaration of `bal`. In Pint, storage locations are **non-mutable by default**. That is,
solvers cannot propose new values for a storage location _unless_ they are solving a predicate that
allows the storage location to be mutable. This is accomplished using the `mut` keyword added before
a storage access. In the example of the previous section, because `mut` was added before
`storage::bal`, a solver can now propose a _state mutation_ that updates the value of `x`.

When the `mut` keyword as added before a storage access into a compound type, mutability applies
**only** to the portion of the compound type that is being accessed. For example, in the example
below:

```pint
{{#include ../../../../examples/storage_static.pnt:mut}}
```

`v.1` is a storage access into nested tuple `v` defined in the `storage` block declared earlier.
Here, both `v.1.0` ( a `bool`) and `v.1.1` (a `b256`) are both allowed to be mutated, but `v.0` is
not allowed to be.

#### "Empty" State

You may be wondering what happens if a storage variable was never previously updated but was read
anyways. In this case, there is no value stored at that storage variable and nothing can be read.
To help you reason about this, Pint provides the literal `nil` to represent the _absence of a
value_. For example,

```pint
{{#include ../../../../examples/storage_static.pnt:nil}}
```

In the example above, we first check if `w` is `nil` before attempting to read it. If it is `nil`
(i.e. currently has no value), then we initialize `value_1` to `0`. Otherwise, we initialize it to
the non-empty value of `w`. Without checking if `w` is `nil` first, and if we're not sure whether
`w` has a value or not, then it is possible that the state read operation will fail causing a
_panic_ in the VM.

The following is also a valid approach for handling `nil` checks:

```pint
{{#include ../../../../examples/storage_static.pnt:nil_in_storage_expr}}
```

It is also possible to update a variable to `nil` using the "next state" operator:

```pint
{{#include ../../../../examples/storage_static.pnt:update_to_nil}}
```

Here, if `w` currently has a value, then we constrain the next value of `w` to be `nil`.

This concludes our overview on storage which only focused on statically-sized storage types. In the
next chapter, we will cover dynamically-sized storage types which offer a lot more flexibility.
