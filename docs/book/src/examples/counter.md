## Counter

The "counter" is one of the simplest smart contracts that can be written in Pint. It showcases how a
contract can have multiple predicates and how it can declare and use _storage_.

```pint
{{#include ../../../../examples/counter.pnt}}
```

The contract starts by declaring a `storage` block which contains a single storage variable called
`counter` of type `int` (i.e. integer). The contract later declares two separate predicates, each
declaring three statements. Let's walk through the first predicate named `Initialize`:

1. The first statement declares a **decision variable**. Decision variables are quite different from
   "regular" variables which you might be familiar with from imperative languages. Decision
   variables are variables that the _solver_ is required to _find_ values for. You can think of them
   as "arguments" that the solver has to set such that _every_ `constraint` in the predicate
   evaluates to `true`. In `Initialize`, we are declaring a single decision variable called `value`
   of type `int`. This is the value that we want our counter to get initialized to.
1. The second statement declares a `state` variable and initializes it to `storage::counter`. State
   variables are special variables that always need to be initialized to a _storage_ access
   expression. The statement `state counter: int = storage::counter` creates a state variable called
   `counter` and initializes it to the current value of `counter` declared in the `storage` block.
1. The third statement contains the core logic of this predicate. It **declares** that the "next
   value" of `state counter` **must** be equal to `value`. Note the `'` notation here which can be
   only applied to a `state` variable, and means "the next value of the state variable after a valid
   state transition".

The second predicate, called `Increment`, has a similar structure to `Initialize`. However, instead
of initializing `counter`, It increments it by `amount`. Note that both `counter` (the current
value) and `counter'` (the next value) are both used in the constraint to enforce that the next
value is dependent on the current value, which was not the case in `Initialize`.

### Solution

We won't go too deep into the solving process here but it's worth mentioning what a **solution** to
this predicate might look like. Broadly speaking, a solution contains two things:

1. An assignment of all the decision variables in the predicate.
1. A list of all the state mutations proposed.

For example, if a user wants the value of the counter to be `42` they (or a solver acting on their
behalf) can propose a solution to `Initialize` that looks like this:

```toml
# decision variables:
value: 42

# state mutations:
0x0000000000000000000000000000000000000000000000000000000000000000: 42
```

This solution proposes a value of `42` for the decision variable `value` and a new value of `42` for
the storage location `0x00...00` where `counter` is stored (we will go over the storage data layout
later!). A solution must also indicate which predicate is being solved using its address but we're
omitting that here for simplicity.

Alternatively, a solution to `Increment` can be proposed to satisfy the user requirement `counter =
42`. For example, if the current value of `counter` happens to be `35`, then the following solution
to `Increment` can be proposed:

```toml
# decision variables:
amount: 7

# state mutations:
0x0000000000000000000000000000000000000000000000000000000000000000: 42
```

It should be easy to verify that this solution satisfies the constraint `counter' == counter +
amount;` from `Increment`.
