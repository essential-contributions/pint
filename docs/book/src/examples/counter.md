## Counter

The "counter" is one of the simplest smart contracts that can be written in Pint. It showcases how a
contract can have multiple predicates and how it can declare and use _storage_. This particular
implementation of the "counter" contract is different from the one we encountered in the [Quickstart
Guide](../getting_started/quickstart.md#writing-a-pint-program).

```pint
{{#include ../../../../examples/counter.pnt}}
```

The contract starts by declaring a `storage` block which contains a single storage variable called
`counter` of type `int` (i.e. integer). The contract later declares two separate predicates, each
having a single _parameter_ and declaring two statements. Let's walk through the first predicate
named `Initialize`:

1. Predicate `Initialize` has a single _parameter_ called `value` of type `int`. The parameters of a
   predicate are essentially _decision variables_ that a solver is required to find values for such
   that every `constraint` in the predicate evaluates to `true`. The expression "decision variable"
   is commonly used in constraint programming languages to refer to unknowns that a solver must find
   given some constraints. In `Initialize`, the parameter `value` is the value that we want our
   counter to get initialized to.
1. The second statement declares a local variable and initializes it to `mut storage::counter`. The
   statement `let counter: int = mut storage::counter` creates a local variable called `counter` and
   initializes it to the current value of `counter` declared in the `storage` block. The `mut`
   keyword simply indicates that the solver is allowed to propose a new value for `counter`. If
   `mut` is not present, then the storage variable `counter` cannot be modified by anyone attempting
   to solve predicate `Initialize`.
1. The third statement contains the core logic of this predicate. It **declares** that the "next
   value" of `counter` **must** be equal to `value`. Note the `'` notation here which can be only
   applied to a local variable, and means "the next value of the state variable after a valid state
   transition".

The second predicate, called `Increment`, has a similar structure to `Initialize`. However, instead
of initializing `counter`, It increments it by `amount`. Note that both `counter` (the current
value) and `counter'` (the next value) are both used in the constraint to enforce that the next
value is dependent on the current value, which was not the case in `Initialize`.

### Solution

We won't go too deep into the solving process here but it's worth mentioning what a **solution** to
this predicate might look like. Broadly speaking, a solution contains two things:

1. An assignment of all the parameters of the predicate.
1. A list of all the state mutations proposed.

For example, if a user wants the value of the counter to be `42` they (or a solver acting on their
behalf) can propose a solution to `Initialize` that looks like this:

```toml
# parameters:
value: 42

# state mutations:
0: 42
```

This solution proposes a value of `42` for parameter `value` and a new value of `42` for the storage
key `0` where `counter` is stored (we will go over the storage data layout later!). Note that the
storage key `0` where `counter` is stored _can_ be modified by the solution because of the `mut`
keyword added before `storage::counter`.

A solution must also indicate which predicate is being solved using its address but we're omitting
that here for simplicity.

Alternatively, a solution to `Increment` can be proposed to satisfy the user requirement `counter =
42`. For example, if the current value of `counter` happens to be `35`, then the following solution
to `Increment` can be proposed:

```toml
# parameters:
amount: 7

# state mutations:
0: 42
```

It should be easy to verify that this solution satisfies the constraint `counter' == counter +
amount;` from `Increment`.
