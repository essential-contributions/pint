## Counter

The "counter" is one of the simplest smart contract that can be written in Pint. It showcases how a
contract can have multiple `intent`s and how it can declare and use storage.

```pint
{{#include ../../../../examples/counter.pnt}}
```

The contract starts by declaring a `storage` block which contains a single storage variable called
`counter` of type `int` (i.e. integer). The contract later declares two separate `intent`
declarations, each declaring three distinct statements. Let's walk through the first `intent`:

1. The first statement declares a "decision variable". Decision variables are quite different from
   the "regular" variables that you might be familiar with from imperative languages. Decision
   variables are variables that the _solver_ is required to _find_ values for. You can think of them
   as "arguments" that the solver has to set such that _every_ `constraint` in the `intent`
   evaluates to `true`. In `Initialize`, we are declaring a single decision variable called `value`
   of type `int`. This is the value that we want our counter to get initialized to.
1. The second statement declares a `state` variable and initializes it to `storage::counter`. State
   variables are special variables that always need to be initialized to a _storage_ access
   expression. The statement `state counter: int = storage::counter` creates a state variable called
   `counter` and initializes it to the current value of `counter` declared in the `storage` block.
1. The third statement contains the core logic of this `intent`. It **declares** that "next value"
   of `state counter` **must** be equal to `value`. Note the `'` notation here which can be only
   applied to a `state` variable and means "the next value of after a valid state transition".

The second `intent`, called `Increment`, has a similar structure to `Initialize`. However, instead
of initializing `counter`, It increments it by `amount`. Note that both `counter` (the current
value) and `counter'` (the next value) are both used in the constraint to enforce that the next
value is dependent on the current value, which was not the case in `Initialize`.

### Solution

We won't go too deep into the solving process here but it's worth mentioning what a "solution" might
look like. Broadly speaking, a solution contains two things:

1. An assignment of all the decision variables in the intent.
1. A list of all the state mutations proposed.

For example, if a user wants the value of the counter to be `42` they (or an independent solver) can
propose a solution to `intent Initialize` that looks like this:

```toml
# decision variables:
value: 42

# state mutations:
0x0000000000000000000000000000000000000000000000000000000000000000: 42
```

This solution proposes a value of `42` for the decision variable `value` and a new value of `42` for
the storage location `0x00...00` where `counter` is actually stored (we will go over the storage
data layout later!). A solution must also indicate which intent is being solved using its address
but we're omitting that here for simplicity.

Alternatively, a solution to `intent Increment` can be proposed to satisfy the user requirements
(`counter = 42`). If the current value of `counter` is `35`, then a solution to `intent Increment`
can instead be proposed which looks like this:

```toml
# decision variables:
amount: 7

# state mutations:
0x0000000000000000000000000000000000000000000000000000000000000000: 42
```

It should be clear that this solution satisfies the constraint `counter' == counter + amount;`.