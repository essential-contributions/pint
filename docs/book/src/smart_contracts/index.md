## Smart Contracts

Pint is a language for writing "smart contracts". If you're familiar with smart contract languages
like [Solidity](https://soliditylang.org/), then many elements of a Pint contract should feel
familiar. Of course, at its core, Pint is fundamentally different from imperative smart contract
languages. Writing smart contracts in Pint requires a different way of thinking about how to express
the rules that the smart contract must enforce.

A Pint contract is a **collection of predicates**. Each predicate has a name, a list of typed
parameters, and a list of constraints. Predicates of a smart contract describe the various ways
**state can be mutated** in order to accomplish certain tasks (e.g. token transfer). A contract may
also contain a `storage` declaration which contain all the storage variable that the contract owns.
Contract storage is effectively the database of the contract where persistent state lives. We will
discuss storage in details in [Chapter 5](../storage/index.md).

### Contract Structure

The structure of a Pint smart contract is fairly simple. We simply lay out all the code blocks as
follows:

```pint
{{#include ../../../../examples/smart_contracts_a.pnt}}
```

The order of the code blocks is not important, and the `storage` block is optional but most useful
smart contracts will need it.

Unlike imperative smart contracts where the logic lives in contract _methods_ that can be _called_
to make state updates, Pint contracts have predicates (not methods/functions even though they look
like ones!) and nothing is ever "called". Instead, _solutions_ have to submitted that satisfy one or
more predicates in the contract. A solution must specify concrete values for the parameters of the
solved predicates, as well as propose changes to the state if necessary.

If the proposed values for the parameters and the proposed state changes satisfy **all** the
constraints for each solved predicate (potentially including predicates from other contracts), then
the solution is deemed valid and the proposed state changes should be accepted.

### Predicate Parameters

A predicate parameter is a named parameter that every solution is required to assign a value for.
Predicate parameters are quite different from "regular" function parameters that you might be used
to in imperative programming languages since predicates of a smart contract are _not called_!.
Instead, they are _solved_ by proposing values for these parameters such that all the constraints
are satisfied.

All predicate parameters have to be annotated with a type. We will go over the available data types
in Pint, in detail, in a later chapter.

Here's an example that shows how to declare a predicate named `test` with three parameters `foo`,
`bar`, and `baz` with three different types:

```pint
{{#include ../../../../examples/smart_contracts_b.pnt:parameters}}
```

The predicate `test` also declares a constraint that enforces that the square of `foo` is at most
`1024`, meaning that any proposed solution must assign a value for `foo` that satisfies this
condition. For example, if `foo` is set to `7`, this is constraint would be satisfied. If `foo` is
set to `11`, this constraint would not be satisfied. We will go over constraints in more detail in
[Chapter 4.6](../basics/constraints.md).

You can think of the type annotation on each predicate parameter as an implicit "constraint". For
example, parameter `foo` can only take values in the set of signed integers (64-bit signed integers
when targeting the EssentialVM) while `bar` can only take two values: `false` or `true` (i.e. `0` or
`1` in the EssentialVM).

### Contract Interfaces

Each smart contract has an interface which can be easily generated from the contract. The interface
is not required to write the smart contract but is required to interact with the contract from
_external contexts_. For example, one smart contract can propose an update to a storage variables
that is owned by another contract. Will will go over how to do that in [Chapter
5](../storage/index.md). Another example is invoking external predicates which is a more advanced
topic that we will cover in [Chapter 7.1](../advanced/invoking_predicates.md).

A contract interface has the following structure:

```pint
{{#include ../../../../examples/smart_contracts_c.pnt:interface}}
```

You can see the similarities between the structure of the interface and the structure of the smart
contract. An interface starts with `interface` keyword followed by the name of the interface which
can be used when referring the interface from external contexts. Inside the interface declaration,
an optional `storage` block can be declared as well as a list of **predicate interfaces**, each with
its own list of typed parameters. The storage block and the predicate signatures of an interface
should always match the corresponding storage block and predicate signatures of the deployed
contract. Otherwise, correct behavior cannot be guaranteed.

For example, an interface for the [counter example](../examples/counter.md) looks like this:

```pint
{{#include ../../../../examples/smart_contracts_d.pnt:interface}}
```

Hopefully nothing here is surprising! The key point is that an `interface` must expose _everything_
that is public in a contract, and that includes the storage block and the predicate signatures.

> **Note** in the future, Pint will have a tool that will auto-generate interfaces from a smat
> contract.
