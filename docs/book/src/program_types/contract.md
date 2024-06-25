## Smart Contracts

The most common use case for Pint is writing "smart contracts". If you're familiar with smart
contract languages like [Solidity](https://soliditylang.org/), then many elements of a Pint contract
should feel familiar. Of course, at its core, Pint is fundamentally different from imperative smart
contract languages. Writing smart contracts in Pint requires a different way of thinking about how
to express the rules that the smart contract must enforce.

A Pint contract is a **collection of predicates**. Each predicate has a name and contains a list of
constraints. A contract may also contain a `storage` declaration which contain all the storage
variable that the contract owns. Contract storage is effectively the database of the contract where
persistent state lives. We will discuss storage in details in [Chapter 5](../storage/index.md).

### Contract Structure

The structure of a Pint smart contract is fairly simple. We simply lay out all the code blocks as
follows:

```pint
{{#include ../../../../examples/ch_4_2_a.pnt}}
```

The order of the different blocks is not important, and the `storage` block is optional but most
useful smart contracts will need it.

Unlike imperative smart contracts where the logic lives in contract _methods_ that can be _called_
to make state updates, Pint contracts have predicates (not methods/functions!) and nothing is ever
"called". Instead, _solutions_ have to submitted that satisfy one of the predicates in the contract.
A solution must specify concrete values for all the decision variables in the predicate, as well as
propose changes to the state. If the proposed state changes and the decision variables satisfy
**all** the constraints in that particular predicate, then the solution is deemed valid and the
proposed state changes are committed.

### Contract Interfaces

Each smart contract has an interface which can be easily generated from the contract. The interface
is not required to write the smart contract but is required to interact with the contract from
_external contexts_. For example, one smart contract can propose an update to a storage variables
that is owned by another contract. Will will go over how to do that in [Chapter
5](../storage/index.md).

The structure of an contract interface looks like this:

```pint
{{#include ../../../../examples/ch_4_2_b.pnt:interface}}
```

You can see the similarities between the structure of the interface and the structure of the smart
contract. An interface starts with `interface` keyword followed by the name of the interface which
can be used when referring the interface from external contexts. Inside the interface declaration,
an optional `storage` block can be declared as well as a list of **predicate interfaces**. These
predicate interfaces contain all the **public** decision variables that the predicates expose. We
will discuss public decision variables in detail in [Chapter
6](../public_decision_variables/index.md), but for now, you can think of these variables as regular
decision variable except that they can be read from an external context.

Let's revisit the [counter example](../examples/counter.md) but with a small modification where we
have made all decision variables `pub`, which marks them as public:

```pint
{{#include ../../../../examples/ch_4_2_c.pnt:contract}}
```

An interface for the contract above looks like this:

```pint
{{#include ../../../../examples/ch_4_2_c.pnt:interface}}
```

hopefully nothing here is surprising! The key point here is that an `interface` must expose
_everything_ that is public in a contract, and that includes the storage block and all public
decision variables in each predicate.

Note that, if a predicate `Foo` has no public decision variables, both `intent Foo { }` and `intent
Foo;` can be used when adding it to the interface.

> **Note** in the future, Pint will have a tool that will auto-generate interfaces.
