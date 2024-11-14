## External Storage Access

It is common for one smart contract to want to reason about the state of another external smart
contract. For example, a decentralized exchange contract typically requires reading and modifying
the state (balances) owned by the external contracts of the tokens that are to be traded through the
exchange.

In imperative smart contract languages like [Solidity](https://soliditylang.org/), reasoning about
external state is typically done by _calling_ some methods in the external contract that access or
modify that contract's state. In Pint however, the storage variables of a contract are **public**
and can be accessed from outside the contract using the contract's
[interface](../smart_contracts/index.md#contract-interfaces).

While a contract's interface contains all the public symbols that can be externally accessed, it
does not specify the _address_ of the contract. The _address_ of a contract is a 256-bit value that
uniquely identify the contract on the blockchain. Specifying the address is required because
multiple contracts with different implementations may share the same interface.

Recall the interface of the [counter example](../examples/counter.md) that we presented earlier:

```pint
{{#include ../../../../examples/smart_contracts_d.pnt:interface}}
```

Assume that a contract with that interface has been deployed and has the following address:

```pint
{{#include ../../../../examples/storage_external.pnt:id}}
```

In order to access storage variable `counter` from interface `Counter`, we can create a path to
`conter` as follows:

```pint
{{#include ../../../../examples/storage_external.pnt:access}}
```

The path `Counter@[ContractID]::storage::counter` has three parts separated by `::`:

1. The name of the interface `Counter` followed by the address of the corresponding deployed
   contract in between `@[..]`.
1. The keyword `storage` to indicate that we're accessing the `storage` block of `Counter`.
1. `counter`, the name of the storage variable we want to access.

We can now use the local variable `counter` as usual. For example, we can constrain it as follows:

```pint
{{#include ../../../../examples/storage_external.pnt:constraint}}
```

This implies that we want that new value of the counter (which is owned by the external contract) to
be at least `100`. This could be accomplished in different ways:

1. If the current value of the counter is at least `100`, then nothing needs to be done. We don't
   even need to propose a solution for any predicates in the external contract.
1. If the current value of the counter is less than `100`, then the solution **must**:
   1. Either solve predicate `Initialize` with parameter `value` set to `100` or more.
   1. Or solve predicate `Increment` with parameter `amount` set to `100 - counter` or more.

Similarly to local storage access expressions, the expression
`Counter@[ContractID]::storage::counter` can only be used in the right-hand side of a `let`
declaration.

> **Note**: The `mut` keyword cannot be added to external storage accesses. External storage
> locations belong to the external contract that owns and it's up to the predicates in that contract
> to control their mutability.
