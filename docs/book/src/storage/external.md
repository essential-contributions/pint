## External Storage Access

It is common for one smart contract to want to reason about the state of another external smart
contract. For example, a Decentralized Exchange contract typically requires reading and modifying
the state (balances!) owned by the external contracts of the tokens that are being traded through
the exchange.

In imperative smart contract languages like [Solidity](https://soliditylang.org/), reasoning about
external state is typically done by _calling_ some methods in the external contract that access or
modify that contract's state. In Pint however, the storage variables of a contract are **public**
and can be accessed from outside the contract using the contract's
[interface](../program_types/contract.md#contract-interfaces).

### Interface Instance

While a contract's interface contains all the public symbols that can be externally accessed, it
does not specify the _address_ of the contract. The _address_ of a contract is 256-bit value that
uniquely identify the contract on the blockchain. Specifying the address is required because
multiple contracts with different addresses may share the same interface.

In order to specify which external contract to actually interact with, we need an **interface
instance**. Consider the [counter example](../examples/counter.md) that we presented earlier and its
interface that looks like this:

```pint
{{#include ../../../../examples/ch_5_3.pnt:interface}}
```

Assume that a contract with that interface has been deployed and has the following address:

```pint
{{#include ../../../../examples/ch_5_3.pnt:id}}
```

In order to interact with that particular contract, we would first declare an interface instance as
follows:

```pint
{{#include ../../../../examples/ch_5_3.pnt:interface_instance}}
```

### External Storage Access Using the Interface Instance

Now that we have an instance of the interface, we can use it to create a path to the external
storage variable. For example,

```pint
{{#include ../../../../examples/ch_5_3.pnt:access}}
```

Note that the path `CounterInstance::storage::counter` has three parts:

1. The name of the interface instance `CounterInstance` that indicates which instance we would like
   to access. Recall that there could be multiple interface instances, with different addresses, for
   the same `interface`, hence the need to start the path with the name of the interface instance
   and not the name of interface itself.
1. The keyword `storage` to indicate that we're accessing the `storage` block of `CounterInstance`.
1. `counter`, the name of the storage variable we want to access.

Once we have assigned the external storage expression to a `state` variable, we can then use that
variable as we usually do.

Similarly to local storage access expressions, the expression `CounterInstance::storage::counter`
can only be used on the right-hand side of a `state` declaration.
