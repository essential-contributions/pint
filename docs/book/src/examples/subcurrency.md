## Subcurrency

The following contract implements the simplest form of a cryptocurrency. The contract allows the
creation of new coins (i.e. minting) as well as sending coins from one address to another.

```pint
{{#include ../../../../examples/subcurrency.pnt}}
```

This contract introduces some new concepts. Let's walk through it line by line.

The contract starts with a `storage` declaration that contains two storage variables:

1. `total_supply` is of type `int` and represents the total supply of coins available at any given
   point in time.
1. `balances` is a **map** from `b256` to `int` and stores balances of addresses as integers. `b256`
   is a primitive type that represents a 256-bit hash value and is used here to represent an
   address.

The contract also declares two intents: `Mint` and `Send`.

The `Mint` intent is fairly simple:

1. It declares two decision variables `receiver: b256` and `amount: int`. The goal of this intent to
   mint `amount` coins and send them to `receiver`.
1. It initializes a `state` variable called `receiver_balance` using the storage access expression
   `storage::balances[receiver]`. This syntax returns the value in `balances` that `receiver` maps
   to. The intent also initializes another state variable called `total_supply` to
   `storage::total_supply`.
1. It enforces two constraints:
   1. the first constraint requires the total supply to be incremented by `amount`.
   1. The second constraint requires the balance of `receiver` to be incremented by `amount`.

The `Send` intent has the following structure:

1. It declares three decision variables `from: b256`, `receiver: b256`, and `amount: int`. The goal
   of this intent to send `amount` coins from address `from` to address `receiver`.
1. It initializes a `state` variable called `from_balance` to the balance of `from` and another
   variable called `receiver_balance` to the balance of `receiver`.
1. It enforces three constraints
   1. the first constraint requires `from_balance` to be larger than `amount`. That is, the address
      `from` must currently actually have enough coins to send to `receiver`.
   1. The second constraint effectively decrements the balance of `from` by `amount`, by requiring
      the next state of `from_balance` to be `from_balance - amount`.
   1. The third constraint effectively increments the balance of `receiver` by `amount`, by
      requiring the next state of `receiver_balance` to be `receiver_balance + amount`.

> **Note** to make things simpler and easier to understand, this contract has no _authentication_
> anywhere in its code. That is, anyone can mint new coins and initiate a transfer from one
> arbitrary address to another. This, of course, is not the desired behavior for most
> cryptocurrencies. That being said, we will cover authentication in a later chapter and discuss how
> to use authentication make this contract more secure.
