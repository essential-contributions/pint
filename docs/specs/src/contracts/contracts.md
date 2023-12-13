# Yurt Contracts

This chapter describes how Yurt can be used as a smart contract language for an intent-centric blockchain. The high level idea is that smart contracts are now written using a constraint programming language instead of an imperative programming language like Solidity or Sway. This makes the task of the validators much easier (just **verify** solutions against the constraints!) while offloading the bulk of the work to the solvers **off-chain**.

We will use the term "**intent**" to refer to user intents written in Yurt and the term "**contract**" to refer to smart contracts written in Yurt for an intent-centric blockchain.

While both an intent and a contract are written in the same language Yurt, they are actually different in a lot of ways. There are several language features that are only allowed in one of them but not the other. For example, declaring storage variables is only allowed in contracts.

## Structure of a Yurt contract

A Yurt contract contains the following items:

1. A list of storage variables that define persistent state required by the contract.
2. A list of contract messages.

For example, here's how an ERC20 Yurt contract might looks like:

```yurt
storage {
    balances: map<int, int>,
    total_supply: int,
    name: string,
    symbol: string,
}

contract ERC20 {
    owns { storage.balances } // Or something similar
    message transfer { to: int, value: int } {
        let from = context.sender();

        // -- Start with some safety check here --

        state from_bal = storage.balances[from];
        state to_bal = storage.balances[to];

        constraint from_bal - from_bal' == value;
        constraint to_bal' - to_bal == value;
    }

    owns { storage.total_suppy } // Or something similar
    message add_supply { amount: int } {
        state supply = storage.total_supply;
        constraint context.sender() == 0x0..01; // Address that is allowed to add supply
        constraint supply' == supply + amount;
    }
}
```

> **Note:** the method `context.sender()` provides the address of the account calling this contract.

Notice that there are no actual explicit writes to storage. However, there is an _implicit_ state update in the message `transfer`, for example, using the constraints:

```yurt
constraint from_bal - from_bal' == value;
constraint to_bal' - to_bal == value;
```

which imposes some requirements on the _next_ state using the "prime" (`'`) operator: we want the balance of the message sender (with address `context.sender()`) to go down by `value` and the balance of the receiver (with address `to`) to go up by `value`.

## Structure of a User Intent

A user intent is typically much simpler than a contract. A user intent is a collection of constraints that may or may not require interacting with a contract. For example, below is user intent that interacts with the contract above:

```yurt
use erc20::*;
contract erc20_instance = ERC20(0x111..111);

// Send half the current balance
let amount_to_transfer: int;
constraint amount_to_transfer == erc20_instance::balances[context.address()] / 2;

// Send an output message `transfer`
message transfer = erc20_instance::transfer;
constraint message.to == 0x222..2222;
constraint message.value == amount_to_transfer);

solve satisfy;
```

> **Note:** the method `context.address()` provides the address of the account submitting the intent.

The above is a simple intent that directly interacts with the contract and requests that a certain amount of tokens are transferred to another account.

A more subtle way of writing the intent above is as follows:

```yurt
use erc20::*;
let erc20_instance = ERC20(0x111..111);

state from_bal = erc20_instance::balance[context.address()]
state to_bal = erc20_instance::balance[0x222..222]

// Send half the current balance
let amount_to_transfer: int;
constraint amount_to_transfer == from_bal / 2;

constraint from_bal - from_bal' == amount_to_transfer;
constraint to_bal' - to_bal == amount_to_transfer;

solve satisfy;
```

The above still interacts with the ERC20 contract but does not use the `transfer` message directly. Instead, it encodes its constraints in the intent itself. This may give solvers more freedom when attempting to solve the intent.

## Solver Flow

Solvers receive the user intent and attempt to find assignment for all the decision variables that satisfy all the constraints. A solution is then proposed and later checked by the validators. When solving an intent, solvers will first resolve all state accesses and context calls.

For the first intent above, a solver will first write the equivalent of the following, but in the low level representation of an intent that will be defined by the protocol:

```yurt
// Send half the current balance
let amount_to_transfer: int;

constraint amount_to_transfer == storage.balances[context.sender()] / 2;

// Inline the message `transfer`
let to = 0x222..222;
let from = context.sender();

state from_bal = storage.balances[from];
state to_bal = storage.balances[to];

constraint from_bal - from_bal' == amount_to_transfer;
constraint to_bal' - to_bal == amount_to_transfer;

solve satisfy;
```

The solver is able to do this inlining because they have access to the contract's "bytecode" (i.e. the low level representation mentioned above).

Next, the solver will evaluate the current storage and context values via a special low level query APIs (similar to EVM's storage opcode but more complex - say SQL - and generated by the compiler). Assume that the current balances of the sender and the receiver are `1000` and `200`, respectively, and that the address of the sender is `0x333..333`.

```yurt
// Send half the current balance
let amount_to_transfer: int;

constraint amount_to_transfer == 1000 / 2;

let to = 0x222..222;
let from = 0x333..333;

// there are now known values
let from_bal = 1000;
let to_bal = 200;;

// New next state decision variables
let from_bal_next: int;
let to_bal_next: int;

constraint from_bal - from_bal_next == amount_to_transfer;
constraint to_bal_next - to_bal == amount_to_transfer;

solve satisfy;
```

The most important thing to notice in the above is that **this is now a constraint program with no state variables, no state accesses, and no context calls**. In can trivially™ be solved by a standard constraint solver (or maybe something even simpler).

A solution to the above may look like this:

```yurt
amount_to_transfer: 500
next(storage.balances[0x333..333]): 500 // New sender balance
next(storage.balances[0x222..222]): 700 // New receiver balance
```

The `next` values in the above are obtained from the decision variables `from_bal_next` and `to_bal_next`. The solver should, of course, keep track of what these decision variables mean so that they can convey the right information to the validators.

> **Note:** variables like `amount_to_transfer` may be skipped to reduce bandwidth since they are more like parameters than actual decision variables. In fact, other trivially known decision variables, such as `from_bal` and `to_bal`, are skipped here for simplicity. Solvers and validators need to have an agreement on how these variables are handled. Note, however, that not every (non-state) decision variable can be skipped in the general case.

The takeaway here is that **the task of the solver is to propose new state values (i.e. state transitions!) which is how state is updated in this model**. They also propose values for non-state decision variables to help the verification process by the validators.

## Validator Flow

Validators have a much simpler job than solvers as they don't have to deal with any of the complex solving algorithms. The first step for validators is similar to the first step by solvers where contract messages are inlined and storage accesses and context calls are evaluated:

```yurt
// Send half the current balance
let amount_to_transfer: int;

constraint amount_to_transfer == 1000 / 2;

// Inline the message `transfer`
let to = 0x222..222;
let from = 0x333..333;

let from_bal = 1000;
let to_bal = 200;

constraint from_bal - from_bal_next == amount_to_transfer;
constraint to_bal_next - to_bal == amount_to_transfer;
```

> **Note:** the `solve` directive is gone because no solving is required here.

The validators then receive the solution proposed by the solver and "plug it in" where appropriate in all three constraints:

```yurt
constraint 500 == 1000 / 2;
constraint 1000 - 500 == 500;
constraint 700 - 200 == 500;
```

All these constraints are trivially satisfied so the validators are happy and the state transition is finalized and updated.
