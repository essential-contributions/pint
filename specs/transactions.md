# Transactions

A transaction is a cryptographically signed message that changes the state of a blockchain. Solving an intent boils down to finding a sequence of transactions (aka an execution trace) that satisfies the intent. In order to express intents using our DSL, we need to be able to model transactions using the primitive types offered by the language such as integers, floats, etc.

We will limit the scope of this document to [Ethereum transactions](https://ethereum.org/en/developers/docs/transactions/), but the concepts discussed should apply to most, if not all, transaction formats.

## Modeling Transaction Fields

An Ethereum transaction includes several fields with different data types. We will discuss each field separately and show how to model it using our DSL.

### `from`

This is the address of the sender and will usually be the address of the user (or contract) submitting the intent and is _not_ a decision variable. However, we still need to model this address using the DSL because it will be required for specifying a state access list.

### `recipient`

This is the receiving address which could be an externally-owned account (EOA) or a contract. This will usually be a decision variable. That being said, it is not reasonable to expect solvers to search through all possible $2^{160}$ 20-byte addresses when solving for an execution trace. Instead, solvers need to impose additional constraints on `recipient` that restrict the search space as much as possible. For example, a solver may decide to include a list of CFMM contract addresses in their search space. More addresses could be added to the search space by inspecting the submitted intent (e.g. address `b` in [Example 1](introduction.md#example-1)).

### `value`

This is the amount of `Eth` to transfer from `sender` to `recipient`. This value is a decision variable and can simply be modeled with a `float` variable. The result can then be rounded down to an integer in the final solution.

### `gasLimit`, `maxPriorityFeePerGas`, `maxFeePerGas`

These are all parameters related to the gas fees spent by the transaction and can be modeled using `float` variables. The results can then be rounded down to integers in the final solution. A user may or may not care about restricting these but ultimately the protocol should attempt to minimize the transaction cost.

### `nonce`

This is a sequentially incrementing counter which indicates the transaction number from the account. This is not a decision variable and does not need to be modeled.

### `data`

The data field is only relevant for transactions that access a contract:

1. The first four bytes specify which contract function to call.
1. The rest of the data represent the arguments [encoded as specified in the ABI specs](https://docs.soliditylang.org/en/latest/abi-spec.html#formal-specification-of-the-encoding).

Searching through arbitrary bytes is, of course, not reasonable. Instead, each feasible `recipient` has its own valid data field "template" which restricts its own search space. For each valid function (and therefore a function selector), valid function arguments can themselves be decision variables that contribute to the `data` field. For example, the values of `amountOut` and `amountInMax` in the Uniswap V2 router contract method [`swapTokensForExactTokens`](https://github.com/Uniswap/v2-periphery/blob/master/contracts/interfaces/IUniswapV2Router01.sol#L68-L74) can be decision variables (of type `float`) _when_ `recipient` is the address of the Uniswap V2 router contract.

## Use Model

In most cases, users will not care about limiting any transaction parameters, except potentially for transaction costs. Most of the fields described in [Modeling Transaction Fields](#modeling-transaction-fields) need to be constrained by the solver, particularly the `recipient` and the `data` fields. Solver will compete in how "wide" they can make their search spaces (e.g. support more CFMMs) while still being able to solve for a satisfactory execution trace efficiently.
