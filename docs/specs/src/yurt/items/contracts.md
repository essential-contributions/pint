### Contract Items

Contract items describe actual deployed contracts with a known contract ID and a list of available functions. Contract items require a known integer ID and a list of function signatures. Contract can also "inherit" functions from [interfaces](./interfaces.md). Contract items have the following syntax:

```bnf
<contract-item> ::= "contract" <ident> "(" <expr> ")"
                    [ "implements" <path> ( "," <path> )* ]
                    "{" ( <function-sig> ";" )* "}"
```

For example, consider the contract item below:

```yurt
contract MyToken(0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48) implements IERC20, Ownable {
    fn foo() -> int;
    fn bar() -> int;
}
```

This contract is called `MyToken` and has an integer ID of `0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48`. The contract has the following functions:

1. All the functions declared in the `IERC20` interface.
1. All the functions declared in the `Ownable` interface.
1. All the functions declared in the body of the contract, namely `foo()` and `bar()`.

A call to any of these functions can be made using a `<call-expr>` with the name of the contract used in `<path>`. For example, `MyToken::foo()`. Contract function calls _always_ return values which must be bound to [state variables](./states.md), such as `state u = MyToken::foo()`.
