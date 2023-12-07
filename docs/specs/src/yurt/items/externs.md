### "Extern" Items

"Extern" items contain lists of function signatures that represent external APIs that can access data on a blockchain. More specifically, for the Ethereum blockchain, the functions should match the [JSON-RPC methods](https://ethereum.github.io/execution-apis/api-documentation/) that all Ethereum clients must implement.

The syntax for extern items is as follows:

```bnf
<extern-item> ::= "extern" "{" ( <function-sig> ";" )* "}"
```

For example:

```yurt
extern {
    fn eth_getBalance(address: string) -> string;

    fn eth_gasPrice() -> string;
}
```

The types used in the signature of `extern` functions depend on the types used by the external APIs. In the case of Ethereum JSON-RPC, a string is used to encode all values, hence a `string` type must be used in the `extern` block.

Extern functions are available directly without any special scoping. The only requirement is that the functions are called in the same file where the `extern` block is declared or that the functions are imported using an [import item](../items/imports.md), similarly to regular functions.
