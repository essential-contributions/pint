### Interface Items

Interface items contain lists of smart contract functions that a smart [contract](./contracts.md) can have.

Interface items describe lists of smart contract functions, in the form of function signatures, that a contract can have. An interface item has the following syntax:

```bnf
<interface-item> ::= "interface" <ident> "{" ( <function-sig> ";" )* "}"
```

For example, the following is a simple interface with 3 functions:

```yurt
interface IERC20 {
    fn totalSupply() -> int;
    fn balanceOf(account: int) -> int;
    fn allowance(owner: int, spender: int) -> int;
}
```

Interface functions are not callable directly. Instead, they have to be called through a [contract](./contracts.md).
