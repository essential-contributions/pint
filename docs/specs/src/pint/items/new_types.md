### "New Type" Items

A "new type" item introduces a new custom type with a given name and an underlying type.

The syntax for declaring a new type is:

```bnf
<new-type-item> ::= "type" <ident> "=" <ty> ";"
```

For example:

```pint
type AccountTuple = { id: int, balance: real, address: string };

type IdArray = int[5];

type Address = string;
```

New type values may be initialized through:

- Expressions that match the new type's "structure", such as tuple and array expressions.

- Literals of primitive types (`int`, `real`, `bool`, `string`) as long as they are compatible with the new type's underlying definition.

For example:

```pint
let walletDetails: AccountTuple = {id: 1, balance: 2.0, address: "0x1234...ABCD"};

let ids: IdArray = [1, 2, 3, 4, 5];

let myAddress: Address = "0x1234567890abcdef";
```

New types are **not aliases**, meaning they are not completely interchangeable with their underlying type. That is, there is no implicit conversion between a new type and its underlying type (or other new types with the same underlying type) except when explicit literals, array expressions, or tuple expressions are used for initialization.
