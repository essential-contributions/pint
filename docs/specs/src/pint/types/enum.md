### Enum Type

An enum type refers to an [enum declaration](../items/enums.md) using its path and has the following syntax:

```bnf
<enum-ty> ::= <path>
```

For example, `::path::to::MyEnum` may refer to an enum called `MyEnum` if an enum called `MyEnum` exists and is available in the scope of `::path::to::MyEnum`.
