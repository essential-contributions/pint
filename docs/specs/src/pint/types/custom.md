### Custom type

Custom types may refer to any type and are declared using ["New Type" Items](../items/new_types.md). Custom types are referred to using a path:

```bnf
<custom-ty> ::= <path>
```

For example, `::path::to::MyNewType` may refer to some custom type called `MyNewType` if a [new type](../items/new_types.md) called `MyNewType` exists and is available in the scope of `::path::to::MyNewType`.
