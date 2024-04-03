## Types

Pint provides 4 scalar primitive types:

- Boolean: `bool`
- Integer: `int`
- Real: `real`
- String: `string`

Pint also provides tuples and arrays as a compound built-in type, as well as C-style enumeration types and custom types.

The syntax for types is as follows:

```bnf
<ty> ::= "bool"
       | "int"
       | "real"
       | "string"
       | <tuple-ty>
       | <array-ty>
       | <enum-ty>
       | <custom-ty>
```
