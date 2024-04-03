### Enum Declaration Items

In Pint, an enum type is a named enumeration of integer constants. Unlike sum types found in some functional languages, each member of an enum in Pint is associated with an integer discriminant, making it similar to C-style enums. The syntax for declaring an enum is:

```bnf
<enum-decl-item> ::= "enum" <ident> "=" <ident> ( "|" <ident> )* ";"
```

For example, `enum Colour = Red | Green | Blue;` declares an enum with three variants. An instantiation of a `Colour` can be created using a path that includes the name of the enum, as in `Colour::Green;`.

Each enum variant is assigned a discriminant that matches the index of its location, starting with `0`, in the sequence of variants as they appear in the enum declaration. An enum variant can be converted to an integer that is equal to its assigned discriminant using [`as`](../expressions/atoms/casts.md).
