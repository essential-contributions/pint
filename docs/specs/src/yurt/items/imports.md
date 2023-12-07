### Import Items

Within a scope, import items create shortcuts to items defined in other files. Import items have the following syntax:

```bnf
<use-path> ::= [ "::" ] ( <ident> | "self" ) ( "::" ( <ident> | "self" ) )*
<use-tree> ::= [ [ <use-path> ] "::" ] "*"
             | [ [ <use-path> ] "::" ] "{" [ <use-tree> "," ... ] "}"
             | <use-path> [ "as" <ident> ]

<import-item> ::= "use" <use-tree> ";"
```

An import item creates one or more local name bindings synonymous with some other path. Usually a `use` item is used to shorten the path required to refer to a module item. These items may appear in modules, usually at the top. **An item must be imported _before_ it can be used**. This is the only instance in Yurt where the order of items is important.

"Use" items support a number of convenient shortcuts:

- Simultaneously binding a list of paths with a common prefix, using the brace syntax `use ::a::b::{c, d, e::f, g::h::i};`
- Simultaneously binding a list of paths with a common prefix and their common parent module, using the `self` keyword, such as `use a::b::{self, c, d::e};`.
- Rebinding the target name as a new local name, using the syntax `use p::q::r as x;`. This can also be used with the last two features: `use a::b::{self as ab, c as abc};`.
- Nesting groups of the previous features multiple times, such as `use ::a::b::{self as ab, c, d::{e, f::g}};`.
