## Notation

The specification of the Yurt programming language presented in this document follows the [BNF format](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form). The basics of the BNF used are as follows:

- Non-terminals are written between angle brackets, e.g. `<item>`.
- Terminals are written in double quotes, e.g. `"solve"`.
- Optional items are written in square bracket, e.g. `[ "," ]`
- Sequences of zero or more items are written with parentheses and a star, e.g. `( "," <ident> )*`
- Sequences of one or more items are written with parentheses and a plus, e.g. `( "," <ident> )+`.
- Non-empty lists are written with an item, a separator/terminator terminal, and three dots. For example, `<expr> "," ...` is short for `<expr> ( "," <expr> )* [ "," ]`. The final terminal is always optional in non-empty lists.
