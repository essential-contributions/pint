## Custom Types

Custom data types are named types that you can define in your program to refer, via an alias, to a
primitive type, a compound type, or a `union`. Unions are another special class of custom types that
allow you to define a type by enumerating its possible _variants_.

Both type aliases and union declarations must be made at the top level of a module, and are not
allowed inside a `predicate` declaration.

This chapter covers type aliases and unions. It also covers `match` statements which help reason
about unions and their variants.
