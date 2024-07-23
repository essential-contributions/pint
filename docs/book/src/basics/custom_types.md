## Custom Types

Custom data types are named types that you can define in your program to refer, via an alias, to a
primitive type, a compound type, or an `enum`. Enums are another special class of custom types that
define enumerations with named variants.

Both custom type and enum declarations must be made at the top level of a module, and are not
allowed inside a `predicate` declaration.

### Type Aliases

Pint provides the ability to declare a type alias to give an existing type another name. For this we
use the `type` keyword. For example, we can create the alias `Balance` to `int` like so:

```pint
{{#include ../../../../examples/ch_3_5.pnt:alias}}
```

Now, the alias `Balance` is a synonym for `int`. Values that have the type `Balance` will be treated
the same as values of type `int`:

```pint
{{#include ../../../../examples/ch_3_5.pnt:alias_same}}
```

Because `Balance` and `int` are the same type, we can compare values of both types.

### Defining structs using `type`

Many programming languages offer the concept of a "struct" which lets you **package** together and
**name** multiple related values that make up a meaningful group. While Pint does not offer a
special `struct` construct, it does offer a way to name a tuple, name its fields, and access its
elements as if it were a `struct`.

To define a struct-like tuple (which we will just call a struct going forward), we use the `type`
keyword followed by the name chosen for the tuple. We then use the `=` operator to bind the new type
name to a tuple type with all of its fields named.

For example:

```pint
{{#include ../../../../examples/ch_3_5.pnt:simple_struct}}
```

To use a struct after we’ve defined it, we create an instance of that struct by specifying concrete
values for each of the fields. We create an instance using the same tuple expression syntax: curly
brackets containing _key: value_ pairs, where the keys are the names of the fields and the values
are the data we want to store in those fields. We don’t have to specify the fields in the same order
in which we declared them in the struct. In other words, the struct definition is like a general
template for the type, and instances fill in that template with particular data to create values of
the type. For example, we can declare a particular `User` as shown below:

```pint
{{#include ../../../../examples/ch_3_5.pnt:simple_struct_instance}}
```

To get a specific value from a struct, we use the dot notation similarly to tuples. For example, to
access this user's balance, we use `user1.balance`.

### Enumerations

Enums allow you to define a type by enumerating its possible variants. Where structs and tuples give
you a way of grouping together related fields and data, like a `User` with its `status`, `address`,
and `balance`, enums give you a way of saying a value is one of possible set of values. For example,
we may want to say that `User` is one of a set of possible account types that also includes
`Contract`. To do this, Pint allows us to encode these possibilities as an `enum`.

Let’s look at a situation we might want to express in code and see why enums are useful. Say we need
to work with three possible tokens: DAI, USDC, and USDT. Because these are the only tokens we want
to work with, we can enumerate all possible variants, which is where enumeration gets its name:

```pint
{{#include ../../../../examples/ch_3_5.pnt:simple_enum}}
```

Note how the possible variants of `Token` are separated by a `|`. `Token` is now a custom data type
that we can use elsewhere in our code. Also, we can now create an instance of each of the three
variants of `Token` like this:

```pint
{{#include ../../../../examples/ch_3_5.pnt:simple_enum_instances}}
```

Note that the variants of the enum are namespaced under its identifier, and we use a double colon to
separate the two. This is useful because now all three values `Token::DAI`, `Token::USDC`, and
`Token::USDT` are of the same type: `Token`. We can then, for instance, declare a variable called
`token_type` to be of type `Token` and assign it to either variants depending on how large some
value `amount` is.

```pint
{{#include ../../../../examples/ch_3_5.pnt:enum_selection}}
```

We can even use enums inside structs as follows:

```pint
{{#include ../../../../examples/ch_3_5.pnt:enum_in_struct}}
```
