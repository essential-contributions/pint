## Type Aliases

Pint provides the ability to declare a type alias to give an existing type another name. For this we
use the `type` keyword. For example, we can create the alias `Balance` to `int` like so:

```pint
{{#include ../../../../../examples/type_aliases.pnt:alias}}
```

Now, the alias `Balance` is a synonym for `int`. Values that have the type `Balance` will be treated
the same as values of type `int`:

```pint
{{#include ../../../../../examples/type_aliases.pnt:alias_same}}
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
{{#include ../../../../../examples/type_aliases.pnt:simple_struct}}
```

To use a struct after we’ve defined it, we create an instance of that struct by specifying concrete
values for each of the fields. We create an instance using the same tuple expression syntax: curly
brackets containing _key: value_ pairs, where the keys are the names of the fields and the values
are the data we want to store in those fields. We don’t have to specify the fields in the same order
in which we declared them in the struct. In other words, the struct definition is like a general
template for the type, and instances fill in that template with particular data to create values of
the type. For example, we can declare a particular `User` as shown below:

```pint
{{#include ../../../../../examples/type_aliases.pnt:simple_struct_instance}}
```

To get a specific value from a struct, we use the dot notation similarly to tuples. For example, to
access this user's balance, we use `user1.balance`.
