## Data Types

Every value in Pint is of a certain _data type_, which tells Pint and solvers what kind of data is
being specified so they know how to work with that data. We'll look at two data type subsets: scalar
and compound.

Keep in mind that Pint is a _statically typed_ language, which means that it must know the types of
all variables at compile time. The compiler can often infer what type we want to use based on the
value and how we use it. In cases where many types are possible, we must add a type annotation as
described in the [chapter on decision variables](./variables.md).

### Scalar Types

A _scalar_ type represents a single value. Pint has three primary scalar types: integers, Booleans,
and 256-bit hashes.

#### Integer Type

An _integer_ is a number without a fractional component. Pint has a single integer type called `int`
which, when targeting the EssentialVM, represents a **64-bit signed integer** . An `int`, therefore,
can store numbers from $-(2^{n-1})$ to $2^{n-1} - 1$ inclusive, where `n` is the number of bits that
represent the integer (64 in the case of EssentialVM).

You can write integer literals in any of the forms shown in the table below. Note that number
literals can use `_` as a visual separator to make the number easier to read, such as `1_000`, which
will have the same value as if you had specified `1000`.

| Number literals | Examples      |
| --------------- | ------------- |
| Decimal         | `12_333`      |
| Hex             | `0x123f`      |
| Binary          | `0b1111_1101` |

#### Numeric Operations

Pint supports the basic mathematical operations you’d expect for integers: addition, subtraction,
multiplication, division, and remainder. Integer division truncates toward zero to the nearest
integer. The following code shows how you’d use each numeric operation in a `var` statement:

```pint
{{#include ../../../../examples/ch_3_2.pnt:math_ops}}
```

Note that binary operators in Pint are strict with regards to what types they allow, that is, only
identical types can be used in a binary operator. For example, adding an `int` and a `bool` is not a
valid operation and will result in a compile error.

#### The Boolean Type

As in most other programming languages, a Boolean type in Pint has two possible values: `true` and
`false`. The Boolean type in Pint is specified using `bool`. For example:

```pint
{{#include ../../../../examples/ch_3_2.pnt:bools}}
```

#### The `b256` Type

The `b256` is a special type that represents a 256-bit hash. It is often used to represent addresses
or storage keys and cannot be used as a numerical integers. That is, two `b256` values cannot added
for example.

A `b256` literals can be represented using a Hexadecimal or a Binary literal as follows:

```pint
{{#include ../../../../examples/ch_3_2.pnt:b256}}
```

### Compound Types

Compound types can group multiple values into one type. Pint has two primitive compound types:
tuples and arrays.

#### The Tuple Type

A tuple is a general way of grouping together a number of values with a variety of types into one
compound type. Tuples have a fixed length: once declared, they cannot grow or shrink in size.

We create a tuple by writing a comma-separated list of values inside curly brackets `{ .. }`. Each
position in the tuple has a type, and the types of the different values in the tuple don’t have to
be the same. We’ve added optional type annotations in this example:

```pint
{{#include ../../../../examples/ch_3_2.pnt:simple_tuple}}
```

The variable `tup` binds to the entire tuple because a tuple is considered to be a single compound
element. To get the individual values out of a tuple, we can use the period (`.`) operator followed
by the index of the value we want to access. For example:

```pint
{{#include ../../../../examples/ch_3_2.pnt:tuple_access}}
```

This program creates the tuple `tup_2` and then accesses each element of the tuple using its
respective index. As with most programming languages, the first index in a tuple is 0.

It is also possible to name some or all the fields of a tuple type as follows:

```pint
{{#include ../../../../examples/ch_3_2.pnt:simple_tuple_named}}
```

Note that, in this example, only two out of the 3 fields are named. In order to access the
individual elements of a tuple with named fields, the period (`.`) can again be used with either the
index of the tuple field or its name. For example:

```pint
{{#include ../../../../examples/ch_3_2.pnt:tuple_access_named}}
```

Tuples without any values are not allowed in `Pint`. That is, the following:

```pint
var empty: {} = {};
```

is disallowed and errors out as follows:

```console
Error: empty tuple types are not allowed
    ╭─[ch_3_2.pnt:43:12]
    │
 43 │ var empty: {} = {};
    │            ─┬
    │             ╰── empty tuple type found
────╯
Error: empty tuple expressions are not allowed
    ╭─[ch_3_2.pnt:43:17]
    │
 43 │ var empty: {} = {};
    │                 ─┬
    │                  ╰── empty tuple expression found
────╯
```

#### The Array Type

Another way to have a collection of multiple values is with an array. Unlike a tuple, every element
of an array must have the same type. Unlike arrays in some other languages, arrays in Pint have a
fixed length.

We write the values in an array as a comma-separated list inside square brackets:

```pint
{{#include ../../../../examples/ch_3_2.pnt:simple_array}}
```

You write an array's type using the element type followed by its size between square brackets, like
so:

```pint
{{#include ../../../../examples/ch_3_2.pnt:array_type}}
```

Here, `int` is the type of each element. The number `5` indicates that the array contains five
elements.

You can access elements of an array using by _indexing_ into it, like this:

```pint
{{#include ../../../../examples/ch_3_2.pnt:array_access}}
```

In this example, the variable named `c_first` will get the value `1` because that is the value at
index `0` in the array. The variable named `c_second` will get the value `2` from index `1` in the
array.
