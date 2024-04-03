### Array Type

An array type represents a collection of items that share the same type. Arrays can be multi-dimensional and have the following syntax:

```bnf
<array-ty> ::= <ty> ( "[" <expr> | <path> "]" )+
```

An array dimension can be indexed using an expression of type `int` which has to be non-negative. It can also be indexed using a `<path>` that represents an enum variant of some enum type or some [new type](../items/new_types.md) that resolves to an enum type. The allowed type of the index depends on how the dimension is specified in the array type definition.

- An array dimension that can be indexed using an integer requires that the corresponding dimension size is specified in between brackets as an expression that is evaluatable, **at compile-time**, to a **strictly positive** integer. Otherwise, the compiler should emit an error.

- An array dimension that can be indexed using an enum variant requires that the corresponding dimension size is specified in between brackets as the appropriate enum type.

For example, in:

```pint
let N = 5;
enum Colour = Red | Green | Blue;
let a: real[N][Colour];`
```

`a` is a two dimensional array that contains `N` arrays of size 3 each (because `Colour` is an enum that has 3 variants). An element of `a` can be [accessed](../expressions/atoms/arrays.md) using a syntax similar to `a[3][Colour::Green]`, which accesses the second element of the fourth array in `a`.
