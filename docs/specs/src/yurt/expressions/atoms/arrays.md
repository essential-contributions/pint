#### Array Expressions and Array Element Access Expressions

Array expressions are written as:

```bnf
<array-expr> ::= "[" ( <expr> "," ... ) "]"
```

For example: `let a = [ 1, 2, 3 ];`. The type of this array can be inferred by the compiler to be `int[3]`.

The following is another example:

```yurt
let b: real[2][3] = [ [ 1.0, 2.0, 3.0], [4.0, 5.0, 6.0] ];
```

where the type of the array is indicated by the type annotation. Note that the initializer of `b` is an array expression that contains other array expressions to reflect the fact that `b` is two dimensional. Also note that `real[2][3]` is an array that contains 2 elements where each element is an array of size 3, and not the other way around.

The grammar disallows empty array expressions `[ ]` because arrays of size 0 are not allowed.

All element expressions used in an array expression must have the exact same type and that type must match the type indicated in the array type annotation, if available.

Array element access expressions are written as:

```bnf
<array-element-access-expr> ::= <expr> ( "[" expr "]" )+
```

For example, `a[1];` refers to the second element of array `a` in the example above. Therefore, `a[1]` should be equal to `2`. Similarly, `b[0][2]` in the example above refers to the third elements from the first inner array of `b`. That is, `b[0][2]` should be equal to `3.0`.

Yurt requires that the expression used to index into is an array. For example, in `foo()[5]`, `foo()` must return an array of an appropriate size.

Yurt also requires that each array index is known (i.e. evaluatable) at compile-time. In addition, Yurt requires that each index evaluates to:

1. A **non-negative** integer that is **strictly smaller** than the corresponding dimension (i.e. within bounds), if the dimension has a size that is specified using an integer in the array type definition.
1. A path to an enum variant of some enum type, if the dimension size is specified using that enum type in the array type definition.

Below is an example where both an integer and an enum variant are used to index into a two-dimensional array:

```yurt
let N = 5;
enum Colour = Red | Green | Blue;
let a: real[N][Colour];`

let a_3_g = a[3][Colour::Green];
```
