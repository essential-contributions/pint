### Namespaces

All names declared at the top-level of a Yurt file belong to a single namespace. It includes the following names:

1. All variable names.
2. All function names.
3. The names of all user-defined types.

[import item](../items/imports.md) used in a Yurt file import new names to its namespace.

### Name Shadowing

Name shadowing in Yurt is not allowed. For example, the following should fail to compile:

```yurt
let x = 5;
let x = 6;
```

```yurt
let x = 5;
state x = MyPath::bar();
```

Note, however, that [macro](../items/macros.md) bodies **are allowed** to declare new variables with names that have been used outside the macro body, because macro expansion is designed to be [hygienic](../items/macros.md#hygiene). For example, the following is allowed:

```yurt
let half = 5;

macro @is_even($a) {
    let half: int;
    constraint $a == half * 2; // `half` here refers to the one declared inside the macro
}
```
