#### Boolean Literals

Boolean literals have this syntax:

```bnf
<bool-literal> ::= "false" | "true"
```

#### Integer and Real literals

There are three forms of integer literals: decimal, hexadecimal, and binary:

```bnf
<int-literal> ::= [0-9]+
                | 0x[0-9A-Fa-f]+
                | 0b[0-1]+
```

For example: `1`, `0030`, `0x333`, `0b1010`.

Real literals have the following form:

```bnf
<real-literal> ::= [0-9]+"."[0-9]+
                  | [0-9]+"."[0-9]+[Ee][-+]?[0-9]+
                  | [0-9]+[Ee][-+]?[0-9]+
```

For example: `1.05`, `2.5e-4`, `1.3E5`.

A `-` preceding an integer or real literal is parsed as a unary minus, not as part of the literal.

#### String Literals

String literals are written as:

```bnf
<string-literal> ::= "\"" ([^"\n] | "\\" ("x" [0-9a-fA-F][0-9a-fA-F] | "n" | "t" | "\"" | "\\")) "\""
```

For example: `"Hello, world!\n"`.

String literals can be broken across multiple lines by escaping the newline and leading whitespace with a `\`. For example:

```pint
let mutli_line_string: string = "first line\
                                 second line\
                                 third line";
```
