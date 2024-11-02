## Comments

All programmers strive to make their code easy to understand, but sometimes extra explanation is
warranted. In these cases, programmers leave comments in their source code that the compiler will
ignore but people reading the source code may find useful.

Here’s a simple comment:

```pint
{{#include ../../../../examples/comments.pnt:one_line_comment}}
```

In Pint, the only comment style supported starts a comment with two slashes, and the comment
continues until the end of the line. For comments that extend beyond a single line, you’ll need to
include `//` on each line, like this:

```pint
{{#include ../../../../examples/comments.pnt:multi_line_comment}}
```

Comments can also be placed at the end of lines containing code:

```pint
{{#include ../../../../examples/comments.pnt:eol_comment}}
```

But you’ll more often see them used in this format, with the comment on a separate line above the
code it’s annotating:

```pint
{{#include ../../../../examples/comments.pnt:pre_comment}}
```
