# Contents

- [WHAT?](#what)
- [WHY?](#why)
- [HOW?](#how)
    - [Summary of general areas of concern:](#summary-of-general-areas-of-concern)
        - [Control flow](#control-flow)
        - [Pointer management](#pointer-management)
        - [Stack management](#stack-management)
        - [General optimisations](#general-optimisations)
        - [Readability](#readability)
    - [Control flow](#control-flow-2)
    - [Memory/pointer management](#memorypointer-management)
    - [Stack management](#stack-management-2)
    - [General optimisations](#general-optimisations-2)
    - [Readability](#readability-2)
- [DISCUSSION](#discussion)

_These notes are for while the IR is WIP and will be deleted before merge._

# WHAT?

A new intermediate representation (IR) layer between AST level transforms and ASM generation.

# WHY?

Generating ASM for a stack machine VM requires complicated juggling of values on and off the stack.
The VM also has several sources of data: storage, predicate params, memory and the stack itself.
This complexity makes generating valid ASM _directly from an AST_ hard in the general case and
especially complicated for larger Pint expressions and data structures.

On top of initial ASM gen, performing optimisations is almost impossible.

# HOW?

## Summary of general areas of concern:

1. Control flow.
2. Memory/pointer management.
3. Stack management.
4. General optimisations.
5. Readability.

### Control flow

Performing optimisations on code with hard-coded control flow can easily lead to errors.

Using basic-blocks for control flow delineates areas for optimisations and allows jump offsets to be
calculated in the epilogue.

**Where is it needed?**

Which expressions?
Which ops?

**What features?**

Are arguments required?
Do they need to be re-ordered?

### Pointer management

Handling values coming from different memories is a subtle art.  There's no need to copy something
to the stack from memory and then to copy it back.  There's no need to copy a large data structure
from param data into memory to then just read a single word from it.

Using abstract opaque pointers for each memory removes the need to make assumptions on how they
should be used.

**ptr and GEP?**

What sort of API is required for manipulating opaque pointers?

**Are opaque pointers truly viable?**

There are obviously times we need values on the stack.
What would a program using opaque pointers actually look like?
Can they always be optimised?

### Stack management

Certain operations like morphisms require maintaining a local context.  These can be stored on the
stack (or alternatively and less efficiently in memory) but entering and leaving those contexts
need structure.

Goes hand-in-hand with pointer management, as the context is on the stack and must be freed upon
leaving, so ptrs can't be left to the stack for larger expression results.

### General optimisations

_CSE_

e.g. `x + x` should generate `x` once and duplicated it rather than generating `x` twice.
e.g. a[2] + a[4] should generate `a` once.

_Peephole_

e.g. `push 1; mul` or `push 0; add` can be removed.
e.g. `push x ; pop` can be removed.
e.g. `push x ; push x` can be `push x; dup`; and `push 0 ; dupf` can be `dup`.

### Readability

Reading the output of ASM gen for debugging is very useful.  Having symbolic control flow and
comments makes the output more readable.

## Control flow

_todo_

## Memory/pointer management

_todo_

## Stack management

_todo_

## General optimisations

_todo_

## Readability

_todo_

# DISCUSSION



%% vim:foldlevel=3
