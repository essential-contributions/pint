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
- [PROGRAM GRAPH](#program-graph)
- [DISCUSSION](#discussion)
- [SCRATCH](#scratch)

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

e.g. `push 1; mul` or `push 0; add` can be removed. _Or never added, detected in SSA._
e.g. `push x ; pop` can be removed.
e.g. `push x ; push x` can be `push x; dup`; and `push 0 ; dupf` can be `dup`.

### Readability

Reading the output of ASM gen for debugging is very useful.  Having symbolic control flow and
comments makes the output more readable.

----------------------------------------------------------------------------------------------------

## General approach

It looks like two stages would be useful, or three if we include final ASM gen.

1. Generate SSA graph with `if/else` and `repeat` control flow blocks, resulting in `constraint`
   values which are a bit like `return` for functions.
2. Convert to VM ASM wrappers for most values, still with structured control flow.
3. Convert structured control flow to jumps and reps; remove comments.

----------------------------------------------------------------------------------------------------

## Control flow

_Needed by `?:`, `&&` and `||`._

These are special cases where an expression is determined to _not_ be evaluated.  But they don't
imply any other true control flow and are relatively insular.

Could be represented by `if`/`else` construct, as opposed to truly general labels/branches.

When the unevaluated expression is simple/cheap (e.g., an immediate) then control flow isn't needed;
use `SEL` instead.

_Also needed by morphisms._

The body of a loop, a simple repeat with no special requirements.

----------------------------------------------------------------------------------------------------

## Memory/pointer management

An SSA value can be essentially a pointer, to stack or memory.  The type of the value isn't
necessary, only its size.

Immediates larger than a word (array or tuple expressions, union variants) can be stored into memory
all at once at the start of the program?  Slightly more efficient than multiple stores.  Base index
to stored values would need to be stashed. **Better would be a .data section.**

A GEP indexes a value in a pointer value.  Would end up on the stack.

We can copy value to memory, or between memory.  Values are generally immutable though, so copying
within memory shouldn't be necessary.

How is this lowered to ASM?

### Compiling

Every value has a location:
- immediates on the stack
- ASM ops:
    - on the stack except for:
    - stores (move from stack to memory)
    - storage reads
    - some paths
    - parent node results (probably a path)
- paths to params

So?
Pass 1: Assume all values are on stack.
Pass 2: Merge GEPS to minimise size of values.
Pass 3: Values larger than 1 must be put into memory with STOR and LODR.
Pass 4: Optimise memory accesses, minimise copying.

----------------------------------------------------------------------------------------------------

## Stack management

Enter/leave construct?  The key principle is that stack needs to be balanced between the two.  If
the inner expression returns a single value then it can be juggled.  Otherwise it must go to memory.

How is this lowered to ASM?

----------------------------------------------------------------------------------------------------

## General optimisations

Yes.

For _CSE_ any named symbol _which is used more than once_ needs to be recognised, put in an SSA
value and also reused.  We ensure that explicit reuse is definitely employed.  Implicit reuse is
trickier, obviously, and I guess uses the typical compiler CSE techniques.

----------------------------------------------------------------------------------------------------

## Readability

Comments all the way until they're optimised out, finally discarded at very final ASM.

----------------------------------------------------------------------------------------------------

# PROGRAM GRAPH

Predicates are broken into nodes in a graph; currently we put `let`s in to nodes of their own, and
`constraint`s are put into leaves.

The point of the graph is to encourage concurrency and parallel computation.  Splitting nodes can
therefore be more judicious.

* Working backwards from each constraint its dependencies (the simple values it uses) can be
  determined.
* If a dependency is used only by a single constraint then it may be computed in the same leaf node
  as that constraint.
* If a dependency is used by many constraints then it, and _its_ dependencies may go in a separate
  node.
* These dependencies can be discovered quite easily when using SSA form.

Boolean `constraint`s can also be joined in leaf nodes with `&&`.

Assignment `constraint`s are joined using `++` logically, and into the final state update
ultimately.

Conditional `match` statements are lowered to `if/else`.  These must be high level control flow
structures so leaf nodes may be constructed properly.  Boolean `constraint`s must have the `if/else`
predicate included in the leaf node.  Assignment `constraint`s must be merged into state updates
based on the predicate.

# DISCUSSION

_Comments here._

# SCRATCH

**All return pointers, or SSA values:**

  _Immediates (const) go on stack if 1 word, memory otherwise:_
  Nil, Immediate

  _Can be pred param, memory location or morphism param. Also stack or memory?_
  Path

  _Aggregates go in memory, but are elements inlined? Aggregates of aggregates should probably have
  pointers unless they can be inlined (and not copied). But must be contiguous for node/parent
  stuff?_
  Array, Tuple, UnionVariant

_Take a pointer and GEP into it:_
Index, TupleFieldAccess, UnionTag, UnionValue

_Ops can wrap the ASM equiv? Except logical AND/OR which require control flow._
UnaryOp, BinaryOp

_Needs control flow if branch can panic.  Perhaps if expenseive.  Heuristic to decide?_
Select

_Complicated morphism; needs local context; indexes into array; result is array in memory._
Map

_Special ASM:_
LocalPredicateCall, ExternalPredicateCall, IntrinsicCall

_ASM black box._
AsmBlock

_Lowered, not compiled:_
Error, Cast, In, Range, Generator, Match, MacroCall, LocalStorageAccess, ExternalStorageAccess

# SOUP OF NODES

Good option for an already defined IR format.  Focuses on dataflow and control flow as separate
edges in the same graph.

Simple, a tutorial: https://github.com/SeaOfNodes/Simple/tree/main

## Notes

Stuff from the tutorial which might not apply for Pint:

_All the premature optimisations._

_The type system._

_Projections and multinodes?_

Projection nodes are GEPs.  But they project into multinodes, which seem to be just a vec of indices
to other nodes, rather than a different specific tuple data structure.

%% vim:foldlevel=3
