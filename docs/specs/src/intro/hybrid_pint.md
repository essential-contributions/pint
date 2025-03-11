# Hybrid Pint

## Current Design of Pint

A Pint program is a contract that contains a set of predicates. Each predicate can be written as a
Boolean function `P`:

$$
P(d, s, s')
$$

where:

1. $d$ is the predicate data (aka decision variables)
1. $s$ is the current state of the blockchain (always known)
1. $s'$ is the next state of the blockchain (also to be found)

Given a predicate $P$, there are two flows that use $P$:

### Solver Flow\*\*

Because $s$ is the current state of the blockchain, it is known to everyone including solvers.
Therefore, what remains is to solve the following constraint program:

$$
\begin{aligned}
& \text{find}       && d, s' \\
& \text{subject to} && P(d, s, s') = 1
\end{aligned}
$$

### **Validation Flow**

During validation, a solution $(d, s')$ is provided which means that all the inputs to $P$ are
known, because $s$ is always known. Therefore, the validation flow simply checks that $P(d, s, s') =
1$

## Proposed Hybrid Approach

One drawback of the approach above is that it requires publishing both $d$ and $s'$. In general,
there will be multiple values for $s'$ that satisfy all the constraints. Alternatively, a predicate
may look like this:

$$
P(d, s, s') \wedge (s' = f(d, s))
$$

what this means is that, while $P$ can still be described as a function of $d$, $s$, and $s'$, we
also require that $s'$ can be written entirely in terms of $d$ and $s$ using some function $f$.

Here's how the two flows above would now look like:

### **New Solver Flow**

The solver now solves this constraint program:

$$
\begin{aligned}
& \text{find}       && d, s' \\
& \text{subject to} && P(d, s, s') = 1 \\
&                   && s' = f(d, s)
\end{aligned}
$$

which is _separable_. The following program can be solved first:

$$
\begin{aligned}
& \text{find}       && d \\
& \text{subject to} && P(d, s, f(d, s)) = 1
\end{aligned}
$$

and then $s'$ can be easily computed as $s' = f(d, s)$.

### **New Validation Flow**

During validation, only $d$ is now required! What this means is that the validators are only
required to compute $s'$ by evaluating $f(d, s)$ and then checking that $P(d, s, s') = 1$.

## Implications on the Design of Pint

Pint currently allows writing code in either of the forms above. However, the hybrid approach would
require some static analysis to ensure that its properties are valid.
