## Overview of an Intent Model

Conceptually, a Pint program has two parts:

1. The _intent model_: the main part of the program, which describes the structure of a particular class of intents.
1. The data: the input data for the model, which specifies one particular intent with this class of intents.

The pairing of a model with a particular data set is an _intent model instance_.

The model and data may be separated, or the data may be "hard-wired" into the model.

There are two broad classes of intents: satisfaction and optimization. In satisfaction intents all solutions are considered equally good, whereas in optimization intents the solutions are ordered according to an objective and the goal is to find a solution whose objective is optimal. [Solve Items](items/solve.md) specifies how the class of intent is chosen.
