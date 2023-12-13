# JSON Representation of Intents

Each intent in the network of solver agents is represented using a JSON object generated by the compiler. The JSON spec below aims to standardize intents so that multiple languages and multiple solvers can be built around it. This will help widen the ecosystem of intent-based architectures.

## JSON Spec

Each intent is represented as a JSON object containing the following properties:

- `"from"`: a string containing the address of the intent submitter.
- `"variables"`: an array of all the decision variables that the solver of an intent needs to search for. Each variable is represented using a JSON object with the following properties:
  - `"name"`: a string containing the name of the variable.
  - `"type"`: a string containing the type of the variable. Available types are `"bool"`, `"int"`, and `"float"`.
  - `"stateAccessed"`: ....
- `"constraints"`: an array of all the constraints that describe the intent. Each constraint is an array that contains the following elements:
  - The first element is `"Equal"` or `"LessEqual"` indicating whether the constraint is an equality or an inequality constraint, respectively.
  - The second and third elements are _expressions_ describing both sides of the equality or inequality constraint. Each expression is one of the following:
    - A number literal. Refer to [Number Literals](#number-literals) for a list of all possible number representations.
    - A string representing the name of one of the decision variables above.
    - An array containing the following:
      - A string representing an operator. Refer to [Operators](#operators) for a list of all available operators.
      - The arguments of the operators as _expressions_. The format of each expression follows the rules described in this section.
- `"stateTransitions"`: An array of all state transitions that describe the intent. Each state transition is an array that contains two strings:
  - The first string is the name of a decision variable.
  - The second string is the name of the decision variable that represents the _next state_ of the first decision variable.

> **Note**
> The list of constraints can be generalized to include arbitrary Boolean expressions.

> **Note**
> The spec above is limiting in a few ways and should eventually be generalized once we have a better understanding of the problem domain. For example, constraints on transactions and intermediate states are not possible yet.

### Number Literals

Number literals can be one of the following:

- A JSON number, e.g. `4`, `1.4`, `3.2e10`.
- A string containing a hexadecimal number, e.g. `"0x00000000"`.
- One of the following strings representing special numbers:

| Constant         | Value              |
| ---------------- | ------------------ |
| `"ExponentialE"` | $e = 2.71828...$   |
| `"Pi"`           | $\pi = 3.14159...$ |

### Operators

The following is a list of supported operators:

| Operator     | Arguments | Operation     |
| ------------ | --------- | ------------- |
| `"Add"`      | `a`, `b`  | $a + b$       |
| `"Subtract"` | `a`, `b`  | $a - b$       |
| `"Multiply"` | `a`, `b`  | $a * b$       |
| `"Divide"`   | `a`, `b`  | $\frac{a}{b}$ |
| `"Power"`    | `a`, `b`  | $a^b$         |
| `"Sqrt"`     | `a`       | $\sqrt{a}$    |
| `"Exp"`      | `a`       | $e^a$         |
| `"Ln"`       | `a`       | $\ln(a)$      |
| `"Log"`      | `a`, `b`  | $\log_b(a)$   |

## A Simple Example

Below is a simple example showing how the JSON object for the [simple swap intent](../intro/introduction.md#example-2) from the [introduction](../intro/introduction.md) would look like:

```json
{
  "from": "0x1111111111111111111111111111111111111111",
  "variables": [
    {
      "name": "eth",
      "type": "float",
      "stateAccessed": {
        "account": "0x1111111111111111111111111111111111111111",
        "value": "balance"
      }
    },
    {
      "name": "dai",
      "type": "float",
      "stateAccessed": {
        "account": "0x6B175474E89094C44Da98b954EedeAC495271d0F",
        "value": "call balanceOf(0x1111111111111111111111111111111111111111)"
      }
    },
    {
      "name": "eth_next",
      "type": "float"
    },
    {
      "name": "dai_next",
      "type": "float"
    }
  ],
  "constraints": [
    ["Equal", ["Subtract", "eth", "eth_next"], "3e18"],
    ["LessEqual", "5400000000", ["Subtract", "dai_next", "dai"]]
  ],
  "stateTransitions": [
    ["eth", "eth_next"],
    ["dai", "dai_next"]
  ]
}
```