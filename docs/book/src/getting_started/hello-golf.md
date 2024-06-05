## Hello, Golf!

Let's make things more interesting! While the "Hello, world!" program is a good introductory example for starting your Pint journey, it is not a very interesting Pint program because it has a single constraint that can be satisfied trivially. In this chapter, we go over another Pint program that has a real world application.

You're in charge of designing a new 18-hole golf course with a total "par" of 72. If you're not familiar with the term, "par" for a given golf hole is the predetermined number of strokes that a _proficient_ golfer should require to complete the hole. Each golf hole is assigned a "par" of 3, 4, or 5 depending on its playing length. Your task is to come up with a "par" assignment for each of the 18 holes such that the total par is 72 (and then to design the actual holes after).

Create a new project under `~/projects` and call it `golf`:

```console
$ cd ~/projects
$ mkdir golf
$ cd golf
```

Next, make a new source file and call it `main.pnt`. In order to model the problem above using Pint, we have to think about 2 things:

1. What decision variables should we define and solve for that would help us solve our design problem?
1. What constraints exactly describe the requirements of the problem?

### Decision Variables

Recall that our goal is to assign a par value for each of the 18 golf holes. One way of representing these values is using 18 **integer** decision variables as follows:

```pint
var h0: int;
var h1: int;
.
.
var h17: int;
```

Notice the `int` (integer) primitive type that annotates each variable. Par values have to be integers!

A better way and more concise way of rewriting the above is using an array of 18 `int` values as follows:

```pint
var h: int[18];
```

The type `int[18]` describes an array of 18 integers.

### Constraints

#### Par Values Constraints

The first constraint that the design problem imposes is that each hole is assigned a par value of 3, 4, or 5. There are two ways to model this in Pint.

We can use the comparison operators to indicate that each par is greater or equal (`>=`) to 3 and (`&&`) less than or equal (`<=`) to 5:

```pint
constraint h[0] >= 3 && h[0] <= 5;
constraint h[1] >= 3 && h[1] <= 5;
.
.
constraint h[17] >= 3 && h[17] <= 5;
```

Notice the use of the array element access syntax using `[..]`. In Pint, `h[i]` is the `i+1`th element in array `h`.

Alternatively, we can constraint each element in `h` using `in` and a **range** expression as follows:

```pint
constraint h[0] in 3..5;
constraint h[1] in 3..5;
.
.
constraint h[17] in 3..5;
```

The expression `h[i] in 3..5` means: `h[i]` belongs to the set of integers between 3 and 5 inclusive. If `h` was an array of `real` values instead, then the range `3..5` would refer to the set of real numbers between 3 and 5 inclusive.

#### Total Par Constraint

The problem definition imposes another requirement: the total par of the golf course should be 72. The way to represent this constraint is using a summation as follows:

```pint
constraint h[0] + h[1] + h[2] + h[3] + h[4] + h[5] + h[6]
         + h[7] + h[8] + h[9] + h[10] + h[11] + h[12] + h[13]
         + h[14] + h[15] + h[16] + h[17] == 72
```

### Final Program

To finish our program, we need a `solve` directive and an (optional) `print` directive. Because we do not have an objective function to optimize for, we can simply use a `solve satisfy` directive.

> **Note:** some modeling problems require optimizing (i.e. maximizing or minimizing) some objective function. Modeling these problems in Pint require using the directives `solve maximize <expr>;` or `solve minimize <expr>;` where `<expr>` is any valid Pint expression. The goal of an optimization problem is to find the _best_ solution satisfying the constraints instead of _any_ solution.

Our final program now looks like this:

```pint
var h: int[18];

constraint h[0] in 3..5;
constraint h[1] in 3..5;
constraint h[2] in 3..5;
constraint h[3] in 3..5;
constraint h[4] in 3..5;
constraint h[5] in 3..5;
constraint h[6] in 3..5;
constraint h[7] in 3..5;
constraint h[8] in 3..5;
constraint h[9] in 3..5;
constraint h[10] in 3..5;
constraint h[11] in 3..5;
constraint h[12] in 3..5;
constraint h[13] in 3..5;
constraint h[14] in 3..5;
constraint h[15] in 3..5;
constraint h[16] in 3..5;
constraint h[17] in 3..5;

constraint h[0] + h[1] + h[2] + h[3] + h[4] + h[5] + h[6]
         + h[7] + h[8] + h[9] + h[10] + h[11] + h[12] + h[13]
         + h[14] + h[15] + h[16] + h[17] == 72

solve satisfy;

print h;
```

Open the `main.pnt` file you created earlier and paste the code above. Save the file, go back to your terminal window in the `~/projects/golf/` directory, and enter the following command to compile and solve the program:

```console
$ pintc --solve main.rs
```

The following should be printed to the terminal:

```console
[3, 3, 3, 3, 3, 3, 3, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5];
```

The values you see may differ from the above, but any values you see should satisfy have the two properties:

1. Each value is an integer between 3 and 5 inclusive.
1. The sum of all the values is exactly 72.

### Let's Make it More Interesting

You may notice that the solution above does not have any par 4 holes (again, the solution you see in your terminal might be different). This is probably undesirable for most players. Because our program did not impose any requirements on the statistical distribution of pars, the programmatic solver is free to choose a "boring" solution, as long as it satisfies the constraints.

To make the golf course more interesting, we may want to be more specific about the distribution of par values. Assume that we would like to have **exactly 6 par 4 holes** in our new golf course. There are multiple ways of modeling this, but here's one them:

```pint
constraint  (h[0] == 4) as int
          + (h[1] == 4) as int
          + (h[2] == 4) as int
          + (h[3] == 4) as int
          + (h[4] == 4) as int
          + (h[5] == 4) as int
          + (h[6] == 4) as int
          + (h[7] == 4) as int
          + (h[8] == 4) as int
          + (h[9] == 4) as int
          + (h[10] == 4) as int
          + (h[11] == 4) as int
          + (h[12] == 4) as int
          + (h[13] == 4) as int
          + (h[14] == 4) as int
          + (h[15] == 4) as int
          + (h[16] == 4) as int
          + (h[17] == 4) as int == 6;
```

The left-hand side of the equality above counts the number of elements in array `h` that are equal to 4. Since the expression `h[i] == 4` is a `bool`, we need to cast it to an `int`, using `as int`, in order to be able to use addition (`+`).

Adding the constraint above to our program from earlier and running `pintc --solve main.pnt` results in the following solution:

```console
[5, 5, 3, 4, 3, 4, 5, 4, 4, 4, 5, 5, 3, 3, 3, 3, 5, 4];
```

which is certainly a much more interesting distribution of par values.

> **Note:** Pint statements can be written in any order so feel free to paste the new constraint anywhere in your code, but keep in mind that readability is a critical property of good code.

> **Note:** there are other more concise ways of modeling the problem above in Pint. Later chapters will describe additional Pint tools that will allows us to write shorter and more maintainable code.
