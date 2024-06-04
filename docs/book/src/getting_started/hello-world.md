## Hello, World!

Now that you've installed Pint, it's time to write your first Pint program. It's traditional when learning a new language to write a little program that prints the text `Hello, world!`

### Creating a Project Directory

You'll start by making a directory to store your Pint code. It doesn't matter to Pint where your code lives, but for the exercise and projects in this book, we suggest making a `projects` directory in your home directory and keeping all your projects there.

Open a terminal and enter the following commands to make a `projects` directory and a directory for the "Hello, world!" project within the `projects` directory:

```console
$ mkdir ~/projects
$ cd ~/projects
$ mkdir hello_world
$ cd hello_world
```

### Writing a Pint Program

Next, make a new source file and call it `main.pnt`. Pint files always end with the `pnt` extension. Now open the `main.pnt` file you just created and enter the code below:

```pint
var greeting: string;

constraint greeting == "Hello, world!";

solve satisfy;

print greeting;
```

Save the file and go back to your terminal window in the `~/projects/hello_world/` directory. Enter the following command to compile and **solve** the file:

```console
$ pintc --solve main.rs
```

The following should be printed to the terminal:

```console
"Hello, world!"
```

Congratulations! You've officially written your first Pint program.

> **Note:** Pint programs are not "run" like traditional programs. They are "solved".

### Anatomy of a Pint program

Let's review this "Hello, world!" program in detail.

The first line in this program is:

```pint
var greeting: string;
```

which declares a new **decision variable** called `greeting` and specifies its type to be a `string`, which is a **primitive type** in `Pint.`

The second line is:

```pint
constraint greeting == "Hello, world!";
```

which defines a new **constraint**. Constraints are the building blocks of any Pint program; they contain all the "useful" logic! The above constraint imposes the restriction that any **assignment** of the decision variable `greeting` must satisfy the Boolean condition `greeting == "Hello, world!"`.

The third line is:

```pint
solve satisfy;
```

This is a directive that describes what kind of program this is. In this case, this is a **satisfaction program**: we wish to find a value for `greeting` that satisfies the constraint but we do not care which one.

The fourth line is

```pint
print greeting;
```

which simply prints a valid assignment of `greeting`. The program above is trivial in the sense that there is a single assignment of `greeting` that satisfies the constraint. That single assignment is, of course, `greeting = "Hello, world!"`, which is why we see `Hello, World!` in the terminal after running `pintc --solve main.pnt`.

> **Note:** Pint programs that do not have any assignments satisfying _all_ their constraints are called **unsatisfiable**.
