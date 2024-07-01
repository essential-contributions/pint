# Managine Growing Projects

As you write large programs, organizing your code will become increasingly important. By grouping
related functionality and separating code with distinct features, you’ll clarify where to find code
that implements a particular feature and where to go to change how a feature works.

As a project grows, you should organize code by splitting it into multiple **modules** and then
multiple files. You can also extract parts of a project into separate packages that become external
dependencies. This chapter covers all these techniques.

Pint has a number of features that allow you to manage your code’s organization, including which
details are exposed, which details are private, and what names are in each scope in your programs.
These features, sometimes collectively referred to as the module system, include:

- **Packages**: A feature of the pint tool that lets you build, test, and share projects.
- **Modules** and **use**: Let you control the organization and scope of paths.
- **Paths**: A way of naming an item, such as a type, a macro, or a `const`.

In this chapter, we’ll cover all these features, discuss how they interact, and explain how to use
them to manage scope. By the end, you should have a solid understanding of the module system.
