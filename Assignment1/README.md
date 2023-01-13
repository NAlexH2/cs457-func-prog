# Assignment 1

This directory contains your first assignment. It is structured as a stack
project. The directory contains a number of files that configure how to build
this assignment. For now, you do not need to care about these files. The most
important file that you want to look at is called `Main.lhs` and is located
under the `app` directory.

The `Main.lhs` file contains all the instructions necessary for finishing your
first assignment. The file is written as a literate Haskell program, so all the
code must start with a ">" in the beginning of a line. You only need to submit
the `Main.lhs` file you completed for this assignment.

## How to compile your program

To compile your program, you just type the following command anywhere under the
project's directory:

``` sh
stack build
```

If you have finished your homework, you might see a "parse error". This is
normal as one of the problems in the assignment does contain some syntax error
for you to fix.

Your code MUST successfully compile to get any points in this assignment.

## How to run your program

The assignment has already included a number of tests. To run these tests,
simply type the following command:

``` sh
stack run Main
```

Alternatively:

``` sh
stack run --
```

## Assignment Due

11:59 PM AOE (UTC -12), Thursday, Jan. 19
