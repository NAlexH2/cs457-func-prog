# Assignment 2

In this assignment, you are going to get a little bit taste of test-driven
development. In each problem, you are asked to first think about all the
possible cases and add some test cases to our existing tests. After that, you
implement the function and check if your implementation passes all the tests you
have provided. In the end, we will showcase a Haskell innovation in testing,
namely QuickCheck, for automatically generating tests, for you to check if your
test cases and implementation are complete.

In this assignment, you will be working on the `src/Lists.hs` file. More
detailed instructions can be found in that file.

You only need to submit the `Lists.hs` file for the homework.

The following commands are for reference:

## Compilation

Compiling your code: 
``` sh
stack build
```

## Execution

To run the test cases that you have written:
``` sh
stack run --
```
or

``` sh
stack run Main
```

## Testing

To run the automatically generated test cases: 

``` sh
stack test
```
