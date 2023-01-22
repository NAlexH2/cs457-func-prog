-- | Name: Put your name here
-- | Date: Put the date here
-- | Assignment 2

module Lists (
  runTests,
  grid,
  square,
  replicate,
  pyths,
  perfects,
  scalarProduct) where

import Prelude hiding (replicate)
import Test.HUnit

-- | You have experienced literate Haskell in the last assignment. This time,
-- you will be playing with a "more typical" Haskell source file.

-- | In this assignment, you are going to get a little bit taste of test-driven
-- development. In each problem, you are asked to first think about all the
-- possible cases and add some test cases to our existing tests. After that, you
-- implement the function and check if your implementation passes all the tests
-- you have provided. In the end, we will showcase a Haskell innovation in
-- testing, namely QuickCheck, for automatically generating tests, for you to
-- check if your test cases and implementation are complete.

-- | Again, we start with a function that runs all the test cases.
runTests :: IO ()
runTests =  do
  _ <- runTestTT $ TestList [ testGrid,
                              testSquare,
                              testReplicate,
                              testPyths,
                              testPerfects,
                              testScalarProduct]
  return ()

-- | Suppose that a coordinate grid of size m * n is given by the list of all
-- pairs (x, y) of integers such that 0 <= x <= m and = <= y <= n. Using a list
-- comprehension, define a function 'grid :: Int -> Int -> [(Int, Int)]' that
-- returns a coordinate grid of a given size.

-- TODO: We have provided an example test case for 'grid', please add at least
-- two more test cases here. Your object here is not to add complex test cases
-- or too many test cases. Instead, think about representative cases that our
-- example does not cover here. (You can refer to our code in Assignment1 if you
-- do not know how to add test cases.)
testGrid :: Test
testGrid = "testGrid" ~: TestList
  [ grid 1 2 ~?= [(0,0), (0,1), (0,2), (1,0), (1,1), (1,2)] ]

-- TODO: Implement the 'grid' function.
grid :: Int -> Int -> [(Int, Int)]
grid = error "Not implemented"

-- | Using a list comprehension and the function 'grid' above, define a function
-- 'square :: Int -> [(Int, Int)]' that returns a coordinate square of size n,
-- excluding the diagonal from (0, 0) to (n, n).

-- TODO: We have provided an example test case for 'square'. Again, please add
-- at least two more test cases here.
testSquare :: Test
testSquare = "testSquare" ~: TestList
  [ square 2 ~?= [(0,1), (0,2), (1,0), (1,2), (2,0), (2,1)] ]

-- TODO: Implement the 'square' function.
square :: Int -> [(Int, Int)]
square = error "Not implemented"

-- | In a similar way to the function 'length', show how the library function
-- 'replicate :: Int -> a -> [a]' that produces a list of identical elements can
-- be defined using a list comprehension.

-- TODO: We have provided an example test case for 'replicate'. Again, please
-- add at least two more test cases here.
testReplicate :: Test
testReplicate = "testReplicate" ~: TestList
  [ replicate 3 True ~?= [True, True, True] ]

-- TODO: Implement the 'replicate' function.
replicate :: Int -> a -> [a]
replicate = error "Not implemented"

-- | A triple (x, y, z) of positive integers is Pythagorean if it satisfies the
-- equation x^2 + y^2 = z^2. Using a list comprehension with three generators,
-- define a function 'pyths :: Integer -> [(Integer, Integer, Integer)]' that
-- returns the list of all such triples whose components are at most a given
-- limit. Note: You might run into a warning labeled as '[-Wtype-defaults]' with
-- your implementation, you can ignore this warning for this problem.

-- TODO: We have provided an example test case for 'pyths'. Again, please add at
-- least two more test cases here.
testPyths :: Test
testPyths = "testPyths" ~: TestList
  [ pyths 10 ~?= [(3,4,5), (4,3,5), (6,8,10), (8,6,10)] ]

-- TODO: Implement the 'pyths' function.
pyths :: Integer -> [(Integer, Integer, Integer)]
pyths = error "Not implemented"

-- | A positive integer is prefect if it equals to the sum of all of its
-- factors, excluding the number itself. Using a list comprehension and the
-- function 'factors', define a function 'perfects : Int -> [Int]' that returns
-- the list of all perfect numbers up to a given limit (including the limit, if
-- it happens to be a perfect number).

-- TODO: We have provided an example test case for 'perfects'. Again, please add
-- at least two more test cases here.
testPerfects :: Test
testPerfects = "testPerfects" ~: TestList
  [ perfects 500 ~?= [6, 28, 496] ]

-- TODO: Implement the 'perfects' function. Note: I have planted a bug in the
-- implementation of 'factors', can you spot it?
perfects :: Int -> [Int]
perfects = error "Not implemented"
  where
    factors :: Int -> [Int]
    factors n = [x | x <- [1..n], x `div` n == 0]

-- | The scalar product of two list of integers xs and ys of length n is given
-- by the sum of the products of corresponding integers. For example, a scalar
-- product of [1,2,3] and [4,5,6] is 1*4 + 2*5 + 3*6 = 32. In a similar manner
-- to 'chisqr', show how a list comprehension can be used to define a function
-- 'scalarProduct :: [Int] -> [Int] -> Int' that returns the scalar product of
-- two lists.

-- TODO: We have provided an example test case for 'scalarProduct'. Again,
-- please add at least two more test cases here. Note: In the case that two
-- lists do not have equal length, you can decide what the functions
-- returns---as long as it does not cause a runtime error.
testScalarProduct :: Test
testScalarProduct = "testScalarProduct" ~: TestList
  [ scalarProduct [1,2,3] [4,5,6] ~?= 32 ]

-- TODO: Implement the 'scalaProduct' function.
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct = error "Not implemented"

-- | If you are happy with your test cases and your implementation, you can run
-- 'stack run --' or 'stack run Main' to run all your tests. If you are happy
-- with the test result, try 'stack test' and see if that makes any differences.
-- You can move to 'test/Spec.hs' file to see what those tests are.
