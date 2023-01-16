Name: Alex Harris
Date: 1/14/2023
Assignment 1

The following line contains a command that enables some special features of
Haskell to make this file work. For now, you can just safely ignore this line.

> {-# LANGUAGE RankNTypes #-}

This page is a "literate" Haskell program, meaning that explanation is
interspersed with actual Haskell code. To complete your assignment, edit
'Main.lhs' and submit this file through Canvas. You only need to submit the
'Main.lhs' file.

This file starts by first declaring that we are creating a module called 'Main'
and are using functions defined in the modules 'Prelude' and 'Test.HUnit'.

The 'Prelude' line imports all except for the functions listed (which you will
write). The module 'Prelude' is special in that it is always imported by
default, so the the point of this line is not to import more functions, but
rather to exclude a few functions. (Haskell does not allow functions to be
redefined in the same module.)

The 'Test.HUnit' line imports all functions defined in that module.

> module Main (main, bools, nums, add, copy, apply) where
> import Prelude hiding (last, init)
> import Test.HUnit

The main "entry point" for this assignment runs the tests for each homework
problem below. You should not edit this definition. Instead, your goal is to
modify the problems below so that all of the tests pass. Note that the
definitions in Haskell modules do not need to come in any particular order;
here, the main function uses the definitions 'testSyntax', 'testListPt1', etc,
even though their definitions come much later in the file.

> main :: IO ()
> main = do
>   _ <- runTestTT $ TestList [ testSyntax,
>                               testList,
>                               testListPt1,
>                               testListPt2,
>                               testStyle]
>   return ()

To compile your program, run the command 'stack build' in the 'Assignment1'
directory. To run your program, type the command 'stack run Main'. Your code
must successfully compile to get any points, so make sure you check if your code
works with stack before submitting your homework.

Style is also important for getting a good grade in your assignments. Please
refer to the Haskell Style Guide distributed on Canvas (under the "Modules"
tab).

> -----------------------------------------------------------------------------
> -- Problem (Syntax)
> -----------------------------------------------------------------------------

The script below contains three syntactic errors. Correct these errors and then
check that your script works properly using 'stack build'.

> -- | TODO: fix the code
> n :: Int
> n = a `div` length xs
>  where
>     a = 10
>     xs = [1, 2]

Next, we have some code for testing if your implementation is correct. Your
solution should successfully pass all tests.

> tn ::Test
> tn = "n" ~: TestList [ n ~?= 5]

> testSyntax :: Test
> testSyntax = "testSyntax" ~:
>   TestList [ tn ]

> --------------------------------------------------------------------------------
> -- Problem (Types)
> -------------------------------------------------------------------------------- >

Write down definitions that have the following types; it does not matter what
Wthe definitions actually do as long as they are type correct.

> -- | TODO: write down definitions that have the following types
> bools :: [Bool]
> bools = [True, False]

> nums :: [[Int]]
> nums = [[1,2], [3,4]]

> add :: Int -> Int -> Int -> Int
> add = _ + _ + _ + _

> copy :: a -> (a, a)
> copy (_, _) = a

> apply :: (a -> b) -> a -> b
> apply | b = a

> --------------------------------------------------------------------------------
> -- Problem (Functions, Part I)
> -------------------------------------------------------------------------------- >

> testListPt1 :: Test
> testListPt1 = "testListPt1" ~:
>       TestList [ testLast last "testLast",
>                  testLast lastAlt "testLastAlt",
>                  testInit init "testInit",
>                  testInit initAlt "testInitAlt" ]

Remind yourself of the following functions we have demonstrated in the first
lecture:

- 'head' selects the first element of a non-empty list.
- 'tail' removes the first element from a non-empty list.
- '!!' selects the nth element of list (counting from 0).
- 'take' selects the first n elements of a list.
- 'drop' removes the first n elements from a list.
- 'length' calculates the length of a list.
- 'sum' calculates the sum of a list of numbers.
- 'product' calculates the product of a list of numbers.
- '++' appends two lists.
- 'reverse' reverses a list.

Some examples can be found in the test cases below. We have not seen the '~?='
operator before: it is basically an '==' operator with some extra support for
printing error information. You should interpret 'head xs ~?= 1' as 'head xs' 
is expected to result in 1.

> -- | Examples of common list functions.
> testList :: Test
> testList = "testList" ~:
>       TestList [ head xs ~?= 1,
>                  tail xs ~?= [2, 3, 4, 5],
>                  xs !! 2 ~?= 3,
>                  take 3 xs ~?= [1, 2, 3],
>                  drop 3 xs ~?= [4, 5],
>                  length xs ~?= 5,
>                  sum xs ~?= 15,
>                  product xs ~?= 120,
>                  [1, 2, 3] ++ [4, 5] ~?= xs,
>                  reverse xs ~?= [5, 4, 3, 2, 1]]
>       where
>               xs :: [Integer]
>               xs = [1, 2, 3, 4, 5]

The library function 'last' selects the last element of a non-empty list; for
example, 'last [1, 2, 3, 4 ,5] = 5'. Show how the function 'last' could be
defined in terms of the list functions shown above.

> -- | TODO: implement 'last'.
> -- | Given a non-empty list, returns the last element in the list.
> last :: [a] -> a
> last [] = error "Empty List"
> last [a] = [a]
> last a = a !! (length a-1)

Can you think of another possible definition?

> -- | TODO: implement 'last' in a different way.
> -- | Given a non-empty list, returns the last element in the list.
> lastAlt :: [a] -> a
> lastAlt [] = error "Empty list"
> lastAlt [a] = [a]
> lastAlt a = reverse a !! 0

The library function 'init' removes the last element from a non-empty list; for
example, 'init [1, 2, 3, 4, 5] = [1, 2, 3, 4]'. Show how 'init' could similarly
be defined in two different ways.

> -- | TODO: implement 'init'.
> -- | Given a non-empty list, returns the the list without its last elements.
> init :: [a] -> [a]
> init [] = error "Empty list"
> init [a] = [a] 
> init (x:xs) = take (length (x:xs) - 1) (x:xs)

> -- | TODO: implement 'init' in a different way.
> -- | Given a non-empty list, returns the the list without its last elements.
> initAlt :: [a] -> [a]
> initAlt [] = error "Empty list"
> initAlt [a] = [a] 
> initAlt (x:xs) = reverse (drop 1 (reverse (x:xs)))

Your implementation should successfully pass all test cases when running 'stack
run Main'.

> testLast :: (forall a. [a] -> a) -> String -> Test
> testLast f msg = msg ~:
>       TestList [ f [1] ~?= (1 :: Int),
>                  f ['5', '4', '3'] ~?= '3',
>                  f [True, False] ~?= False,
>                  f [1..10] ~?= (10 :: Int)]
>
> testInit :: (forall a. [a] -> [a]) -> String -> Test
> testInit f msg = msg ~:
>       TestList [ f ([1] :: [Int]) ~?= [],
>                  f ['5', '4', '3'] ~?= ['5', '4'],
>                  f [True, False] ~?= [True],
>                  f ([1..10] :: [Int]) ~?= [1..9]]

> --------------------------------------------------------------------------------
> -- Problem (Functions, Part II)
> --------------------------------------------------------------------------------

> testListPt2 :: Test
> testListPt2 = "testListPt2" ~:
>       TestList [ testThird thirdV1 "testThirdV1",
>                  testThird thirdV2 "testThirdV2",
>                  testThird thirdV3 "testThirdV3",
>                  testSafetail safetailV1 "testSafetailV1",
>                  testSafetail safetailV2 "testSafetailV2",
>                  testSafetail safetailV3 "testSafetailV3" ]

Define a function 'third :: [a] -> a' that returns the third element in a list
that contains at least this mamy elements using (for this task, you do not need
to consider the case that the input list has fewer than 3 elements):

(i) 'head' and 'tail':

> -- | TODO: implement [third] using 'head' and 'tail'.
> thirdV1 :: [a] -> a

(ii) list indexing '!!':

> -- | TODO: implement [third] using '!!'.
> thirdV2 :: [a] -> a

(iii) pattern matching:

Note: Your implementation for [thirdV3] might trigger a warning about
inexhaustive pattern matching when compiling your code. You do not need to worry
about this warning for this function.

> -- | TODO: implement [third] using pattern matching.
> thirdV3 :: [a] -> a

Consider a function 'safetail :: [a] -> [a]' that behaves in the same way as
'tail' except that it maps the empty list to itself rather than producing an
error. Using 'tail' and the function 'null :: [a] -> Bool' that decides if a
list is empty or not, define 'safetail' using:

(i) a conditional expression:

> -- | TODO: implement 'safetail' using a conditional expression
> safetailV1 :: [a] -> [a]

(ii) guarded equations:

> -- | TODO: implement 'safetail' using guarded equations
> safetailV2 :: [a] -> [a]

(iii) pattern matching:

> -- | TODO: implement 'safetail' using pattern matching
> safetailV3 :: [a] -> [a]

> testThird :: (forall a. [a] -> a) -> String -> Test
> testThird third msg = msg ~:
>       TestList [ third ['a', 'b', 'c'] ~?= 'c',
>                  third ([1..100] :: [Int]) ~?= 3,
>                  third ([42, 35, 97, 86, 21] :: [Integer]) ~?= 97 ]

> testSafetail :: (forall a. [a] -> [a]) -> String -> Test
> testSafetail safetail msg = msg ~:
>       TestList [ safetail ([] :: [Bool]) ~?= [],
>                  safetail ([1..100] :: [Int]) ~?= tail [1..100],
>                  safetail ['a'] ~?= tail ['a'],
>                  safetail ([7.0, 2.0] :: [Double]) ~?= tail [7.0, 2.0] ]

> --------------------------------------------------------------------------------
> -- Problem (Good Style)
> --------------------------------------------------------------------------------
> testStyle :: Test
> testStyle = "testStyle" ~:
>    TestList [ tabc , tarithmetic ]

All of the following Haskell code does what it is supposed to do (i.e. the tests
pass), but it is difficult to read. Rewrite the following expressions so that
they exactly follow the style guide. Be careful: the style guide includes quite
a few rules, and we've broken most of them in what follows! (You don't need to
rewrite the test following each part, but you do need to make sure that you
don't break the code as you refactor it!)

NOTE: Do not change the name of any of the top level declarations below, even if
you think that they aren't very good (they aren't). We will be using automatic
testing to ensure that you do not break anything when you rewrite these
functions. On the other hand, local variables (such as function parameters and
those bound by let and where) can and should be renamed.

NOTE: If you have set up your editor/IDE and hlint correctly, your IDE should
give you a few hints on how to improve these functions. But, it won't tell you
everything.

> -- Part One
> -- | TODO: refactor the following code
> abc :: Bool -> Bool -> Bool -> Bool
> abc x y z =
>   if x then if y then True else
>        if (x && z) then True else False
>   else False

> tabc :: Test
> tabc = "abc" ~: TestList [abc True False True  ~?= True,
>                           abc True False False ~?= False,
>                           abc False True True  ~?= False]

> -- Part Two
> -- | TODO: refactor the following code
> arithmetic :: ((Int, Int), Int) -> ((Int,Int), Int) -> (Int, Int, Int)
> arithmetic x1 x2 =
>      let a = fst (fst x1) in
>      let b = snd (fst x1) in
>      let c = snd x1 in
>      let d = fst (fst x2) in
>      let e = snd (fst x2) in
>      let f = snd x2
>        in
>        ((((((b*f) - (c*e)), ((c*
>        d) - (a*f)
>        ), ((a*e)-(b*d))))))
>
> tarithmetic :: Test
> tarithmetic = "arithmetic" ~:
>    TestList[ arithmetic ((1,2),3) ((4,5),6) ~?= (-3,6,-3),
>              arithmetic ((3,2),1) ((4,5),6) ~?= (7,-14,7) ]
