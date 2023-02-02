Name: Alex Harris
Date: 1/30/2023
Assignment 3

(If you have not finished 'Lists.hs', stop here and make sure you finish that
file first!)

> module Folds
>        (concat,
>         startsWith,
>         endsWith,
>         tails,
>         tails',
>         countSub,
>         testFoldr) where

> import Prelude hiding (concat)
> import Test.HUnit

> --------------------------------------------------------------------------------
> -- Problem (map and foldr practice for lists)
> --------------------------------------------------------------------------------

Go back to the following functions that you defined in 'Lists.lhs' and redefine
them using one of the higher-order functions 'map', 'foldr' or 'para' (see
below). These are the only list library functions that you should use on this
problem.

If you need any additional helper functions you must define them yourself (and
any helper functions should also use map, foldr or para instead of explicit
recursion).

For test cases, you can copy test cases that you have written in 'Lists.lhs'.

> testFoldr :: Test
> testFoldr = TestList [ tconcat,  tstartsWith, tendsWith, ttails, tcountSub]

> -- | The concatenation of all of the elements of a list of lists
> --
> -- >>> concat [[1,2,3],[4,5,6],[7,8,9]]
> -- [1,2,3,4,5,6,7,8,9]

NOTE: remember you cannot use any list functions from the 'Prelude' or
'Data.List' for this problem, even for use as a helper function. Instead, define
it yourself.

> concat :: [[a]] -> [a]
> concat = undefined

> tconcat :: Test
> tconcat = "concat" ~: TestList
>     [
>       concat [[],[],[]] ~?= [],
>       concat [[1,2,3],[4,5,6],[7,8,9]] ~?= [1,2,3,4,5,6,7,8,9],
>       concat [[1,2],[3,4],[]] ~?= [1,2,3,4],
>       concat [[1,2],[],[3,4]] ~?= [1,2,3,4],
>       concat [[],[1,2],[3,4]]
>     ]

> -- | The 'startsWith' function takes two strings and returns 'True'
> -- iff the first is a prefix of the second.
> --
> -- >>> "Hello" `startsWith` "Hello World!"
> -- True
> --
> -- >>> "Hello" `startsWith` "Wello Horld!"
> -- False

NOTE: use foldr for this one, but it is tricky! (Hint: the value returned by
foldr can itself be a function.)

> startsWith :: String -> String -> Bool
> startsWith = undefined

> tstartsWith :: Test
> tstartsWith = "tstartsWith" ~: (assertFailure "testcase for startsWith" :: Assertion)
> -- INTERLUDE: para

Now consider a variant of foldr called para. In the case of cons, foldr provides
access to the head of the list and the result of the fold over the tail of the
list. The para function should do the same, but should also provide access to
the tail of the list (before it has been processed).

> -- | foldr variant that provides access to each tail of the list
> para :: (a -> [a] -> b -> b) -> b -> [a] -> b
> para _ b [] = b
> para f b (x:xs) = f x xs (para f b xs)

For example, consider the tails function.

> -- | The 'tails' function calculates all suffixes of a give list and returns them
> -- in decreasing order of length. For example:
> --
> -- >>> tails "abc"
> -- ["abc", "bc", "c", ""],
> --
> tails :: [a] -> [[a]]
> tails []     = [[]]
> tails (x:xs) = (x:xs) : tails xs

It is a natural fit to implement tails using para. See if you can redefine the
function above so that the test cases still pass.

> tails' :: [a] -> [[a]]
> tails' = undefined

> ttails :: Test
> ttails = "tails" ~: TestList [
>     "tails0" ~: tails' "abc" ~?= ["abc", "bc", "c", ""],
>     "tails1" ~: tails' ""    ~?= [""],
>     "tails2" ~: tails' "a"   ~?= ["a",""] ]

> -- | The 'endsWith' function takes two lists and returns 'True' iff
> -- the first list is a suffix of the second. The second list must be
> -- finite.
> --
> -- >>> "ld!" `endsWith` "Hello World!"
> -- True
> --
> -- >>> "World" `endsWith` "Hello World!"
> -- False

NOTE: use para for this one!

> endsWith :: String -> String -> Bool
> endsWith = undefined

> tendsWith :: Test
> tendsWith = "endsWith" ~: (assertFailure "testcase for endsWith" :: Assertion)
> -- | The 'countSub' function returns the number of (potentially overlapping)
> -- occurrences of a substring sub found in a string.
> --
> -- >>> countSub "aa" "aaa"
> -- 2
> -- >>> countSub "" "aaac"
> -- 5

(You may use the para and startsWith functions in countSub.)

> countSub  :: String -> String -> Int
> countSub = undefined

> tcountSub :: Test
> tcountSub = "countSub" ~: (assertFailure "testcase for countSub" :: Assertion)
