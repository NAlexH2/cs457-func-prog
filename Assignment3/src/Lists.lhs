Name: Alex Harris
Date: 1/30/2023
Assignment 3

> module Lists
>       (reverse,
>        zip,
>        minimumMaybe,
>        startsWith,
>        endsWith,
>        transpose,
>        countSub,
>        testStyle,
>        testLists) where
> import Prelude hiding (reverse, zip)
> import Test.HUnit
  
> --------------------------------------------------------------------------------
> -- Problem (Good Style)
> --------------------------------------------------------------------------------

> testStyle :: Test
> testStyle = "testStyle" ~:
>    TestList [ treverse, tzip ]

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

NOTE: If you have set up VSCode and hlint correctly, your IDE should give you a
few hints on how to improve these functions. But, it won't tell you everything.

> -- OLD:
> -- reverse :: [a] -> [a]
> -- reverse l  = reverseAux l [] where
> --   reverseAux l acc =
> --     if null l then acc
> --        else reverseAux (tail l) (head l : acc)

> -- NEW:
> reverse :: [a] -> [a]
> reverse l  = reverseAux l []
>   where
>       reverseAux l acc | null l    = acc
>                        | otherwise = reverseAux (tail l) (head l : acc)

> treverse :: Test
> treverse = "reverse" ~: TestList
>     [reverse [3,2,1] ~?= ([1,2,3] :: [Int]),
>      reverse [1]     ~?= ([1]     :: [Int]) ]

> -- OLD:
> -- zip :: [a] -> [b] -> [(a, b)]
> -- zip xs ys = g 0 xs ys where
> --   g n xs ys = if n == length xs || n == length ys then [] else
> --           (xs !! n, ys !! n) : g (n + 1) xs ys

> -- NEW:
> zip :: [a] -> [b] -> [(a, b)]
> zip xs ys = g 0 xs ys 
>   where
>       g n xs ys | n == length xs || n == length ys = []
>                 | otherwise = (xs !! n, ys !! n) : g (n + 1) xs ys


> tzip :: Test
> tzip = "zip" ~:
>   TestList [ zip "abc" [True,False,True] ~?= [('a',True),('b',False), ('c', True)],
>              zip "abc" [True] ~?= [('a', True)],
>              zip [] [] ~?= ([] :: [(Int,Int)]) ]

> --------------------------------------------------------------------------------
> -- Problem (List library chops)
> --------------------------------------------------------------------------------

Define, debug and test the following functions. Some of these functions are part
of the Haskell standard prelude or standard libraries like Data.List. Their
solutions are readily available online. You should not google for this code:
instead, implement them yourself.

For each part of this problem, you should replace the testcase for that part
based on the description in the comments. Make sure to test with multiple inputs
using TestList. We will be grading your test cases as well as the correctness
and style of your solutions! HINT: your testing code should include any tests
that we give you in the the comments!

Do not use any list library functions in this problem. This includes any
function from the 'Prelude' or from 'Data.List' thats take arguments or returns
a result with a list type. Note that (:) and [] are data constructors for the
list type, not functions, so you are free to use them. Please also avoid list
comprehension syntax, as it actually de-sugars into library functions!

> testLists :: Test
> testLists = "testLists" ~: TestList
>   [tminimumMaybe, tstartsWith, tendsWith, ttranspose, tcountSub]

> -- Part One
> -- | The 'minimumMaybe` function computes the mininum value of a
> -- nonempty list. If the list is empty, it returns Nothing.
> --
> -- Recall the definition of 'Maybe' can be found in 'ADT.lhs' on Canvas.
> --
> -- >>> minumumMaybe []
> -- Nothing
> -- >>> minumumMaybe [2,1,3]
> -- Just 1
> minimumMaybe :: [Int] -> Maybe Int
> minimumMaybe [] = Nothing
> minimumMaybe (x:xs) = case minimumMaybe xs of
>                       Nothing -> Just x
>                       Just y -> if x < y then Just x
>                                 else Just y

> tminimumMaybe :: Test
> tminimumMaybe = "minimumMaybe" ~: TestList 
>      [ 
>        minimumMaybe [] ~?= Nothing,
>        minimumMaybe [2,1,3] ~?= Just 1,
>        minimumMaybe [9,9,9,9] ~?= Just 9,
>        minimumMaybe [999, 1, 9, 100, 4, 4, 1] ~?= Just 1
>      ]


> -- Part Two
> -- | The 'startsWith' function takes two strings and returns 'True'
> -- iff the first is a prefix of the second.
> --
> -- >>> "Hello" `startsWith` "Hello World!"
> -- True
> --
> -- >>> "Hello" `startsWith` "Wello Horld!"
> -- False
> startsWith [] [] = True
> startsWith  _ [] = False
> startsWith []  _ = False
> startsWith (x : xs) (y : ys)  | x == y && xs == ""  = True
>                               | x == y              = startsWith xs ys
>                               | otherwise           = False

> tstartsWith :: Test
> tstartsWith = "startsWith" ~: TestList
>       [
>         startsWith "" ""                  ~?= True,
>         startsWith "" "Hello World!"      ~?= False,
>         startsWith "Hello" ""             ~?= False,
>         startsWith "Hello" "Hello World!" ~?= True,
>         startsWith "Hello" "Wello World!" ~?= False
>       ]


> -- Part Three
> -- | The 'endsWith' function takes two lists and returns 'True' iff
> -- the first list is a suffix of the second. The second list must be
> -- finite.
> --
> -- >>> "ld!" `endsWith` "Hello World!"
> -- True
> --
> -- >>> "World" `endsWith` "Hello World!"
> -- False
> endsWith :: String -> String -> Bool
> endsWith [] []                        = True
> endsWith  _ []                        = True
> endsWith []  _                        = False
> endsWith xs (y:ys)  | strEq xs (y:ys) = True
>                     | otherwise       = endsWith xs ys
>     where
>       strEq :: String -> String -> Bool
>       strEq [] []                     = True
>       strEq (x:xs) (y:ys) | x == y    = strEq xs ys
>                           | otherwise = False
>       strEq _ _                       = False

> tendsWith :: Test
> tendsWith = "endsWith" ~: TestList
>      [
>         endsWith "" ""                    ~?= True,
>         endsWith "" "Hello World!"        ~?= True,
>         endsWith "World!" ""              ~?= False,
>         endsWith "ld!" "Hello World!"     ~?= True,
>         endsWith "World" "Hello World!"   ~?= True,
>         endsWith "World!" "Hello World!"  ~?= True
>      ]


> -- Part Four
> -- | The 'transpose' function transposes the rows and columns of its argument.
> -- If the inner lists are not all the same length, then the extra elements
> -- are ignored. Note, this is *not* the same behavior as the library version
> -- of transpose (i.e. the version of transpose from Data.List).
> --
> -- >>> transpose [[1,2,3],[4,5,6]]
> -- [[1,4],[2,5],[3,6]]
> -- >>> transpose []
> -- []
> -- >>> transpose [[]]
> -- []
> -- >>> transpose [[3,4,5]]
> -- [[3],[4],[5]]
> -- >>> transpose [[1,2],[3,4,5]]
> -- [[1,3],[2,4]]
> -- (WARNING: this one is tricky!)
> transpose :: [[a]] -> [[a]]
> transpose = undefined

> ttranspose :: Test
> ttranspose = "transpose" ~: TestList
>      [
>         transpose []                 ~?= [],
>         transpose [[]]               ~?= [],
>         transpose [[3,4,5]]          ~?= [[3],[4],[5]],
>         transpose [[1,2,3],[4,5,6]]  ~?= [[1,4],[2,5],[3,6]],
>         transpose [[1,2],[3,4,5]]    ~?= [[1,3],[2,4]]
>      ]

> -- Part Five
> -- | The 'countSub' function returns the number of (potentially overlapping)
> -- occurrences of a substring sub found in a string.
> --
> -- >>> countSub "aa" "aaa"
> -- 2
> -- >>> countSub "" "aaac"
> -- 5

(You may use startsWith function in countSub.)

> countSub :: String -> String -> Int
> countSub = undefined

> tcountSub :: Test
> tcountSub = "countSub" ~: (assertFailure "testcase for countSub" :: Assertion)
