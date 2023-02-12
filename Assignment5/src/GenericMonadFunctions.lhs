Name: Alex Harris
Date: 2/11/2023
Assignment 5

> module GenericMonadFunctions where

> import Prelude hiding (mapM, sequence)
> import Test.HUnit
> import qualified Data.Char as Char

Generic Monad Operations
========================

This problem asks you to recreate some of the operations in the
[Control.Monad](https://hackage.haskell.org/package/base-4.14.2.0/docs/Control-Monad.html)
library. You should *not* use any of the functions defined in that library to
solve this problem. (These functions also appear in more general forms
elsewhere, so other libraries that are off limits for this problem include
`Control.Applicative`, `Data.Traversable` and `Data.Foldable`.)

NOTE: because these operations are so generic, the types will really help you
figure out the implementation, even if you don't quite know what the function
should do.

For that reason you should also test *each* of these functions with at least two
unit test cases, one using the `Maybe` monad, and one using the `List` monad.
After you you have tried the function out, try to describe in words what each
operation does for that specific monad.

Here is the first one as an example.

> -- (a)
>

Given the type signature:

> mapM :: Monad m => (a -> m b) -> [a] -> m [b]

We implement it by recursion on the list argument.

> mapM _ []     = return []
> mapM f (x:xs) = do
>    b  <- f x
>    bs <- mapM f xs
>    return (b:bs)

Then define the following test cases, which make use of the following
helper functions.

> maybeUpper :: Char -> Maybe Char
> maybeUpper x = if Char.isAlpha x then Just (Char.toUpper x) else Nothing

> onlyUpper :: [Char] -> [Char]
> onlyUpper = filter Char.isUpper

> testMapM :: Test
> testMapM = "testMapM" ~:
>       TestList [ mapM maybeUpper "sjkdhf" ~?= Just "SJKDHF",
>                  mapM maybeUpper "sa2ljsd" ~?= Nothing,
>                  mapM onlyUpper ["QuickCheck", "Haskell"] ~?= ["QH", "CH"],
>                  mapM onlyUpper ["QuickCheck", ""] ~?= []]

Finally, we observe that this function is a generalization of List.map, where
the mapped function can return its value in some monad m.

> -- (b)
>

> foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
> foldM = error "foldM: unimplemented"

> testFoldM :: Test
> testFoldM = TestList [ addEven [1,2,3]  ~=? Nothing,
>                        addEven [2,4]    ~=? Just 6,
>                        foldM safeDiv 16 [2,2] ~=? Just 4,
>                        foldM (flip replicate) 'a' [3,2] ~=? "aaaaaa"
>                      ]
> addEven :: [Int] -> Maybe Int
> addEven = foldM f 0 where
>                f x y | even x    = Just (x + y)
>                      | otherwise = Nothing
> safeDiv :: Int -> Int -> Maybe Int
> safeDiv x y = if y == 0 then Nothing else Just (x `div` y)
>

> -- (c)
>

> sequence :: Monad m => [m a] -> m [a]
> sequence = error "sequence: unimplemented"

> testSequence :: Test
> testSequence = TestList [
>      sequence [Just (3::Int), Nothing, Just 4] ~=? Nothing
>    , sequence [[1::Int,2],[3],[4,5]] ~=? [[1,3,4],[1,3,5],[2,3,4],[2,3,5]]
>    , sequence (map maybeUpper "abcd") ~=? Just "ABCD"
>    , sequence (map maybeUpper "abcd2") ~=? Nothing
>    ]

> -- (d) This one is the Kleisli "fish operator"
> --

> (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
> (>=>) = error ">=>: unimplemented"

> testKleisli :: Test
> testKleisli = TestList [ (maybeUpper >=> earlyOrd) 'a' ~=? Just 65
>                        , (maybeUpper >=> earlyOrd) '2' ~=? Nothing
>                        , (mDup >=> mDup) 1 ~=? [1,1,1,1]
>                        , (replicate 2 >=> replicate 3) 'l' ~=? "llllll"
>                        ]
>
> mDup :: Int -> [Int]
> mDup x = [x,x]
>
> earlyOrd :: Char -> Maybe Int
> earlyOrd c = if c < 'm' then Just (Char.ord c) else Nothing

> -- (e)
>

> join :: (Monad m) => m (m a) -> m a
> join = error "join: unimplemented"

> testJoin :: Test
> testJoin = TestList [ join [[1::Int,2],[3,4]] ~=? [1,2,3,4],
>                       join [[1::Int,2],[3,4],[]] ~=? [1,2,3,4],
>                       join (Just (Just (3::Int))) ~=? Just 3
>                     ]

> -- (f) Define the 'liftM' function
>

> liftM   :: (Monad m) => (a -> b) -> m a -> m b
> liftM = error "liftM: unimplemented"

> testLiftM :: Test
> testLiftM = TestList [ liftM not (Just True) ~=? Just False,
>                        liftM not [True,False] ~=? [False,True] ]

> -- Thought question: Is the type of `liftM` similar to that of another
> -- function we've discussed recently?

>

> -- (g) And its two-argument version ...

> liftM2  :: (Monad m) => (a -> b -> r) -> m a -> m b -> m r
> liftM2 = error "liftM2: unimplemented"

> testLiftM2 :: Test
> testLiftM2 = TestList [liftM2 (+) (Just (1::Int)) (Just 2) ~=? Just 3,
>                        liftM2 (+) [1,2] [3,4::Int] ~=? [4,5,5,6],
>                        liftM2 (,) [0..10] [0..8] ~=? grid 10 8 ]

> grid :: Int -> Int -> [(Int, Int)]
> grid m n = [(x, y) | x <- [0..m], y <- [0..n]]
