-- | Name: Alex Harris
-- | Date: 2/4/2023
-- | Assignment 4

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
Merge Sort
==========
-}

module MergeSort where

import SortedList (SortedList)
import qualified SortedList as SL
-- import qualified Data.List as List
import Data.Monoid
import Test.HUnit
import GHC.Base (undefined)

{-
A warm-up exercise: write the function `insert`, which takes an element and a
sorted list and inserts the element into the list at the first position where it
is less than or equal to the next element. For this definition, do not use any
functions from the `Data.List` library (which, indeed, contains such a
function).
-}

-- TODO: define the following function
insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert a [x] = if a >= x then x:[a] else a:[x]
insert a (b:c:cs) | a >= b && a <= c = b:a:c:cs
                  | otherwise = b : c : insert a cs

{-
Using this function, we can define the *insertion sort* algorithm over lists.
Insertion sort, like several other sorting algorithms, is *incremental*---it
works by processing a single element of the input unsorted list at a time, and
when it finishes processing the input, its work is done. Using the `insert`
function above, write a function which takes a list and returns the list sorted.
You must express your answer in terms of a fold.
-}

-- TODO: define the following function
insertionSort :: Ord a => [a] -> [a]
insertionSort a = foldr insert [] a
      

{-
Keep in mind that this sorting algorithm, although succinct, is not
efficient---it has O(N ^ 2) asymptotic complexity.

You may have noticed that the `insert` function above has an invariant attached
to its description; namely, that its list argument is already sorted. If that
invariant holds, then `insert` guarantees that its output will also be sorted;
otherwise, there is no such guarantee.

A leading question: what if we could keep track of the "sorted-ness" of a list
using the type system? You already know that we can---that's precisely what we
did in `SortedList.lhs`.
-}

-------------------------------------------------------------

{-
SortedLists to the Rescue
-------------------------

I previously promised that the interface we built for `SortedList`s would be
sufficient to do useful things with them. One particularly obvious useful thing
to do with `SortedList`s is to construct them from arbitrary lists---that is, to
sort an arbitrary list and produce a `SortedList` with the result. This function
is already defined in the `SortedList` module as `fromList`.)
-}

sortedFromList :: Ord a => [a] -> SortedList a
sortedFromList a = SL.fromList a

{-
By projecting out the underlying list, we get a sorting function, that we'll
call `sortedListSort`.
-}

sortedListSort :: Ord a => [a] -> [a]
sortedListSort a = SL.toList (sortedFromList a)

testSortedFromList :: Test
testSortedFromList =
  let unsorted = [51,67,89,95,14,31,28,87,0,25]
      sorted   = [0,14,25,28,31,51,67,87,89,95] in
  sortedListSort unsorted ~?= sorted

{-
One thing you may have noticed while writing the above function is that there is
only one place you could have made reference to the `SortedList` type
specifically: in the use of the `singleton` operation. Indeed, this operation is
the only `SortedList`-specific way to create new `SortedList`s---any other way
comes through the `Monoid` instance. Perhaps there's some common pattern here
that we could abstract! (There is.) Let's express it by making the `singleton`
function into a parameter of a new function, that we will call `foldMapList`, so
that we can rewrite the algorithm above like so:
-}

sortedFromList' :: Ord a => [a] -> SortedList a
sortedFromList' = foldMapList SL.singleton

{-
Again, we can project out the underlying list to get a list sorting function.
-}

sortedListSort' :: Ord a => [a] -> [a]
sortedListSort' = SL.toList . sortedFromList'

testSortedFromList' :: Test
testSortedFromList' =
  let unsorted :: [Int]
      unsorted = [47,80,28,47,45,76,1,35,19,1] in
  sortedListSort' unsorted ~?= sortedListSort unsorted  -- old & new agree

{-
In order to make this work, you need to define the `foldMapList` combinator.
-}

-- TODO: define the following function
foldMapList :: Monoid m => (a -> m) -> [a] -> m
foldMapList f a = foldr distributeMappend mempty a
  where
    distributeMappend e r = (f e) `mappend` r

{-
The type of `foldMapList` is very general---we can use this function to combine
arbitrary lists by providing a function that maps their contents to some
particular `Monoid` (such as `SortedList`). For instance, `foldMapList Sum`
gives us the sum of the numbers in the list; `foldMap Product` gives us their
product.

A small exercise: using `foldMapList` and the `Sum` and `Product` newtypes you
learned about in `SortedList.lhs`, implement the following function, which takes
a doubly-nested list of numbers as input, and returns the sum of the product of
each of the inner lists. In your solution, do not explicitly use any of the
ordinary numeric operations like `(+)`, `(*)`, `sum`, or `product`, and eschew
explicit recursion.
-}

-- TODO: define the following function
sumOfProducts :: Num a => [[a]] -> a
sumOfProducts a = getSum $ foldMapList 
                  Sum [getProduct $ foldMapList Product x | x <- a]

testSumOfProducts :: Test
testSumOfProducts = sumOfProducts 
                    [[1],[2,3],[4,5,6],[7,8,9,10]] ~?= (5167 :: Int)


{-
Like Merge Sort, the `sortedListSort` function is based on merging sorted lists
together. This merge-sort-like algorithm has a flaw, though: it's quadratic in
runtime. Why?
-}

benchmark :: IO ()
benchmark = (print . last . sortedListSort') ([10000,9999..0] :: [Int])

{-
(Try it yourself by setting `:set +s` in ghci. On my machine, it take 12.01 secs
and allocates 21,353,389,296 bytes))

For any singleton `SortedList [a]` and any other `SortedList as`, computing
`mappend (SortedList [a]) (SortedList as)` is identical not only in resultant
value, but also in algorithmic structure to computing the result of `insert a
as`. The definition of `foldMapList` linearly scans across its input list,
successively combining values using `mappend`---and so, like insertion sort, the
whole whole algorithm ends up executing a quadratic number of comparisons.

A real merge sort algorithm, as you likely know, divides its input more
intelligently than the one we've written above in terms of `foldMapList`. By
dividing its input roughly in half every iteration, it only has to do a
logarithmic amount of merging.

To make our merge sort do this, we need to use a different kind of `foldMap`!

The Foldable Typeclass
----------------------

At this point, I'd like to point something out: `foldMapList` can itself be even
further generalized. We already know that lists are not the only data structures
which support folding---we've seen folds for trees of various kinds and for
other data structures as well. As a result, it makes sense to allow some kind of
`foldMap` operation for those structures also. In the standard library, we
therefore have:

< foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m

That is to say, `foldMap` is a method of yet another type class, `Foldable`, of
which the type constructor `[]` is an instance. Implementing this interface
roughly corresponds to saying, "this data structure contains some elements, and
I know how to do a fold across them." To implement the `Foldable` class for some
type, we just need to implement `foldMap`.

Implement the `Foldable` instance for the `Crispy` datatype. Remember to keep in
mind a guiding principle: when you are confused, don't think about what things
are supposed to mean; just follow the types and see where they take you.
-}

data Crispy a = Snap a [a] a
              | Crackle [[Crispy a]]
              | Pop Integer deriving (Eq,Show)



-- TODO: define the following instance
instance Foldable Crispy where
  foldMap f (Snap a bs c) = foldMap f (a:c:bs)
  foldMap f (Crackle []) = foldMap f []
  foldMap f (Crackle xs) = foldMap (foldMap (foldMap f)) xs
  foldMap f (Pop a) = foldMap f []

testCrispy :: Test
testCrispy =
  let c1, c2 :: Crispy Integer
      c1 = Snap 700 [] 600
      c2 = Pop 1234567890
      c3 = foldMap (show . subtract 1) (Crackle [[c1], [c2]]) in
  TestList [ 
            1300 ~?= getSum (foldMap Sum c1), 
            1 ~?= getProduct (foldMap Product c2), 
            "699599" ~?= c3
           ]

-------------------------------------------------------------

{-
Back to Sorting
---------------

In order to express an efficient merge sort in terms of `foldMap`, we need to
design a data structure that represents a sequence of elements (just like a
list), but whose `Foldable` instance uses a divide-and-conquer strategy, rather
than the `[]` instance's linear fold pattern of recursion.
-}

newtype DivideList a = DivideList { getDivideList :: [a] } deriving (Eq, Show)

{-
That means that we need `DivideList`s to be `Foldable`, but in a different way.
First, implement the `divide` function, which splits a `DivideList` in its
middle, returning the result of the split.
-}


-- TODO: define the following function
divide :: DivideList a -> (DivideList a, DivideList a)
divide (DivideList []) = (DivideList [], DivideList [])
divide (DivideList xs) = getHalf xs
  where
    half = length xs `div` 2
    getHalf xs =  (DivideList (take half xs), DivideList (drop half xs))

testDivide :: Test
testDivide = TestList [ divide (DivideList "abcd") ~?=
                          (DivideList "ab", DivideList "cd"),
                        divide (DivideList "abcde") ~?=
                          (DivideList "ab", DivideList "cde"),
                        divide (DivideList "") ~?=
                          (DivideList "", DivideList "") ]

{-
Using this function, we can define the `Foldable` instance for `DivideList`s.
Note that this definition is trickier than it seems. If you encounter an
infinite loop, it means that you have not covered one of a particular set of
slightly non-trivial edge cases.
-}

-- TODO: define the following instance
instance Foldable DivideList where
  foldMap f xs =
    case divide xs of
      (DivideList as, DivideList bs) ->  foldMap f as `mappend` foldMap f bs

testDivideList :: Test
testDivideList =
  let xs = DivideList [1,2,3]
      ys = DivideList [] in
  TestList [ Product (6 :: Int) ~?= foldMap Product xs
           , Sum (0 :: Int) ~?= foldMap Sum ys
           ]

{-
Now that we know how general the `foldMap` function is, have a look at the
implementation of `sortedListSort'` above -- does its input type need to only be
a list? Generalize its type signature so that it outputs a list of sorted
elements located inside an arbitrary `Foldable` structure.
-}

-- TODO: write down the type signature for ``foldSort` and define the function.
foldSort :: (Ord a, Foldable t) => t [a] -> [a]
foldSort a = foldMap sortedListSort a -- implementation should use foldMap


{-
By parameterizing over any `Foldable` container, what we've done is to *factor
out the folding strategy* into the choice of original container! To pick a
different divide-and-conquer strategy, we need only specify a different
container type, and give it a `Foldable` instance that folds along different
creases.

So, while our `sortedListSort` was O(N ^ 2), we can produce a differently
structured algorithm by instead folding over a `DivideList` instead:
-}


realMergeSort :: Ord a => [a] -> [a]
realMergeSort a = foldSort (DivideList [a])
-- I modified this function to make better sense while reading it to myself
-- was: realMergeSort = foldSort . DivideList
-- Which I understand, but it feels like just syntactic surgar and slightly
-- harder to read for a noob.

{-
If you've done everything correctly, this main function should return rather
quickly. This is much faster than the example above. On my machine it takes
(0.12 secs, 98,375,216 bytes).
-}

main :: IO ()
main = (print . last . realMergeSort) ([10000,9999..0] :: [Int])


{-
Concluding Thoughts About This Exercise
---------------------------------------

The important takeaway here is this: `foldMap` defines once and for all a
universal "divide-and-conquer" algorithm---all we need to do to use it is to
provide a way to "divide" an input container (i.e. give a `Foldable instance`),
then give a way to compute on those elements (i.e. the mapped function `a -> m`)
and a way to combine ("conquer") the results of that computation (i.e. a
`Monoid` instance for the result type).

Almost any divide-and-conquer algorithm can be fit into this framework, and that
means we can avoid repeating ourselves when writing such programs. We can reuse
the division strategy of `DivideList` when writing some other algorithm, and
likewise for the sorted-merging combination strategy of `SortedList`.
-}
