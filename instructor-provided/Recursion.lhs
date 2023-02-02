> module Recursion where
> import Prelude hiding (reverse, zip, drop, odd, even, product)

Recursion on lists
=================================

> reverse :: [a] -> [a]
> reverse [] = []
> reverse (x:xs) = reverse xs ++ [x]

> insert :: Ord a => a -> [a] -> [a]
> insert x [] = [x]
> insert x (y:ys) | x <= y    = x : y : ys
>                 | otherwise = y : insert x ys

> isort :: Ord a => [a] -> [a]
> isort [] = []
> isort (x:xs) = insert x $ isort xs

Recursion on multiple arguments
=================================

> zip :: [a] -> [b] -> [(a, b)]
> zip (x:xs) (y:ys) = (x, y) : zip xs ys
> zip _ _ = []

> drop :: Int -> [a] -> [a]
> drop 0 xs = xs
> drop _ [] = []
> drop n (x:xs) | n > 0 = drop (n-1) xs
>               | otherwise = []

> qsort :: Ord a => [a] -> [a]
> qsort [] = []
> qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
>       where smaller = [y | y <- xs, y <= x]
>             larger  = [y | y <- xs, y >  x]

Mutual recursion
=================================

> data Nat = Zero | Succ Nat

> even :: Nat -> Bool
> even Zero = True
> even (Succ n) = odd n

> odd :: Nat -> Bool
> odd Zero = False
> odd (Succ n) = even n

All numbers from even and odd positions:

evens :: [a] -> [a]

odds :: [a] -> [a]


Exercises on recursion
==================================


Steps on defining recursive functions:

1. Define the type
2. enumerate the cases
3. Define the simple cases
4. Define the other cases
5. Generalize and simplify


> product :: Num a => [a] -> a
> product []     = 1
> product (x:xs) = x * product xs


drop :: Int -> [a] -> [a]

init :: [a] -> [a]
