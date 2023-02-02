> module HigherOrderFunctions where
> import Prelude hiding (foldl, foldr, map, filter, sum, product, or, and)

> add :: Int -> (Int -> Int)
> add = \x -> \y -> x + y

> twice :: (a -> a) -> a -> a
> twice f = f . f -- twice x = f (f x)

Map
===========

> map :: (x -> y) -> [x] -> [y]

map _ [] = []
map f (x:xs) = f x : map f xs

> map f = foldr (\x -> (f x :)) []

> filter :: (a -> Bool) -> [a] -> [a]
> filter f xs = [ x | x <- xs, f x ]

all, any, takeWhile, dropWhile, zipWith

Foldr
============

f []     = v
f (x:xs) = x # f xs

sum :: [Int] -> Int
sum = foldr (+) 0

sum [1, 2, 3]
==> foldr (+) 0 [1,2,3]
==> 1 + (foldr (+) 0 [2,3])
==> 1 + (2 + (foldr (+) 0 [3]))
==> 1 + (2 + (3 + (foldr (+) 0 [])))
==> 1 + (2 + (3 + 0))
==> 1 + 2 + 3 ==> 6

> product :: [Int] -> Int
> product = foldl (*) 1

> or :: [Bool] -> Bool
> or = foldl (||) False

> and :: [Bool] -> Bool
> and = foldl (&&) True

f :: (x -> y)
xs :: [x]
foldr :: (x -> [y] -> [y]) -> [y] -> [x] -> [y]
x :: x
------------------------
foldr (\x ys -> f x : ys) [] xs

b   == [y]
[a] == [x]
a   == x

> foldr :: (a -> b -> b) -> b -> [a] -> b
> foldr _ v [] = v
> foldr g v (x:xs) = x `g` (foldr g v xs)


> length :: [a] -> Int

length [] = 0
length (x:xs) = 1 + length xs

> length = foldl (\y _ -> 1 + y) 0

> reverse :: [a] -> [a]

reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

> reverse = foldr snoc []

> snoc :: a -> [a] -> [a]
> snoc x xs = xs ++ [x]

reverse (x:xs) = (\x xs -> xs ++ [x]) x (reverse xs)
==> reverse xs ++ [x]

Foldl
============

> sum :: [Int] -> Int
> sum = sum' 0
>       where sum' :: Int -> [Int] -> Int
>             sum' acc [] = acc
>             sum' acc (x:xs) = sum' (acc + x) xs

sum [1, 2, 3]
==> sum' 0 [1, 2, 3]
==> sum' (0 + 1) [2, 3]
==> sum' (0 + 1 + 2) [3]
==> sum' (0 + 1 + 2 + 3) []
==> 0 + 1 + 2 + 3


f v []     = v
f v (x:xs) = f (v # x) xs

> foldl :: (a -> b -> a) -> a -> [b] -> a
> foldl g v [] = v
> foldl g v (x:xs) = foldl g (v `g` x) xs

f == foldl g
v == v
(#) == `g`


product :: Num a => [a] -> a

or :: [Bool] -> Bool

and :: [Bool] -> Bool


length :: [a] -> Int

reverse :: [a] -> [a]




Function composition
====================

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)


Binary encoder
====================

1011

1 +
1 * 2 +
0 * 2 * 2 +
1 * 2 * 2 * 2 = 1 + 2 + 8 = 11

1 + 2 * (1 + 2 * (0 + 2 * 1))

> data Bit = One | Zero
>       deriving (Eq, Show)

> type Bin = [Bit]

> bit2int :: Bit -> Int
> bit2int One  = 1
> bit2int Zero = 0

> int2bit :: Int -> Bit
> int2bit 0 = Zero
> int2bit _ = One

> bin2int :: Bin -> Int
> bin2int = foldr (\b n -> bit2int b + 2 * n) 0

bin2int [] = 0
bin2int (b:bs) = bit2int b + 2 * bin2int bs

> int2bin :: Int -> Bin
> int2bin 0 = [Zero]
> int2bin n = int2bit (n `mod` 2) : int2bin (n `div` 2)
