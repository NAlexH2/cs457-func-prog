module W3D2 where
import Prelude hiding (map, filter, sum, product, or, and, reverse, length)


-- add :: Int -> (Int -> Int)
-- add = \x -> \y -> x+y

twice :: (a -> a) -> a -> a
twice f = f . f -- same as -- > twice x = f (f x)


-- Map
map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]
-- Requires a function tied in with this when called

filter :: (a -> Bool) -> [a] -> [a]
filter f xs = [x | x <- xs, f x]

-- all, any, takeWhile, dropWhile, zipWith

-- Foldr
----------------
-- f []      = v
-- f (x:xs)  = x # f xs

-- sum :: [Int] -> Int
-- sum = foldr (+) 0
-- sum [] = 0
-- sum (x:xs) = x + sum xs

product :: [Int] -> Int
product = foldr (*) 1
-- product = foldl (*) 1

-- product [] = 1
-- product (x:xs) = x * product xs

or :: [Bool] -> Bool
or = foldr (||) False
-- or = foldl (||) False

-- or [] = False
-- or (x:xs) = x || or xs

and :: [Bool] -> Bool
and = foldr (&&) True
-- and = foldl (&&) True

-- and [] = True
-- and (x:xs) = x && and xs


-- foldr :: (a->b->b) -> b -> [a] -> b
-- foldr pattern matches all similar patters allowing us to simply call it
-- foldr _ v [] = v
-- foldr g v (x:xs) = g x (foldr g v xs)

length :: [a] -> Int
length xs = foldr (\_ y -> 1 + y) 0 xs

-- reverse :: [a] -> [a]
-- reverse [] = []
-- reverse (x:xs) = reverse xs ++ [x]
-- reverse xs = foldr (\x xs -> xs ++ [x]) [] xs
-- snoc x xs = xs ++ [x]
-- reverse = foldr snoc []



-- Foldl
--------------------
-- f v []       = v
-- f v (x:xs)   = f (v # x) xs
-- Where # is our function 


-- sum :: [Int] -> Int
-- sum xs = sum' 0 xs
--       where sum' :: Int -> [Int] -> Int
--       sum' acc [] = acc
--       sum' acc (x:xs) = sum' (x + acc) xs


-- Function Composition
---------------------------------
-- (.) :: (b -> c) -> (a -> b) -> (a ->c )
-- f . g = \x -> f (g x)
-- h x = f (g x)
-- h = f . g




-- Binary Encoder 
---------------------------------
-- 1011
-- 1 * 2^0
-- 1 * 2^1
-- 0 * 2^2
-- 1 * 2^3
-- ++++ = 1 + 2 + 0 + 8 = 11
-- to do in haskell?

data Bit = One | Zero
        deriving (Eq, Show)
        
type Bin = [Bit]

bit2int :: Bit -> Int
bit2int One  = 1
bit2int Zero = 0

bin2int :: Bin -> Int
bin2int = foldr (\b n -> bit2int b + 2 * n) 0
-- bin2int [] = 0
-- bin2int (b:bs) = bit2int b + 2 * bin2int bs