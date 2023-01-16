module Functions where

-- This is an ordinary Haskell program, we have code and comments.

import GHC.Float.RealFracMethods

-- test if the number is even
even :: Integral a => a -> Bool
even x = x `mod` 2 == 0

-- split a list at a position
splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

-- reciprocal
recip :: Fractional a => a -> a
recip x = 1 / x

-- Conditional expressions and guarded expressions

-- absolute value
abs :: Int -> Int
abs x | x >= 0    = x
      | otherwise = -x

-- return 1 if the number is positive, -1 if negative, and 0 if 0
signum :: Int -> Int
signum x | x > 0 = 1
         | x < 0 = -1
         | otherwise = 0

-- Pattern macthing

-- Boolean not
not :: Bool -> Bool
not True  = False
not False = True

-- Boolean and
(&&) :: Bool -> Bool -> Bool
x && y | x == y = x
       | otherwise = False

-- first element in the tuple
fst :: (a, b) -> a
fst (a, _) = a

-- second element in the tuple
snd :: (a, b) -> b
snd (_, a) = a

-- convert something that is either an Int or Float to Int (rounded down)
-- Hint: floorFloatInt
convert :: Either Int Float -> Int
convert (Left x) = x
convert (Right x) = floorFloatInt x

-- test if the first element is 'a'
test :: [Char] -> Bool
test (a : _) | a == 'a'  = True
             | otherwise = False
test _         = False

-- the first element in the list
head :: [a] -> a
head (x : _) = x

-- the list execpt for its first element
tail :: [a] -> [a]
tail (_ : xs) = xs

-- Lambda expressions

-- add two Ints togetehr
add :: Int -> (Int -> Int)
add = \x -> (\y -> x + y)

-- always return the first argument
const :: a -> b -> a
const a = \_ -> a

-- the first n odd integers
odds :: Int -> [Int]
odds n = map (\x -> 2 * x + 1) [0..n-1]
