module Wip where
import Prelude hiding (last, replicate)

grid :: Int -> Int -> [(Int, Int)]
grid x y = [(a,b) | a <- [0 .. x], b <- [0 .. y]]

square :: Int -> [(Int, Int)]
square sq = [(a,b) | a <- [0 .. sq], b <- [0 .. sq], a /= b]

replicate :: Int -> a -> [a]
replicate n v = [v | _ <- [1 .. n]]

pyths :: Integer -> [(Integer, Integer, Integer)]
pyths n = [ (x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2 ]

perfects :: Int -> [Int]
perfects k = [l | l <- [1..k], l == sum(init(factors l))]
  where
    factors :: Int -> [Int]
    factors n = [x | x <- [1..n], n `mod` x == 0]

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct l1 l2 = sum [x * y | (x,y) <- zip l1 l2]  


-- init :: [a] -> [a]
-- init [] = error "Empty list"
-- init [a] = [a] 
-- init (x:xs) = take (length (x:xs) - 1) (x:xs)

-- initAlt :: [a] -> [a]
-- initAlt [] = error "Empty list"
-- initAlt [a] = [a]
-- initAlt (x:xs) = reverse (drop 1 (reverse (x:xs)))
        



-- last :: [a] -> a
-- last a head (last a)
--   where
--     last [] = []
--     last [x] = [x]
--     last (_:xs) = last xs
-- last a = reverse a !! 

-- thirdV1 :: [a] -> a
-- thirdV1 myList = head (tail (tail myList))

-- thirdV2 :: [a] -> a
-- thirdV2 myList = myList !! 2

-- thirdV3 :: [a] -> a
-- thirdV3 (x:y:z:xs) = z

-- last :: [a] -> a
-- last [] = error "Empty list"
-- last myList = myList !! (length myList - 1)

-- safetailV1 :: [a] -> [a]
-- safetailV1 myList =
--             if null myList then []
--             else tail myList

-- safetailV2 :: [a] -> [a]
-- safetailV2 myList | null myList = []
--                   | otherwise   = drop 1 myList

-- safetailV3 :: [a] -> [a]
-- safetailV3 []                   = error "Empty list"
-- safetailV3 (_:xs) | null xs     = []
                --   | otherwise   = xs