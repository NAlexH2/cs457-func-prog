{-# LANGUAGE InstanceSigs #-}
module Wip where
import Prelude hiding (foldr, length, filter, minimum)
import qualified Data.List as List
import Data.Monoid


newtype SortedList a = SL [a] deriving (Eq, Show)
sumOfProducts :: Num a => [[a]] -> a
sumOfProducts a = getSum foldMapList getProduct [x | xs <- a, x <- xs]

ten :: Int
ten = getSum (foldList (map Sum [1,2,3,4]))

twentyfour :: Int
twentyfour = getProduct (foldList (map Product [1,2,3,4]))


sortedFromList' :: Ord a => [a] -> SortedList a
sortedFromList' = foldMapList singleton

sortedListSort' :: Ord a => [a] -> [a]
sortedListSort' = toList . sortedFromList'

foldMapList :: Monoid m => (a -> m) -> [a] -> m
foldMapList f a = foldMap f a

-- insertionSort :: Ord a => [a] -> [a]
-- insertionSort = foldr insert []

-- insert :: Ord a => a -> [a] -> [a]
-- insert a [] = [a]
-- insert a [x] = if a >= x then x:[a] else a:[x]
-- insert a (b:c:cs) | a >= b && a <= c = b:a:c:cs
--                   | otherwise = b : c : insert a cs


count :: Eq a => SortedList a -> SortedList (a, Integer)
count a = SL [ (b, fromInt y) | x <- vals, y <- [List.length x], b <- [x !! 0] ]
  where
    vals = List.group(toList a)
    fromInt = fromIntegral



numDistinct :: Ord a => SortedList a -> Int
numDistinct (SL []) = 0
numDistinct a = length (fromList (countedOn (toList a)))
  where
    countedOn []  = []
    countedOn [x] = [x]
    countedOn (x:y:zs)  | x == y = countedOn (y:zs)
                        | otherwise = x : countedOn (y:zs)


minimum :: SortedList a -> Maybe a
minimum (SL []) = Nothing
minimum a = Just ((toList a) !! 0)
instance Ord a => Monoid (SortedList a) where
  mappend :: Ord a => SortedList a -> SortedList a -> SortedList a
  l1 `mappend` l2 = SL (myAppend (toList l1) (toList l2))
    where
      myAppend xs [] = xs
      myAppend [] ys = ys
      myAppend (x:xs) (y:ys)  | x <= y = x : myAppend xs (y:ys)
                              | otherwise = y : myAppend (x:xs) ys
  mempty :: Ord a => SortedList a
  mempty = SL []

instance Ord a => Semigroup (SortedList a) where
  (<>) :: Ord a => SortedList a -> SortedList a -> SortedList a
  (<>) = mappend


foldList :: Monoid b => [b] -> b
foldList = List.foldr mappend mempty

-- | convert to a regular list. The elements should be produced in order.
toList :: SortedList a -> [a]
toList (SL as) = as

-- | convert from a regular list.
fromList :: Ord a => [a] -> SortedList a
fromList = foldList . map singleton

{-
Some of the operations that we define for sorted lists just delegate
to the version for regular lists.
-}

-- | construct a sorted list containing a single element
singleton :: a -> SortedList a
singleton a = SL [a]

-- | reduce a SortedList in order
foldr :: (a -> b -> b) -> b -> SortedList a -> b
foldr f b (SL xs) = List.foldr f b xs

-- | decide which elements of the sorted list to keep
filter :: (a -> Bool) -> SortedList a -> SortedList a
filter f (SL xs) = SL (List.filter f xs)

-- | count the number of elements in the sorted list
length :: SortedList a -> Int
length (SL xs) = List.length xs

-- concat :: [[a]] -> [a]
-- concat [[],[],[]] = []
-- concat [[]] = []
-- concat [] = []
-- concat (x:xs) = foldr (\y r -> y:xs) [] concat xs


-- endsWith :: String -> String -> Bool
-- endsWith [] [] = True
-- endsWith  _ [] = True
-- endsWith []  _ = False
-- endsWith xs (y:ys)  | strEq xs (y:ys) = True
--                     | otherwise = endsWith xs ys
--     where
--       strEq :: String -> String -> Bool
--       strEq [] [] = True
--       strEq (x:xs) (y:ys) | x == y = strEq xs ys
--                           | otherwise = False
--       strEq _ _ = False


-- Things you've thought about and tried...
---- cases
---- passing until xs == "" but not ys
---- utilizing the base cases to act as a stopping condition to start matching
----      chars with eachother.
---- Considering how to start from the end of each without recursion then going
----    backwards from there.


-- startsWith :: String -> String -> Bool
-- startsWith [] [] = True
-- startsWith _ [] = False
-- startsWith [] _ = True
-- startsWith (x : xs) (y : ys)  | x == y && xs == "" = True
--                               | x == y = startsWith xs ys
--                               | otherwise = False


-- minimumMaybe :: [Int] -> Maybe Int
-- minimumMaybe [] = Nothing
-- minimumMaybe (x:xs) = case minimumMaybe xs of
--               Nothing -> Just x
--               Just y -> if x < y then Just x
--                         else Just y

  -- where 
  --   minimumAux :: [Int] -> Int
  --   minimumAux (x:xs) =  if xs == [] then x
  --                         else if x < minimumAux xs then x
  --                         else if x > minimumAux xs then minimumAux xs
  --                         else minimumAux xs



-- grid :: Int -> Int -> [(Int, Int)]
-- grid x y = [(a,b) | a <- [0 .. x], b <- [0 .. y]]

-- square :: Int -> [(Int, Int)]
-- square sq = [(a,b) | a <- [0 .. sq], b <- [0 .. sq], a /= b]

-- replicate :: Int -> a -> [a]
-- replicate n v = [v | _ <- [1 .. n]]

-- pyths :: Integer -> [(Integer, Integer, Integer)]
-- pyths n = [ (x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2 ]

-- perfects :: Int -> [Int]
-- perfects k = [l | l <- [1..k], l == sum(init(factors l))]
--   where
--     factors :: Int -> [Int]
--     factors n = [x | x <- [1..n], n `mod` x == 0]

-- scalarProduct :: [Int] -> [Int] -> Int
-- scalarProduct l1 l2 = sum [x * y | (x,y) <- zip l1 l2]  


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