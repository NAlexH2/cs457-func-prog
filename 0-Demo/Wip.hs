
module Wip where
import Prelude hiding (mapM, sequence)
import qualified Control.Monad as Monad
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Text.Read as Text

import Data.Maybe

liftM2  :: (Monad m) => (a -> b -> r) -> m a -> m b -> m r
liftM2 f a b = do
              x <- a
              y <- b
              return (f x y)

liftM   :: (Monad m) => (a -> b) -> m a -> m b
liftM f a = do
          x <- a
          return (f x)

join :: (Monad m) => m (m a) -> m a
join xs = do
       x <- xs
       r <- x
       return r

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) f g x = f x >>= g


sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (x:xs) = do
                a <- x
                b <- sequence xs
                return (a:b)



foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM _ a []      = return a
foldM f a (x:xs) = do
                    b <- f a x
                    b' <- foldM f b xs
                    return b'

addEven :: [Int] -> Maybe Int
addEven = foldM f 0 where
               f x y | even x    = Just (x + y)
                     | otherwise = Nothing
safeDiv :: Int -> Int -> Maybe Int
safeDiv x y = if y == 0 then Nothing else Just (x `div` y)

-- sequenceFirst :: Maybe a -> Maybe b -> Maybe a
-- sequenceFirst x y = x >>= (\a -> case y of
--                                 Just _ -> x
--                                 Nothing -> Nothing)


-- firstJust :: Maybe a -> Maybe a -> Maybe a
-- firstJust x y = case x of
--                   Just _ -> x
--                   Nothing -> y

-- firstJust x y = if isNothing x == False then x
--                   else if isNothing y == False then y
--                   else Nothing 



-- https://hackage.haskell.org/package/containers-0.6.7/docs/Data-Map-Internal.html#v:lookup
-- https://hackage.haskell.org/package/base-4.17.0.0/docs/Text-Read.html#v:readMaybe
-- data Weather = Weather {
--     day :: Int, maxTemp :: Int, minTemp :: Int
--     } deriving (Eq, Show)
-- example :: Weather
-- example = Weather {day = 2, maxTemp = 78, minTemp = 62}

-- parseWeather :: Map String String -> Maybe Weather
-- parseWeather wMap = do
--       theDay <- Map.lookup "day" wMap
--       hTemp <- Map.lookup "maxTemp" wMap
--       lTemp <- Map.lookup "minTemp" wMap
--       tDM <- Text.readMaybe theDay
--       hTM <- Text.readMaybe hTemp
--       lTM <- Text.readMaybe lTemp
--       return (Weather tDM hTM lTM)

-- scalarProduct :: [Int] -> [Int] -> Int
-- scalarProduct a b = sum (prodListBuilder a b)
--   where
--     prodListBuilder :: [Int] -> [Int] -> [Int]
--     prodListBuilder j k = do
--       xys <- [zip j k]
--       y <- map (\(x,y) -> x * y) xys
--       return y

-- perfects :: Int -> [Int]
-- perfects a = do
--   x <- xs
--   qualityCheck x
--   where
--     xs = [1..a]
--     qualityCheck x = if x == sum(init(modCheck x xs)) then [x] else []
--     modCheck n xs =
--       do
--         x <- xs
--         if n `mod` x == 0 then return x else []

-- perfects a = do
--               xs <- filter (\y -> y == sum( init(factors y))) [1..a]
--               return xs
--  where
--   factors :: Int -> [Int]
--   factors n = [x | x <- [1..n], n `mod` x == 0]



-- perfects a  = do filter (\y -> y == sum( init(factors y))) [1..a]
--  where
--   factors a = (\n x -> map (n `mod`) x) a [1..a]
  -- factors n = [x | x <- [1..n], n `mod` x == 0]

-- listOfTuples :: [(Int,Char)]
-- listOfTuples = do
--     n <- [1,2]
--     ch <- ['a','b']
--     return (n,ch)

-- foldSort :: (Ord a, Foldable t) => t [a] -> [a]
-- foldSort a = foldMap sortedListSort a

-- realMergeSort :: Ord a => [a] -> [a]
-- realMergeSort a = foldSort (DivideList [a])

-- instance Foldable DivideList where
--   foldMap f xs =
--     case divide xs of
--       (DivideList as, DivideList bs) ->  foldMap f as `mappend` foldMap f bs


-- newtype DivideList a = DivideList { getDivideList :: [a] } deriving (Eq, Show)

-- divide :: DivideList a -> (DivideList a, DivideList a)
-- divide (DivideList []) = (DivideList [], DivideList [])
-- divide (DivideList xs) = (DivideList (take half xs), DivideList (drop half xs))
--   where
--     half = List.length xs `div` 2

-- -- divide2 :: DivideList a -> (DivideList a, DivideList a)
-- -- divide2 (DivideList xs) = getHalf xs


-- -- data Crispy a = Snap a [a] a
-- --               | Crackle [[Crispy a]]
-- --               | Pop Integer deriving (Eq,Show)

-- -- instance Foldable Crispy where
-- --   foldMap f (Snap a bs c) = foldMap f (a:c:bs)
-- --   foldMap f (Crackle []) = foldMap f []
-- --   foldMap f (Crackle xs) = foldMap (foldMap (foldMap f)) xs
-- --   foldMap f (Pop a) = foldMap f []






-- sumOfProducts :: Num a => [[a]] -> a
-- sumOfProducts a = getSum $ foldMapList 
--                   Sum [getProduct $ foldMapList Product x | x <- a]

-- foldMapList :: Monoid m => (a -> m) -> [a] -> m
-- foldMapList f = List.foldr (mappend . f) mempty

-- ten :: Int
-- ten = getSum (foldList (map Sum [1,2,3,4]))

-- twentyfour :: Int
-- twentyfour = getProduct (foldList (map Product [1,2,3,4]))

-- sortedFromList :: Ord a => [a] -> SortedList a
-- sortedFromList a = fromList a

-- sortedListSort :: Ord a => [a] -> [a]
-- sortedListSort a = toList (sortedFromList a)

-- sortedFromList' :: Ord a => [a] -> SortedList a
-- sortedFromList' = foldMapList singleton

-- sortedListSort' :: Ord a => [a] -> [a]
-- sortedListSort' = toList . sortedFromList'



-- -- insertionSort :: Ord a => [a] -> [a]
-- -- insertionSort = foldr insert []

-- insert :: Ord a => a -> [a] -> [a]
-- insert a [] = [a]
-- insert a [x] = if a >= x then x:[a] else a:[x]
-- insert a (b:c:cs) | a >= b && a <= c = b:a:c:cs
--                   | otherwise = b : c : insert a cs


-- count :: Eq a => SortedList a -> SortedList (a, Integer)
-- count a = SL [ (b, fromInt y) | x <- vals, y <- [List.length x], b <- [x !! 0] ]
--   where
--     vals = List.group(toList a)
--     fromInt = fromIntegral



-- -- numDistinct :: Ord a => SortedList a -> Int
-- -- numDistinct (SL []) = 0
-- -- numDistinct a = length (fromList (countedOn (toList a)))
-- --   where
-- --     countedOn []  = []
-- --     countedOn [x] = [x]
-- --     countedOn (x:y:zs)  | x == y = countedOn (y:zs)
-- --                         | otherwise = x : countedOn (y:zs)


-- minimum :: SortedList a -> Maybe a
-- minimum (SL []) = Nothing
-- minimum a = Just ((toList a) !! 0)
-- newtype SortedList a = SL [a] deriving (Eq, Show)

-- instance Ord a => Monoid (SortedList a) where
--   mappend :: Ord a => SortedList a -> SortedList a -> SortedList a
--   l1 `mappend` l2 = SL (myAppend (toList l1) (toList l2))
--     where
--       myAppend xs [] = xs
--       myAppend [] ys = ys
--       myAppend (x:xs) (y:ys)  | x <= y = x : myAppend xs (y:ys)
--                               | otherwise = y : myAppend (x:xs) ys
--   mempty :: Ord a => SortedList a
--   mempty = SL []

-- instance Ord a => Semigroup (SortedList a) where
--   (<>) :: Ord a => SortedList a -> SortedList a -> SortedList a
--   (<>) = mappend


-- foldList :: Monoid b => [b] -> b
-- foldList = List.foldr mappend mempty

-- -- | convert to a regular list. The elements should be produced in order.
-- toList :: SortedList a -> [a]
-- toList (SL as) = as

-- -- | convert from a regular list.
-- fromList :: Ord a => [a] -> SortedList a
-- fromList = foldList . map singleton

-- {-
-- Some of the operations that we define for sorted lists just delegate
-- to the version for regular lists.
-- -}

-- -- | construct a sorted list containing a single element
-- singleton :: a -> SortedList a
-- singleton a = SL [a]

-- -- | reduce a SortedList in order
-- foldr :: (a -> b -> b) -> b -> SortedList a -> b
-- foldr f b (SL xs) = List.foldr f b xs

-- -- | decide which elements of the sorted list to keep
-- filter :: (a -> Bool) -> SortedList a -> SortedList a
-- filter f (SL xs) = SL (List.filter f xs)

-- -- | count the number of elements in the sorted list
-- length :: SortedList a -> Int
-- length (SL xs) = List.length xs

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