module Wip where
import Prelude hiding (transpose)


concat :: [[a]] -> [a]
concat [[],[],[]] = []
concat [[]] = []
concat [] = []
concat xs = foldr (\x r -> foldr )


endsWith :: String -> String -> Bool
endsWith [] [] = True
endsWith  _ [] = True
endsWith []  _ = False
endsWith xs (y:ys)  | strEq xs (y:ys) = True
                    | otherwise = endsWith xs ys
    where
      strEq :: String -> String -> Bool
      strEq [] [] = True
      strEq (x:xs) (y:ys) | x == y = strEq xs ys
                          | otherwise = False
      strEq _ _ = False


-- Things you've thought about and tried...
---- cases
---- passing until xs == "" but not ys
---- utilizing the base cases to act as a stopping condition to start matching
----      chars with eachother.
---- Considering how to start from the end of each without recursion then going
----    backwards from there.


startsWith :: String -> String -> Bool
startsWith [] [] = True
startsWith _ [] = False
startsWith [] _ = True
startsWith (x : xs) (y : ys)  | x == y && xs == "" = True
                              | x == y = startsWith xs ys
                              | otherwise = False


minimumMaybe :: [Int] -> Maybe Int
minimumMaybe [] = Nothing
minimumMaybe (x:xs) = case minimumMaybe xs of
              Nothing -> Just x
              Just y -> if x < y then Just x
                        else Just y

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