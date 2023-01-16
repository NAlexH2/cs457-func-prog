module Wip where
import Prelude hiding (last, init)

n :: Int
n = a `div` length xs
  where
     a = 10
     xs = [1, 2]

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

thirdV1 :: [a] -> a
thirdV1 myList = head (tail (tail myList))

thirdV2 :: [a] -> a
thirdV2 myList = myList !! 2

thirdV3 :: [a] -> a
thirdV3 (x:y:z:xs) = z

last :: [a] -> a
last [] = error "Empty list"
last myList = myList !! (length myList - 1)

safetailV1 :: [a] -> [a]
safetailV1 myList =
            if null myList then []
            else tail myList

safetailV2 :: [a] -> [a]
safetailV2 myList | null myList = []
                  | otherwise   = drop 1 myList

safetailV3 :: [a] -> [a]
safetailV3 []                   = error "Empty list"
safetailV3 (_:xs) | null xs     = []
                  | otherwise   = xs