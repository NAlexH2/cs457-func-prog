module Wip where
import Prelude hiding (last, init)

n :: Int
n = a `div` length xs
  where
     a = 10
     xs = [1, 2]

-- init :: [a] -> [a]
-- init [] = error "Empty List"
-- init [a] = [a]
-- init (x:y) = 


-- initAlt :: [a] -> [a]
-- initAlt [] = error "Empty List"
-- initAlt [a] = reverse [a]
-- initAlt (x:y) = reverse (x : y)
