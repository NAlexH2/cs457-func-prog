{-# LANGUAGE InstanceSigs #-}
module W5D2 where

import Prelude hiding (Monad, return, (>>=), (>>),
                        Applicative, pure, (<*>),
                        Functor, fmap, (<$>), log)

import Data.Kind
import Lec1 (a)
import Data.Time.Format.ISO8601 (yearFormat)

class Monad m where -- m == Monad
    return :: a -> m a
    -- bind
    -- Where m a is a container to be converted to container of m b
    (>>=) :: m a -> (a -> m b) -> m b 
    -- example
    -- m == [], a == Int, b == String
    -- (>>=) :: [Int] -> (Int -> [String]) -> [String]
  
instance Monad Maybe where
  return :: a -> Maybe a
  return = Just -- or -> return x = Just x
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing >>= k = Nothing
  -- where x is Maybe a, k is a function, and we return a Maybe b of x
  Just x >>= k = k x

instance Monad [] where
  return :: a -> [a]
  return x = [x]
  (>>=) :: [a] -> (a -> [b]) -> [b]
  []      >>= k = []
  (x:xs)  >>= k = k x ++ (xs >>= k)

-- instance Monad IO where
  -- return :: a -> IO a
  -- (>>=) :: IO a -> (a -> IO b) -> IO b

-- now how do we add some numbers together?

newtype Log a = Log (a, [String])

logging :: String -> Log ()
logging s = Log ((), [s])

-- (>>=) :: Log Int -> (Int -> Log Int) -> Log Int

-- writer monad
instance Monad Log where
  return :: a -> Log a
  return x = Log (x, [])
  (>>=) :: Log a -> (a -> Log b) -> Log b
  (Log (a, s1)) >>= k = case k a of
                          Log (b, s2) -> Log (b, s1 ++ s2)


plus :: Int -> Int -> Log Int
-- new soln
plus x y = 
  logging (show x ++ "+" ++ show y ++ "=" ++ show (x + y)) >>=
    (\_ -> return (x + y))

  -- Og soln
  -- Log (x + y, [show x ++ "+" ++ show y ++ "=" ++ show (x + y)])

plus3 :: Int -> Int -> Int -> Log Int
plus3 x y z = plus x y >>= (\a -> plus a z)
-- or
-- plus3 x y z = do 
--     a <- plus x y
--     b <- plus a z
--     return b
