module Monads where

import Prelude hiding (Monad, return, (>>=), (>>),
                       Applicative, pure, (<*>),
                       Functor, fmap, (<$>), log)

import Data.Kind

class Monad (m :: Type -> Type) where
  return :: forall a. a -> m a
  -- bind
  (>>=) :: forall a b. m a -> (a -> m b) -> m b

-- data Maybe a = Nothing | Just a

instance Monad Maybe where
  return :: forall a. a -> Maybe a
  return x = Just x
  (>>=) :: forall a b. Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing >>= k = Nothing
  Just x  >>= k = k x

  m >>= k = case m of
              Nothing -> Nothing
              Just x -> k x


{--
  m == []
  a == Int
  b == Bool -> Double

  (>>=) :: [Int] -> (Int -> [Bool -> Double]) -> [Bool -> Double]
--}

instance Monad [] where
  return :: forall a. a -> [a]
  return x = [x]
  (>>=) :: forall a b. [a] -> (a -> [b]) -> [b]
  (>>= )[] k = []
  (x:xs) >>= k = k x ++ (xs >>= k)

--  bind :: forall a b. [a] -> (a -> [b]) -> [b]
--  [] `bind` k = ...

{--
instance Monad IO where
  return :: forall a. a -> IO a
  (>>=) :: forall a b. IO a -> (a -> IO b) -> IO b
--}


newtype Log a = Log (a, [String]) deriving Show

logging :: String -> Log ()
logging s = Log ((), [s])

instance Monad Log where
  return :: forall a. a -> Log a
  return x = Log (x, [])
  (>>=) :: forall a b. Log a -> (a -> Log b) -> Log b
  (Log (a, s1)) >>= k = case k a of
                          Log (b, s2) -> Log (b, s1 ++ s2)


plus :: Int -> Int -> Log Int
plus x y =
  logging (show x ++ "+" ++ show y ++ "=" ++ show (x + y)) >>=
  (\_ -> return (x + y))

--  Log (x + y, [show x ++ "+" ++ show y ++ "=" ++ show (x + y)])

plus3 :: Int -> Int -> Int -> Log Int
plus3 x y z =
  plus x y >>=
  (\a -> plus a z >>=
    (\b -> return b))
