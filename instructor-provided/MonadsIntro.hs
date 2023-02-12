module MonadsIntro where

import Data.Map
import Data.Kind

implies :: Bool -> Bool -> Bool
implies True False = False
implies _    _     = True


type VarName = Char

data Prop = Var VarName
          | Const Bool
          | And Prop Prop
          | Not Prop
          | Impl Prop Prop
          deriving Show

-- | p0 = A /\ B
p0 :: Prop
p0 = And (Var 'A') (Var 'B')

-- | p1 = A /\ not A
p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

-- | p2 = (A /\ B) => A
p2 :: Prop
p2 = Impl p0 (Var 'A')

-- | p3 = A => (A /\ B)
p3 :: Prop
p3 = Impl (Var 'A') p0

-- | p4 = (A /\ (A => B)) => B
p4 :: Prop
p4 = Impl (And (Var 'A') (Impl (Var 'A') (Var 'B'))) (Var 'B')


eval :: Map VarName Bool -> Prop -> Maybe Bool
eval m (Var c) = Data.Map.lookup c m
eval m (Const b) = Just b

-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
-- Nothing >>= f = Nothing
-- Just a >>= f  = f a

-- return :: a -> Maybe a
-- return = Just

{--
First method of writing `And`:

eval m (And l r) =
  case eval m l of
    Nothing -> Nothing
    Just x  -> case eval m r of
      Nothing -> Nothing
      Just y  -> Just (x && y)

Using `(>>=)`:

eval m (And l r) =
  eval m l >>= f where
    f :: Bool -> Maybe Bool
    f x = eval m r >>= g where
      g :: Bool -> Maybe Bool
      g y = return (x && y)

Using `(>>=)` and lambda expression:

eval m (And l r) =
  eval m l >>=
  (\(x :: Bool) -> eval m r >>=
    (\y -> return (x && y)))

Using do-notation:

eval m (And l r) = do
  x <- eval m l
  y <- eval m r
  return (x && y)
--}

-- Using do-notation and semicolons:
eval m (And l r) = do {
  x <- eval m l ;
  y <- eval m r ;
  return (x && y)
  }

-- (>>=) :: forall a b. Maybe a -> (a -> Maybe b) -> Maybe b
-- (>>=) (eval m l) f
-- (>>=) (eval m r) g

-- a == Bool
-- b == Bool

-- (>>=) :: Maybe Bool -> (Bool -> Maybe Bool) -> Maybe Bool

eval m (Not p) = do
  a <- eval m p
  return $ not a
eval m (Impl l r) = do
  a <- eval m l
  b <- eval m r
  return $ a `implies` b
-- eval m l >>= (\a -> eval m r >>= (\b -> return $ a `implies` b))





-- bind
-- (>>=) :: [a] -> (a -> [b]) -> [b]
-- [] >>= k = []
-- (x:xs) >>= k = k x <> (xs >>= k)
-- xs >>= k = [ y | x <- xs, y <- k x ]

-- return :: a -> [a]
-- return x = [x]


grid :: Int -> Int -> [(Int, Int)]

{--

List comprehension:

grid m n = [(x, y) | x <- [1..m], y <- [1..n]]

Pattern matching and recursion:

grid m n = f [1..m]
  where f [] = []
        f (x:xs) = g [1..n] ++ f xs
          where g [] = []
                g (y:ys) = [(x, y)] ++ g ys
Using `(>>=)`:

grid m n =
  [1..m] >>=
  (\x -> [1..n] >>=
    (\y -> return (x, y)))
--}

-- Using do-notation:

grid m n = do
  x <- [0..m]
  y <- [0..n]
  return (x, y)

-- List comprehension
-- square :: Int -> [(Int, Int)]
-- square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- Using `(>>=)`:
-- square n =
--  grid n n >>=
--   (\(x, y) -> if x /= y then return (x, y)
--              else [])

-- (>>=) :: forall a b. [a] -> (a -> [b]) -> [b]

-- a == (Int, Int)
-- b == (Int, Int)

-- Using do-notation:
square :: Int -> [(Int, Int)]
square n = do
  (x, y) <- grid n n
  if (x /= y) then return (x, y)
  else []

replicate :: Int -> a -> [a]
replicate n a = [1..n] >> return a
-- replicate n a = fmap (const a) [1..n]

pyths :: Integer -> [(Integer, Integer, Integer)]
pyths n = do
  x <- [1..n]
  y <- [1..n]
  z <- [1..n]
  if x^2 + y^2 == z^2 then return (x, y, z)
  else []


readAndWrite :: IO ()
readAndWrite =
  getLine >>=
  (\s -> putStrLn s)

-- readAndWrite = do
--  x <- getLine
--  putStrLn x
