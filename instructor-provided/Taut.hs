module Taut where

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

type Subst = (VarName, Bool)

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find x t = head [ v | (k, v) <- t, x == k ]

impl :: Bool -> Bool -> Bool
impl True False = False
impl _    _     = True

eval :: [Subst] -> Prop -> Bool
eval t (Var v) = find v t
eval _ (Const b) = b
eval t (And p1 p2) = eval t p1 && eval t p2
eval t (Not p) = not (eval t p)
eval t (Impl p1 p2) = impl (eval t p1) (eval t p2)

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f (g x)

-- bools :: Int -> [[Bool]]
-- bools n = map (reverse . map conv . make . int2bin) range
--   where range = [0..2^n-1]
--        conv One  = True
--        conv Zero = False
--        make b = take n (b ++ repeat Zero)

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (\x -> True:x) b ++ map (False:) b
  where b = bools $ n - 1


vars :: Prop -> [VarName]
vars (Var v) = [v]
vars (Const _) = []
vars (And p1 p2) = vars p1 ++ vars p2
vars (Not p) = vars p
vars (Impl p1 p2) = vars p1 ++ vars p2

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x:(rmdups $ filter (\y -> x /= y) xs)

evalAll :: Prop -> [Bool]
evalAll p = map ((\subst -> eval subst p) . zip vs) (bools $ length vs)
  where vs = rmdups $ vars p

taut :: Prop -> Bool
taut = and . evalAll
