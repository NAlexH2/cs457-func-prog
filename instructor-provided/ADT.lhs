> module ADT where

Type Declaration
=================

- String

> type String = [Char]

- Cartesian coordinate

> type Pos = (Int, Int)

- Transformation on a Cartesian coordinate

> type Trans = Pos -> Pos

- Type parameter

> type Pair a = (a, a)

- Association "map"

> type Assoc k v = [(k, v)]

> find :: Eq k => k -> Assoc k v -> v
> find k t = head [ v' | (k', v') <- t, k' == k ]


Data Declaration
=================

- Boolean

data Bool = True | False

- Move

> data Move = North | South | West | East

> move :: Move -> Pos -> Pos
> move North (x, y) = (x, y + 1)
> move South (x, y) = (x, y - 1)
> move West  (x, y) = (x - 1, y)
> move East  (x, y) = (x + 1, y)

> rev :: Move -> Move
> rev North = South
> rev South = North
> rev West  = East
> rev East  = West

- A shape that is either a circle or a rectangle

> data Shape = Circle Float | Rect Float Float

> square :: Float -> Shape
> square x = Rect x x

> area :: Shape -> Float
> area (Circle r) = 3.14 * r^2
> area (Rect x y) = x * y

- Maybe type

data Maybe a = Nothing | Just a

> safeDiv :: Int -> Int -> Maybe Int
> safeDiv _ 0 = Nothing
> safeDiv x y = Just $ x `div` y

> safehead :: [a] -> Maybe a
> safehead [] = Nothing
> safehead (x:_) = Just x

> f :: Int -> Int -> Maybe Int
> f x y = case (safeDiv x y) of
>           Nothing -> Nothing
>           Just z  -> safeDiv 1 z

New Type Declaration
=======================

- Natural numbers

type Nat = Int
data Nat = N Int
newtype Nat = N Int

Recursive Types
=====================

- The inductive definition of natural numbers

Base case: zero is a natural number
Inductive step: if n is a natural number, then (Succ n) is also a natural number


> data Nat = Zero | Succ Nat deriving Show

> nat2int :: Nat -> Int
> nat2int Zero     = 0
> nat2int (Succ n) = 1 + nat2int n

> int2nat :: Int -> Nat
> int2nat 0 = Zero
> int2nat n | n > 0 = Succ $ int2nat (n - 1)
>           | otherwise = Zero

> add :: Nat -> Nat -> Nat
> add Zero     n = n
> add (Succ m) n = Succ $ add m n


- Lists

data List a = Nil | Cons a (List a)

> len :: [a] -> Int
> len []     = 0
> len (_:xs) = 1 + len xs

- Binary trees

> data Tree a = Leaf a | Node (Tree a) a (Tree a)
>  deriving Show

> tree :: Tree Int
> tree = Node (Node (Leaf 1) 3 (Leaf 4)) 5
>             (Node (Leaf 6) 7 (Leaf 9))

> flatten :: Tree a -> [a]
> flatten (Leaf x)     = [x]
> flatten (Node l x r) = flatten l ++ [x] ++ flatten r

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

> occurs :: Ord a => a -> Tree a -> Bool
> occurs x (Leaf y)     = x == y
> occurs x (Node l y r) | x == y    = True
>                       | x < y     = occurs x l
>                       | otherwise = occurs x r
