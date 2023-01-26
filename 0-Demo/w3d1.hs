module W3D1 where
import Prelude hiding (reverse, zip, drop, odd, even, product, init)

{--
Steps on defining recursive functions:

1. Define the type
2. Enumerate the cases
3. Define the simple cases
4. Define the other cases
5. Generalize and Simplify
--}

-- Type Declaration
-------------------

-- String
type String = [Char]

-- Cartesian coordinate
type Pos = (Int, Int)

-- Transformation on a Cartesian coordiante
type Trans = Pos -> Pos

-- Type Parameters
type Pair a = (a, a)

-- Association "map"
type Assoc k v = [(k, v)]

-- Find
find :: Eq k => k -> Assoc k v -> v
-- want to find all v' where k' == k in our list of tuples
find k t = head [ v' | (k', v') <- t, k' == k]


-- Data Declaration
-------------------

-- Boolean
-- data Bool = True | False

-- Move
data Move = North | South | West | East

move :: Move -> Pos -> Pos
move North (x, y) = (x, y + 1)
move South (x, y) = (x, y -1)
move East (x, y) = (x + 1, y)
move West (x, y) = (x - 1, y)

rev :: Move -> Move
rev North = South
rev South = North
rev East = West
rev West = East

-- A shape that is either a circle or a rectangle
data Shape = Circle Float | Rectangle Float Float

square :: Float -> Shape
square x = Rectangle x x

area :: Shape -> Float
area (Circle r) = 3.14 * r^2
area (Rectangle x y) = x * y

-- Maybe type
-- data Maybe a = Nothing | Just a

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
-- safeDiv x y = Just (x `div` y)
safeDiv x y = Just $ x `div` y

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:_) = Just x

f :: Int -> Int -> Maybe Int
f x y = case (safeDiv x y) of
            Nothing -> Nothing
            Just z -> safeDiv 1 z

-- New Type Declaration
-----------------------

-- Natural Numbers
-- newtype Nat = N Int
-- not the correct way because it could still be set to a negative value


-- Recursive Types
------------------

-- The inductive definition of natural numbers

-- Base case: zero is a natural number            (Successor)
-- Inductive step: if n is a natural number, then (Succ n) is also a natural
-- number

-- data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n
-- Example of being used:
-- nat2int Zero = 0
-- nat2int (Succ (Succ Zero)) = 2

-- returns sequence that results in the natural number
int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n   | n > 0 = Succ $ int2nat (n-1)
            | otherwise = Zero


add :: Nat -> Nat -> Nat
add     Zero n   = n
add (Succ m) n   = Succ $ add m n


-- Lists
-- data List a = Nil | Cons a (List a)

len :: [a] -> Int
len [] = 0
len (_ : xs) = 1 + len xs


-- Binary Tree
data Tree a = Leaf a | Node (Tree a) a (Tree a)
    deriving Show

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
         (Node (Leaf 6) 7 (Leaf 9))

-- occurs :: Eq a => a -> Tree a -> Bool
-- occurs x (Leaf y)     = x == y
-- occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) | x == y    = True
                      | x < y     = occurs x l 
                      | otherwise = occurs x r

-- reverse
-- reverse :: [a] -> [a]
-- reverse [] = []
-- reverse (x:xs) = reverse xs ++ [x]

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y    = x : y : ys
                | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x $ isort xs



-- Recursion on multiple arguments
----------------------------------
zip :: [a] -> [b] -> [(a, b)]
zip (x:xs) (y:ys) = (x, y) : zip xs ys
zip _ _ = []

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (x:xs) | n > 0 = drop (n-1) xs
              | otherwise = []

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smallers ++ [x] ++ largers
        where smallers = [y | y <- xs, y <= x]
              largers  = [y | y <- xs, y > x]

-- Mutual Recursion
----------------------------------

data Nat = Zero | Succ Nat
even :: Nat -> Bool
even Zero = True
even (Succ n) = odd n

odd :: Nat -> Bool
odd Zero = False
odd (Succ n) = even n

{--
Steps on defining recursive functions:

1. Define the type
2. Enumerate the cases
3. Define the simple cases
4. Define the other cases
5. Generalize and Simplify
--}

product :: Num a => [a] -> a
product = foldr (*) 1
-- product []      = 1
-- product (x:xs)  = x * product xs

-- All elements from even positions:

evens :: [a] -> [a]
evens xs = evens xs
      where
      evens' [] = []
      evens' (x:xs) = x : odds' xs
      odds' [] = []
      odds' (_:xs) = evens' xs

-- evens []     = []
-- evens (x:xs) = x : odds xs

-- odds :: [a] -> [a]
-- odds []     = []
-- odds (_:xs) = evens xs


-- Exercisese on recursion
----------------------------------

{--
Steps on defining recursive functions:

1. Define the type
2. Enumerate the cases
3. Define the simple cases
4. Define the other cases
5. Generalize and Simplify
--}

init :: [a] -> [a]
init [] = []
init [_] = [] -- one item, drop it, return empty list
init (x:xs) = x : init xs