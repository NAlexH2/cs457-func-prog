> module Classes where
> import Prelude hiding (Foldable, fold, foldr, foldMap, foldl, Monoid, mappend, mempty, mconcat)
> import Data.Kind

Eq
=============

class Eq a where
 (==), (/=) :: a -> a -> Bool

instance Eq Bool where
  True ==  True  = True
  False == False = True
  _ == _ = False
  True /= False = True
  False /= True = True
  _ /= _ = False


f []     = v
f (x:xs) = h x (f xs)
==
foldr v h xs

stringEq :: String -> String -> Bool
stringEq = foldr h v
  where v [] = True
        v (y:ys) = False
        h a b [] = False
        h a b (y:ys) = a == y && b ys

instance Eq String where
  x == y = stringEq x y
  x /= y = not (stringEq x y)

h x (stringEq xs)
= (\a -> (\b -> (\ys -> case ys of
    [] -> False
    (y:ys) -> a == y && b ys))) x (stringEq xs)
= (\b -> (\ys -> case ys of
    [] -> False
    (y:ys) -> x == y && b ys)) (stringEq xs)
= \ys -> case ys of
    [] -> False
    (y:ys) -> x == y && stringEq xs ys




stringEq [] [] = True
stringEq [] (y:ys) = False

stringEq (x:xs) (y:ys) = x == y && stringEq xs ys
stringEq (x:xs) [] = False




Ord
==============


class Eq a => Ord (a :: Type) where
  (<), (<=), (>=), (>) :: a -> a -> Bool
  min, max :: a -> a -> a
  min x y | x <= y = x
          | otherwise = y
  max x y | x >= y = x
          | otherwise = y

data Bool = True | False
      deriving (Eq, Ord, Show, Read)

deriving Eq, Ord, Show, Read


data Shape = Circle Float | Rect Float Float
     deriving (Eq, Ord)

> class Semigroup (a :: Type) where
>     (<>) :: a -> a -> a



Monoids
===========

> class Monoid (a :: Type) where
>       mempty :: a
>       mappend :: a -> a -> a

mempty <> x = x

0 + x = x
1 * x = x
[] ++ xs = xs
Nothing # xs = xs

> instance Monoid ([a] :: Type) where
>       mappend x y = x ++ y
>       mempty = []

data Maybe a = Nothing | Just a

> instance Monoid (a :: Type) => Monoid (Maybe a) where
>       mappend Nothing y = y
>       mappend x Nothing = x
>       mappend (Just x) (Just y) = Just (mappend x y)
>       mempty = Nothing

> mconcat :: Monoid a => [a] -> a
> mconcat [] = mempty
> mconcat (x:xs) = x `mappend` mconcat xs


mconcat [Just [1], Just [2], Nothing]
==> mappend (Just [1]) (mconcat [Just [2], Nothing])
==> mappend (Just [1]) (mappend (Just [2]) (mconcat [Nothing]))
==> mappend (Just [1]) (mappend (Just [2]) (mappend Nothing Nothing)
==> mappend (Just [1]) (mappend (Just [2]) Nothing)
==> mappend (Just [1]) (Just [2])
==> Just (mappend [1] [2])
==> Just ([1] ++ [2])
==> Just [1,2]

type
data
newtype

> newtype Sum = Sum { getSum :: Int }
>   deriving Show
>
> newtype Product = Product { getProduct :: Int }
>   deriving Show


> instance Monoid Sum where
>   mappend (Sum x) (Sum y) = Sum (x + y)
>   mempty = Sum 0

> instance Monoid Product where
>   mappend (Product x) (Product y) = Product (x * y)
>   mempty = Product 1

> sum :: [Int] -> Int
> sum = getSum . mconcat . map Sum

> product :: [Int] -> Int
> product = getProduct . mconcat . map Product

Examples:
- Lists
- Maybe
- Int
- Sum
- Product



Foldables
==================

fold :: Monoid a => [a] -> a
fold [] = mempty
fold (x:xs) = x `mappend` fold xs


> data Tree a = Leaf a | Node (Tree a) (Tree a)
>   deriving Show

> foldTree :: Monoid a => Tree a -> a
> foldTree (Leaf a) = a
> foldTree (Node l r) = foldTree l `mappend` foldTree r

> mapTree :: (a -> b) -> Tree a -> Tree b
> mapTree f (Leaf a) = Leaf (f a)
> mapTree f (Node l r) = Node (mapTree f l) (mapTree f r)




> class Foldable (t :: Type -> Type) where
>       fold :: Monoid a => t a -> a
>       foldMap :: Monoid b => (a -> b) -> t a -> b
>       foldr :: (a -> b -> b) -> b -> t a -> b
>       foldl :: (a -> b -> a) -> a -> t b -> a

> foldrTree :: (a -> b -> b) -> b -> Tree a -> b
> foldrTree f b (Leaf x) = f x b
> foldrTree f b (Node l r) = foldrTree f (foldrTree f b r) l

> foldlTree :: (a -> b -> a) -> a -> Tree b -> a
> foldlTree f b (Leaf x) = f b x
> foldlTree f b (Node l r) = foldlTree f (foldlTree f b l) r

> instance Foldable Tree where
>   fold = foldTree
>   foldMap f = fold . mapTree f
>   foldr = foldrTree
>   foldl = foldlTree

Examples:
- Lists
- Trees
