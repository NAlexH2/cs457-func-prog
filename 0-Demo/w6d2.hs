module W6D2 where
-- MonadFriends

import Prelude hiding (Monad, return, (>>=), (>>),
                        Applicative, pure, (<*>),
                        Functor, fmap, (<$>), log)

import Data.Kind

class Applicative m => Monad m where
  return :: forall a. a -> m a
  (>>=) :: forall a b. m a -> (a -> m b) -> m b

class Monad where
    return :: for all a b. a -> m a
    (>>=) :: for all a b. m a -> (a -> m b) -> m b

class Functor m => Applicative m where
    pure :: forall a. a -> m a
    -- this operater is called ap
    (<*>) :: forall a b. m (a -> b) -> m a -> m b

-- fmap look familiar?? >> map :: (a -> b) -> [a] -> [b]
-- map f [] = []
-- map f (x:xs) = f x : map f xs

class Functor m where
    fmap :: forall a b. (a -> b) -> m a -> m b


instance Functor Maybe where
    fmap :: forall a b. (a -> b) -> Maybe a -> Maybe b
    fmap f Nothing = Nothing
    fmap f (Just a) = Just (f a)

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Functor Tree where
    fmap :: forall a b. (a -> b) -> m a -> m b
    fmap f (Leaf a)     = Leaf (f a)
    fmap f (Node l r)   = Node (fmap f l) (fmap f r)

data Foo a = Bar Int deriving Show

instance Functor Foo where
    fmap :: forall a b. (a -> b) -> Foo a -> Foo b
    fmap f (Bar x) = Bar x

class Functor m => Applicative m where
  pure :: forall a. a -> m a
  -- ap
  (<*>) :: forall a b. m (a -> b) -> m a -> m b

(<$>) :: Functor m => (a -> b) -> m a -> m b
(<$>) = fmap

fmap2 :: Applicative m => (a -> b -> c) -> m a -> m b -> m c
fmap2 f a b = pure f <*> a <*> b

fmap3 :: Applicative m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
fmap3 f a b c = pure f <*> a <*> b <*> c

 
instance Applicative Maybe where
  pure :: forall a. a -> m a
  pure a = Just a
  -- ap
  (<*>) :: forall a b. Maybe (a -> b) -> Maybe a -> Maybe b
  -- Nothing <*> Nothing = Nothing
  -- Nothing <*> (Just a) = Nothing -- No way to get Maybe b, so we do nothing
  -- (Just f) <*> (Nothing) = Nothing -- Same here
  (Just f) <*> (Just a) = Just (f a)
  -- wildcard case - an "otherwise" to the pattern matching since only one
  -- instance does actual work
  _ <*> _ = Nothing

plus3 :: Int -> Int -> Int -> Int
plus3 x y z = x + y + z

-- pure plus3 <*> Just 1 <*> Just 2 <*> Just 3 = Just 6
-- pure plus3 <*> Just 1 <*> Nothing <*> Just 3 = Nothing


instance Applicative [] where
    pure :: forall a. a -> [a]
    pure x = [x]
    (<*>):: forall a b. [a -> b] -> [a] -> [b]
    fs <*> xs = [ f x | f <- fs, x <- xs]

newtype ZipList a = MkZipList [a]

instance Applicative ZipList where
  pure :: forall a. a -> ZipList a
  pure x = MkZipList [x]
  (<*>):: forall a b. ZipList (a -> b) -> ZipList a -> ZipList b
  -- (MkZipList []) (<*>) (MkZipList [])          = MkZipList []
  -- (MkZipList []) (<*>) (MkZipList (x:xs))      = MkZipList []
  -- (MkZipList (f:fs)) (<*>) (MkZipList [])      = MkZipList []
  (MkZipList (f:fs)) <*> (MkZipList (x:xs))  = 
    case (MkZipList fs <*> MkZipList xs) of 
      MkZipList ys -> MkZipList (f x : ys)
  -- the otherwise pattern matching again
  (MkZipList _) <*> (MkZipList _)              = MkZipList []

instance Functor ZipList where
  fmap f (MkZipList xs) = MkZipList (fmap f xs)

takeZ :: Int -> ZipList a -> ZipList a
takeZ n (MkZipList xs) = MkZipList (take n xs)