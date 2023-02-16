module W6D1_2 where
-- MonadFriends

import Prelude hiding (Monad, return, (>>=), (>>),
                        Applicative, pure, (<*>),
                        Functor, fmap, (<$>), log)

import Data.Kind

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