module W6D2_2 where
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

class Functor m where
    fmap :: forall a b. (a -> b) -> m a -> m b

-- fmap look familiar?? >> map :: (a -> b) -> [a] -> [b]
-- map f [] = []
-- map f (x:xs) = f x : map f xs
