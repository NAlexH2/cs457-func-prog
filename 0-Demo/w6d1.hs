module W6D1 where

import Prelude hiding (Monad, return, (>>=), (>>),
                        Applicative, pure, (<*>),
                        Functor, fmap, (<$>), log)

import Data.Kind


class Monad where
    return :: for all a b. a -> m a
    (>>=) :: for all a b. m a -> (a -> m b) -> m b

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

tree = Node
    (Node (Leaf 'a') (Leaf 'b'))
    (Leaf 'c')
-- Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)

-- the int in the middle is the current val, where n+1 on 23 is the next counter
relabel :: Tree Char -> Int -> (Tree Int, Int) -- (Tree Int, Int) allows us to record number of times n was used
-- relabel :: Tree Char -> (Int -> (Tree Int, Int))
-- relabel :: Tree Char -> ST (Tree Int)
-- ST a = Int -> (a, Int)
relabel (Leaf c) n = (Leaf n, n+1)
relabel (Node l r) = case relabel l n of
                (l', m) -> case relabel r m of
                    (r', p) -> (Node l' r', p)
type State = Int
newtype ST a = ST (State -> (a, State))

instance Monad (ST s) where
    -- 'a' in this context is a ST (Tree Int)
    return :: forall a. a -> ST a
    return x = ST $ \s -> (x, s)


    (>>=) :: forall a b. ST a -> (a -> ST b) -> ST b
    -- The verbose version
    -- (>>=) :: forall a b. (State -> (a, State)) -> (a -> State -> (b, State)) -> State -> (b, State)
    (ST m) >>= k = ST $ \s -> case m s of -- function k, the current state, the new state
                    (m', s') -> case k m' of
                        (ST k') -> k' s'


{--
-- (>>=) :: forall a b. (State -> (a, State)) -> (a -> State -> (b, State)) -> State -> (b, State)
Yao's technique to make sense of stuff of the above
--------------------------------------
ST m : ST a ==> m : State -> (a, State)
k : a -> ST b
s : State
a : a
s' : State
k' : State -> (b, State)
======================================
(b, State)
--}
    -- (ST m) >>= k = ST (\s -> case m s of
    --                         (a, s') -> case k a of
    --                             (ST k') -> k' s')
    
    -- inc :: ST State
    -- inc = ST (\s -> (s, s+1))
    get :: ST s s
    get = ST $ \s -> (s,s)

    put :: s -> ST s ()
    put s = ST $ \_ -> ((), s)

    runST :: ST s a -> State -> a
    runST (ST s) n = fst $ s n -- take the first element of the tuple of... this

    inc :: ST Int Int
    inc = get >>= (\c -> put (c+1) >>= (\_ -> return c))
    {--
    inc = do
        c <- get
        _ <- put (c+1)
        return c
     --}


    -- relabelM :: Tree Char -> Int -> (Tree Int, Int)
    relabelM :: Tree Char -> ST Int (Tree Int)
    relabelM (Leaf c) = inc >>= (\n -> return (Leaf n))
    relabelM (Node l r) = relabelM l >>= 
        (\l' -> relaelM r >>= (\r' -> return (Node l' r')))
    
    -- relabelM (Leaf c) = do
    --     s <- inc
    --     return (Leaf s)
    -- relabelM (Node l r) = do
    --     l' <- relabelM l
    --     r' <- relabelM r
    --     return (Node l' r')
