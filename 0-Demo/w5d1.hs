module W5D1 where

import Prelude

readAndWrite :: IO ()
readAndWrite = do
    x <- getLine
    putStrLn x

justPrintSomething :: IO ()
justPrintSomething = putStrLn "Hello dude!"


-- Monad (m :: Type -> Type)
class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b

    --useful combinator
    -- a >> b = a >>= (\_ -> b)
