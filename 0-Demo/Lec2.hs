module Functions where

    import GHC.Float.RealFracMethods

-- test if the number is even
    even :: Integral a => a -> Bool
    even x = x `mod` 2 == 0

-- split a list at a position
    splitAt :: Int -> [a] -> ([a], [a])
    splitAt n xs = (take n xs, drop n xs)

-- reciprocal
    recip :: Fractional a => a -> a
    recip x = 1 / x

-- Conditional expressions and guarded expressions
-- absolute value
    -- abs :: Int -> Int
    -- abs x = if x >= 0 then x else -x

    -- Another way 
    abs :: Int -> Int
    abs x | x >= 0  = x 
          | otherwise = -x

-- reutrn 1 if the number is positive, -1 if negative, and 0 if 0
    -- signum :: Int -> Int
    -- signum x = if x > 0 then 1 else
    --     if x < 0 then -1 else 0

    -- Another way
    signum :: Int -> Int
    signum x | x > 0    = 1
             | x < 0 = -1 
             | otherwise = 0

--Pattern Matching
-- Boolean not
    not :: Bool -> Bool
    not True = False
    not False = True

-- Boolean and
    (&&) :: Bool -> Bool -> Bool
    -- (&&) True && True == True
    --      _ && _ = False
    -- Another way
    x && y | x == y = x
           | otherwise = False

-- first element in the tuple
    fst :: (a, b) -> a
    fst (a, _) = a -- the _ here states we don't care about the other value

-- second element in the tuple
    snd :: (a, b) -> b
    snd (_, a) = a

-- convert something that is either an Int or Float to Int (rounded down)
-- Hint: floorFloatInt
    convert :: Either Int Float -> Int
    convert (Left x) = x
    convert (Right x) = floorFloatInt x

-- test if the first element is 'a'
    test :: [Char] -> Bool
    test ['a', _ ] = True
    test _         = False


-- Lambda expressions
-- add two Ints together
    add :: Int -> (Int -> Int)
    add x = \y -> x + y -- \ before anything is a lambda definitnion
                        -- \x, \y, \d, \b are examples that should work.

-- always return the first argument
    const :: a -> b -> a
    const a = \_ -> a -- another way to define a lamba expression

-- the first n odd integers
    odds :: Int -> [Int]
    -- odds n = map f [0..n-1]
    --     where f x = 2 * x + 1
        -- returns the firs n odd numbers, n = 3 = [1, 3, 5]
    odds n = map (\x -> 2 * x + 1) [0..n-1]