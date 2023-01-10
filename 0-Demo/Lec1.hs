module Lec1 where

-- function takes arg a and returns b
-- double takes an Int and returns an Int
double :: Int -> Int
double x = x + x

quadruple :: Int -> Int
-- x+x+x+x
quadruple x = double (double x)

a :: Int
a = b + c
    where
        b = 1
        c = 2

plus :: Int -> Int -> Int
plus a b = a + b