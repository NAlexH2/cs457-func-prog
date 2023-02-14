Name: Alex Harris
Date: 2/11/2023
Assignment 5

> module ListPractice where
> import Test.HUnit

You should have already worked with these functions in Assignment 2 using list
comprehensions. In this file, you are asked to use either the `do`-notation or
monadic operations (`>>=` and `return`) to define them again. You can use any
functions from the standard library (such as `zip`), but do not use list
comprehensions or pattern matching.

A positive integer is prefect if it equals to the sum of all of its factors,
excluding the number itself. Using a list comprehension and the function
'factors', define a function 'perfects : Int -> [Int]' that returns the list of
all perfect numbers up to a given limit (including the limit, if it happens to
be a perfect number).

> perfects :: Int -> [Int]
> perfects a = do
>   x <- xs
>   y <- qualityCheck x
>   return y
>   where
>     xs = [1..a]
>     qualityCheck x = if x == sum(init(modCheck x)) then [x] else []
>     modCheck n =
>       do
>         x <- xs
>         if n `mod` x == 0 then return x else []

Add your own test cases to convince yourself that your implementation is
correct.

Note: For this file, test cases are not graded. Feel free to use the test cases
you had in Assignment 2.

> testPerfects :: Test
> testPerfects = "testPerfects" ~: TestList
>  [ 
>    perfects 500  ~?= [6, 28, 496],
>    perfects 5    ~?= [],
>    perfects 8200 ~?= [6, 28, 496, 8128]
>  ]


The scalar product of two list of integers xs and ys of length n is given by the
sum of the products of corresponding integers. For example, a scalar product of
[1,2,3] and [4,5,6] is 1*4 + 2*5 + 3*6 = 32. In a similar manner to 'chisqr',
show how a list comprehension can be used to define a function 'scalarProduct ::
[Int] -> [Int] -> Int' that returns the scalar product of two lists.

> scalarProduct :: [Int] -> [Int] -> Int
> scalarProduct a b = sum (prodListBuilder a b)
>  where
>    prodListBuilder :: [Int] -> [Int] -> [Int]
>    prodListBuilder j k = do
>      xys <- [zip j k]
>      y <- map (\(x,y) -> x * y) xys
>      return y


Add your own test cases to convince yourself that your implementation is
correct.

Note: For this file, test cases are not graded. Feel free to use the test cases
you had in Assignment 2.

> testScalarProduct :: Test
> testScalarProduct = "testScalarProduct" ~: TestList
>  [ 
>    scalarProduct [1,2,3] [4,5,6]   ~?= 32, 
>    scalarProduct [] []             ~?= 0,
>    scalarProduct [] [1,2,3]        ~?= 0,
>    scalarProduct [1,2,3] []        ~?= 0,
>    scalarProduct [1,2,3] [4,5]     ~?= 14,
>    scalarProduct [1,2,3] [1,2,3]   ~?= 14
>  ]
