module Main (main) where

import SortedList(testListMonoid, testSortedList,
                  testMinimum, testNumDistinct, testCount)

import MergeSort(testSortedFromList,testSortedFromList',
                 testSumOfProducts,testCrispy, testDivide, testDivideList)

import Test.HUnit ( runTestTT, Test(TestList) )

main :: IO ()
main = do
  -- _ <- runTestTT $ TestList [testSumOfProducts]
  _ <- runTestTT $ TestList [testListMonoid, testSortedList, testMinimum, testNumDistinct, testCount]
  _ <- runTestTT $ TestList [testSortedFromList, testSortedFromList', testSumOfProducts,
                             testCrispy, testDivide, testDivideList]
  return ()
