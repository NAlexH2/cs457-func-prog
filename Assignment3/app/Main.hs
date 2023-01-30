module Main (main) where

import Lists (testStyle, testLists)
import Folds (testFoldr)
import Test.HUnit

main :: IO ()
main = do
  _ <- runTestTT $ TestList [ testStyle, testLists, testFoldr ]
  return ()
