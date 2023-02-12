module Main (main) where

import MaybePractice
import ListPractice
import GenericMonadFunctions
import Test.HUnit

main :: IO ()
main = do
  _ <- runTestTT $ TestList [testParseWeather,
                             testPerfects, testScalarProduct,
                             testMapM, testFoldM, testSequence,
                             testKleisli, testJoin, testLiftM, testLiftM2]
  return ()
