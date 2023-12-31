module Main where

import qualified Day01Test
import qualified Day15Test
import Test.HUnit

tests :: Test
tests =
  TestList
    [ Day01Test.tests,
      Day15Test.tests
    ]

main :: IO ()
main = runTestTTAndExit tests
