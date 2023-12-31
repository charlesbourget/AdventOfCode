module Day15Test (tests) where

import Day15 (part1)
import Input (readInput)
import Test.HUnit

tests :: Test
tests = TestList [testPart1]

testPart1 :: Test
testPart1 =
  TestCase
    ( do
        input <- readInput "../inputs/day15/input.test"
        assertEqual "Day 15, Part 1" (part1 input) 1320
    )
