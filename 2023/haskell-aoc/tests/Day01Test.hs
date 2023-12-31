module Day01Test (tests) where

import Day01 (part1, part2)
import Input (readInput)
import Test.HUnit

tests :: Test
tests = TestList [testPart1]

testPart1 :: Test
testPart1 =
  TestCase
    ( do
        input <- readInput "../inputs/day01/input.test"
        assertEqual "Day 01, Part 1" (part1 input) 142
    )

testPart2 :: Test
testPart2 =
  TestCase
    ( do
        input <- readInput "../inputs/day01/input.test"
        assertEqual "Day 01, Part 2" (part2 input) 281
    )
