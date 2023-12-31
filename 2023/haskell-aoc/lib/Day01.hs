module Day01 (run, part1) where

import Data.Char (digitToInt, isAlpha)
import Input (readInput)

run :: IO ()
run = do
  let inputPath = "../inputs/day01/input.txt"
  input <- readInput inputPath

  putStrLn "Day 01"
  putStrLn $ "\tPart 1: " ++ show (part1 input)

part1 :: [String] -> Int
part1 lines = sum $ map (getFirstAndLast . removeAlpha) lines

removeAlpha :: String -> String
removeAlpha = filter isNotAlpha
  where
    isNotAlpha c = not $ isAlpha c

getFirstAndLast :: [Char] -> Int
getFirstAndLast [] = 0
getFirstAndLast line = read [firstChar, lastChar]
  where
    firstChar = head line
    lastChar = last line
