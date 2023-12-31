module Day01 (run, part1, part2) where

import Data.Char (digitToInt, isAlpha)
import Data.List.Utils (replace)
import Input (readInput)

run :: IO ()
run = do
  let inputPath = "../inputs/day01/input.txt"
  input <- readInput inputPath

  putStrLn "Day 01"
  putStrLn $ "\tPart 1: " ++ show (part1 input)
  putStrLn $ "\tPart 2: " ++ show (part2 input)

part1 :: [String] -> Int
part1 lines = sum $ map (getFirstAndLast . removeAlpha) lines

part2 :: [String] -> Int
part2 lines = sum $ map (getFirstAndLast . removeAlpha . replaceAlpha) lines

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

replaceAlpha :: String -> String
replaceAlpha = replaceWithMap replacements
  where
    replacements = [("one", "o1e"), ("two", "t2o"), ("three", "t3e"), ("four", "f4r"), ("five", "f5e"), ("six", "s6x"), ("seven", "s7n"), ("eight", "e8t"), ("nine", "n9e"), ("zero", "z0o")]

replaceWithMap :: [(String, String)] -> String -> String
replaceWithMap [] n = n
replaceWithMap ((old, new) : rest) n = do
  replace old new (replaceWithMap rest n)
