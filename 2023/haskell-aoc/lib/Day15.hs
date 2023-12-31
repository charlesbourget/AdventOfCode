module Day15 where

import Data.Char (ord)
import Data.List (foldl')
import Data.List.Split (splitOn)
import Input (readInput)

run :: IO ()
run = do
  let inputPath = "../inputs/day15/input.txt"
  input <- readInput inputPath

  putStrLn "Day 15"
  putStrLn $ "\tPart 1: " ++ show (part1 input)

part1 :: [String] -> Int
part1 input = sum $ map hash (splitOn "," (head input))

hash :: String -> Int
hash = foldl' (\acc c -> ((acc + ord c) * 17) `mod` 256) 0
