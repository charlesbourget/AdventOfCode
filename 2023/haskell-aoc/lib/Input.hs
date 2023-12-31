module Input (readInput) where

import Data.List.Split (splitOn)

readInput :: FilePath -> IO [String]
readInput inputPath = do
  lines <- readFile inputPath
  return $ splitOn "\n" lines
