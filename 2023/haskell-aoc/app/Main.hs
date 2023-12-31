module Main where

import qualified Day01
import qualified Day15
import Options.Applicative

newtype Aoc = Aoc
  {day :: Int}

aoc :: Parser Aoc
aoc =
  Aoc
    <$> option
      auto
      ( long "day"
          <> short 'd'
          <> metavar "DAY"
          <> help "Which day to run"
      )

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (aoc <**> helper)
        ( fullDesc
            <> progDesc "Advent of Code"
            <> header "Advent of Code"
        )

run :: Aoc -> IO ()
run (Aoc day) = runDay day

runDay :: Int -> IO ()
runDay 1 = Day01.run
runDay 15 = Day15.run
runDay _ = error "Invalid day"
