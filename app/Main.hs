module Main (main) where

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.Read (readMaybe)

printTimedResult :: (Show b) => (a -> b) -> a -> IO ()
printTimedResult f x = do
  start <- getCurrentTime
  let result = f x
  putStr $ show result -- force evaluation
  end <- getCurrentTime
  let diff = diffUTCTime end start
  printf "\r%s (took %s)\n" (show result) (show diff)

main :: IO ()
main = do
  args <- getArgs
  case args of
    dayArg : partArg : _ ->
      case (readMaybe dayArg :: Maybe Int, readMaybe partArg :: Maybe Int) of
        (Just day, Just part) -> case (day, part) of
          (01, 1) -> readFile "inputs/input01.txt" >>= (printTimedResult Day01.part1 . Day01.parseInput)
          (01, 2) -> readFile "inputs/input01.txt" >>= (printTimedResult Day01.part2 . Day01.parseInput)
          (02, 1) -> readFile "inputs/input02.txt" >>= (printTimedResult Day02.part1 . Day02.parseInput)
          (02, 2) -> readFile "inputs/input02.txt" >>= (printTimedResult Day02.part2 . Day02.parseInput)
          (03, 1) -> readFile "inputs/input03.txt" >>= (printTimedResult Day03.part1 . Day03.parseInput)
          (03, 2) -> readFile "inputs/input03.txt" >>= (printTimedResult Day03.part2 . Day03.parseInput)
          (04, 1) -> readFile "inputs/input04.txt" >>= (printTimedResult Day04.part1 . Day04.parseInput)
          (04, 2) -> readFile "inputs/input04.txt" >>= (printTimedResult Day04.part2 . Day04.parseInput)
          (05, 1) -> readFile "inputs/input05.txt" >>= (printTimedResult Day05.part1 . Day05.parseInput)
          (05, 2) -> readFile "inputs/input05.txt" >>= (printTimedResult Day05.part2 . Day05.parseInput)
          (06, 1) -> readFile "inputs/input06.txt" >>= (printTimedResult Day06.part1 . Day06.parseInput)
          (06, 2) -> readFile "inputs/input06.txt" >>= (printTimedResult Day06.part2 . Day06.parseInput)
          (07, 1) -> readFile "inputs/input07.txt" >>= (printTimedResult Day07.part1 . Day07.parseInput)
          (07, 2) -> readFile "inputs/input07.txt" >>= (printTimedResult Day07.part2 . Day07.parseInput)
          (08, 1) -> readFile "inputs/input08.txt" >>= (printTimedResult Day08.part1 . Day08.parseInput)
          (08, 2) -> readFile "inputs/input08.txt" >>= (printTimedResult Day08.part2 . Day08.parseInput)
          (09, 1) -> readFile "inputs/input09.txt" >>= (printTimedResult Day09.part1 . Day09.parseInput)
          (09, 2) -> readFile "inputs/input09.txt" >>= (printTimedResult Day09.part2 . Day09.parseInput)
          (10, 1) -> readFile "inputs/input10.txt" >>= (printTimedResult Day10.part1 . Day10.parseInput)
          (10, 2) -> readFile "inputs/input10.txt" >>= (printTimedResult Day10.part2 . Day10.parseInput)
          (11, 1) -> readFile "inputs/input11.txt" >>= (printTimedResult Day11.part1 . Day11.parseInput)
          (11, 2) -> readFile "inputs/input11.txt" >>= (printTimedResult Day11.part2 . Day11.parseInput)
          _ -> putStrLn "Unsupported day/part combination"
        _ -> putStrLn "day and part must be integers"
    _ -> putStrLn "Usage: stack run <day> <part>\nExample: stack run 1 2"
