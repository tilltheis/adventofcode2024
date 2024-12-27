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
          (1, 1) -> readFile "inputs/input1.txt" >>= (printTimedResult Day01.part1 . Day01.parseInput)
          (1, 2) -> readFile "inputs/input1.txt" >>= (printTimedResult Day01.part2 . Day01.parseInput)
          (2, 1) -> readFile "inputs/input2.txt" >>= (printTimedResult Day02.part1 . Day02.parseInput)
          (2, 2) -> readFile "inputs/input2.txt" >>= (printTimedResult Day02.part2 . Day02.parseInput)
          (3, 1) -> readFile "inputs/input3.txt" >>= (printTimedResult Day03.part1 . Day03.parseInput)
          (3, 2) -> readFile "inputs/input3.txt" >>= (printTimedResult Day03.part2 . Day03.parseInput)
          (4, 1) -> readFile "inputs/input4.txt" >>= (printTimedResult Day04.part1 . Day04.parseInput)
          (4, 2) -> readFile "inputs/input4.txt" >>= (printTimedResult Day04.part2 . Day04.parseInput)
          (5, 1) -> readFile "inputs/input5.txt" >>= (printTimedResult Day05.part1 . Day05.parseInput)
          (5, 2) -> readFile "inputs/input5.txt" >>= (printTimedResult Day05.part2 . Day05.parseInput)
          (6, 1) -> readFile "inputs/input6.txt" >>= (printTimedResult Day06.part1 . Day06.parseInput)
          (6, 2) -> readFile "inputs/input6.txt" >>= (printTimedResult Day06.part2 . Day06.parseInput)
          (7, 1) -> readFile "inputs/input7.txt" >>= (printTimedResult Day07.part1 . Day07.parseInput)
          (7, 2) -> readFile "inputs/input7.txt" >>= (printTimedResult Day07.part2 . Day07.parseInput)
          (8, 1) -> readFile "inputs/input8.txt" >>= (printTimedResult Day08.part1 . Day08.parseInput)
          (8, 2) -> readFile "inputs/input8.txt" >>= (printTimedResult Day08.part2 . Day08.parseInput)
          _ -> putStrLn "Unsupported day/part combination"
        _ -> putStrLn "day and part must be integers"
    _ -> putStrLn "Usage: stack run <day> <part>\nExample: stack run 1 2"
