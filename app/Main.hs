module Main (main) where

import qualified Day01
import qualified Day02
import qualified Day03
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  case args of
    dayArg : partArg : _ ->
      case (readMaybe dayArg :: Maybe Int, readMaybe partArg :: Maybe Int) of
        (Just day, Just part) -> case (day, part) of
          (1, 1) -> readFile "inputs/input1.txt" >>= (print . Day01.part1 . Day01.parseInput)
          (1, 2) -> readFile "inputs/input1.txt" >>= (print . Day01.part2 . Day01.parseInput)
          (2, 1) -> readFile "inputs/input2.txt" >>= (print . Day02.part1 . Day02.parseInput)
          (2, 2) -> readFile "inputs/input2.txt" >>= (print . Day02.part2 . Day02.parseInput)
          (3, 1) -> readFile "inputs/input3.txt" >>= (print . Day03.part1 . Day03.parseInput)
          (3, 2) -> readFile "inputs/input3.txt" >>= (print . Day03.part2 . Day03.parseInput)
          _ -> putStrLn "Unsupported day/part combination"
        _ -> putStrLn "day and part must be integers"
    _ -> putStrLn "Usage: stack run <day> <part>\nExample: stack run 1 2"
