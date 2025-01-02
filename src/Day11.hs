module Day11 (Input, parseInput, part1, part2) where

import Data.Bifunctor (Bifunctor (second))
import Data.Map (Map)
import qualified Data.Map as Map

type Input = [String]

parseInput :: String -> Input
parseInput = words

totalStoneCountAfterBlinks :: Int -> [String] -> Int
totalStoneCountAfterBlinks totalBlinkCount = fst . foldr step (0, Map.empty)
  where
    step :: String -> (Int, Map (String, Int) Int) -> (Int, Map (String, Int) Int)
    step stone (count, cache) =
      let (count', cache') = stoneCountAfterBlinks stone totalBlinkCount cache
       in (count + count', cache')

    stoneCountAfterBlinks :: String -> Int -> Map (String, Int) Int -> (Int, Map (String, Int) Int)
    stoneCountAfterBlinks _ 0 cache = (1, cache)
    stoneCountAfterBlinks stone blinkCount cache = case Map.lookup (stone, blinkCount) cache of
      Just stoneCount -> (stoneCount, cache)
      Nothing ->
        let (stoneCount, cache') = case stone of
              "0" -> stoneCountAfterBlinks "1" (blinkCount - 1) cache
              str ->
                let len = length str
                 in if even len
                      then
                        let (l, r) = second ((\s -> if s == "" then "0" else s) . dropWhile (== '0')) $ splitAt (len `div` 2) str
                            (lcount, lcache) = stoneCountAfterBlinks l (blinkCount - 1) cache
                            (rcount, rcache) = stoneCountAfterBlinks r (blinkCount - 1) lcache
                         in (lcount + rcount, rcache)
                      else stoneCountAfterBlinks (show $ (read str :: Int) * 2024) (blinkCount - 1) cache
         in (stoneCount, Map.insert (stone, blinkCount) stoneCount cache')

part1 :: Input -> Int
part1 = totalStoneCountAfterBlinks 25

part2 :: Input -> Int
part2 = totalStoneCountAfterBlinks 75
