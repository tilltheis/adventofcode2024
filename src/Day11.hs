{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Day11 (Input, parseInput, part1, part2) where

import Data.Bifunctor (Bifunctor (second))
import Data.Either (fromRight)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Read as T

type Input = [T.Text]

parseInput :: String -> Input
parseInput = map T.pack . words

intToText :: Int -> T.Text
intToText = L.toStrict . B.toLazyText . B.decimal

textToInt :: T.Text -> Int
textToInt = fst . fromRight undefined . T.decimal

pattern (:<) :: Char -> T.Text -> T.Text
pattern x :< xs <- (T.uncons -> Just (x, xs))

pattern Empty :: T.Text
pattern Empty <- (T.uncons -> Nothing)

dropLeadingZeroes :: T.Text -> T.Text
dropLeadingZeroes s@('0' :< Empty) = s
dropLeadingZeroes ('0' :< s) = dropLeadingZeroes s
dropLeadingZeroes s = s

totalStoneCountAfterBlinks :: Int -> [T.Text] -> Int
totalStoneCountAfterBlinks totalBlinkCount = fst . foldr step (0, Map.empty)
  where
    step :: T.Text -> (Int, Map (T.Text, Int) Int) -> (Int, Map (T.Text, Int) Int)
    step stone (count, cache) =
      let (count', cache') = stoneCountAfterBlinks stone totalBlinkCount cache
       in (count + count', cache')

    stoneCountAfterBlinks :: T.Text -> Int -> Map (T.Text, Int) Int -> (Int, Map (T.Text, Int) Int)
    stoneCountAfterBlinks _ 0 cache = (1, cache)
    stoneCountAfterBlinks stone blinkCount cache = case Map.lookup (stone, blinkCount) cache of
      Just stoneCount -> (stoneCount, cache)
      Nothing ->
        let (stoneCount, cache') = case stone of
              "0" -> stoneCountAfterBlinks "1" (blinkCount - 1) cache
              str ->
                let len = T.length str
                 in if even len
                      then
                        let (l, r) = second dropLeadingZeroes $ T.splitAt (len `div` 2) str
                            (lcount, lcache) = stoneCountAfterBlinks l (blinkCount - 1) cache
                            (rcount, rcache) = stoneCountAfterBlinks r (blinkCount - 1) lcache
                         in (lcount + rcount, rcache)
                      else stoneCountAfterBlinks (intToText $ textToInt str * 2024) (blinkCount - 1) cache
         in (stoneCount, Map.insert (stone, blinkCount) stoneCount cache')

part1 :: Input -> Int
part1 = totalStoneCountAfterBlinks 25

part2 :: Input -> Int
part2 = totalStoneCountAfterBlinks 75
