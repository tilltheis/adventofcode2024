module Day07 (Input, parseInput, part1, part2) where

import Data.Bifunctor (Bifunctor (bimap))
import Util (splitOn, splitOn2)

type Input = [(Int, [Int])]

parseInput :: String -> Input
parseInput = map parseLine . lines
  where
    parseLine = bimap read (map read . splitOn ' ') . splitOn2 ':'

isSolvable :: [Int -> Int -> Int] -> Int -> [Int] -> Bool
isSolvable _ _ [] = False
isSolvable _ result [x] = x == result
isSolvable ops result (x : y : ys) = any (\op -> isSolvable ops result (x `op` y : ys)) ops

part1 :: Input -> Int
part1 = sum . map fst . filter (uncurry (isSolvable [(+), (*)]))

concatInts :: Int -> Int -> Int
concatInts a b
  | b < 10 = a * 10 + b
  | b < 100 = a * 100 + b
  | b < 1000 = a * 1000 + b
  | otherwise = undefined

part2 :: Input -> Int
part2 = sum . map fst . filter (uncurry (isSolvable [(+), (*), concatInts]))
