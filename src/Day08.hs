{-# HLINT ignore "Use <=<" #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Day08 (Input (..), parseInput, part1, part2) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

data Input = Input (Int, Int) (Map Char [(Int, Int)])

parseInput :: String -> Input
parseInput input = Input bounds $ foldr step Map.empty antennas
  where
    grid = zipWith (\y xs -> zipWith (curry (y,)) [0 ..] xs) [0 ..] $ lines input
    bounds = (length $ head grid, length grid)
    antennas = concatMap (filter (\(_, (_, c)) -> c /= '.')) grid
    step (y, (x, c)) = Map.insertWith (++) c [(x, y)]

findAntinodes ::
  ((Int, Int) -> (Int, Int) -> [(Int, Int)]) ->
  ((Int, Int) -> (Int, Int) -> [(Int, Int)]) ->
  Input ->
  Int
findAntinodes prevAntinodes nextAntinodes (Input (width, height) antennas) =
  Set.size $ Map.foldr step Set.empty antennas
  where
    isWithinBounds (x, y) = x >= 0 && x < width && y >= 0 && y < height
    step vs acc = Set.union acc . Set.fromList . filter isWithinBounds $ go vs
    go ((x, y) : (x', y') : vs) =
      takeWhile isWithinBounds (prevAntinodes (x, y) (x', y'))
        ++ takeWhile isWithinBounds (nextAntinodes (x, y) (x', y'))
        ++ go ((x, y) : vs)
        ++ go ((x', y') : vs)
    go _ = []

part1 :: Input -> Int
part1 = findAntinodes prevAntinodes nextAntinodes
  where
    prevAntinodes (x, y) (x', y') = [(x - (x' - x), y - (y' - y))]
    nextAntinodes (x, y) (x', y') = [(x' + (x' - x), y' + (y' - y))]

part2 :: Input -> Int
part2 = findAntinodes prevAntinodes nextAntinodes
  where
    prevAntinodes (x, y) (x', y') = iterate (\(a, b) -> (a - (x' - x), b - (y' - y))) (x, y)
    nextAntinodes (x, y) (x', y') = iterate (\(a, b) -> (a + (x' - x), b + (y' - y))) (x, y)