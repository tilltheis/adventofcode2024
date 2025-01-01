module Day10 (Input, parseInput, part1, part2) where

import Data.Char (ord)
import Data.Monoid (Sum (Sum, getSum))
import qualified Data.Set as Set

type Input = ([(Int, Int)], [[Int]])

parseDigit :: Char -> Int
parseDigit = subtract (ord '0') . ord

parseInput :: String -> Input
parseInput input = (starts, grid)
  where
    grid = map (map parseDigit) . lines $ input
    starts = [(x, y) | (y, row) <- zip [0 ..] grid, (x, col) <- zip [0 ..] row, col == 0]

findPaths :: (Monoid a) => ((Int, Int) -> a) -> [[Int]] -> (Int, Int) -> a
findPaths mk grid = findPaths' 0
  where
    findPaths' height (x, y) = mconcat $ zipWith go neighborCoords neighborHeights
      where
        neighborCoords = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
        neighborHeights = map get neighborCoords
        get (x', y') =
          if y' >= 0 && y' < length grid && x' >= 0 && x' < length (grid !! y')
            then Just $ grid !! y' !! x'
            else Nothing
        go coord (Just height')
          | height' == height + 1 =
              if height' == 9 then mk coord else findPaths' height' coord
        go _ _ = mempty

part1 :: Input -> Int
part1 (starts, grid) = sum $ map (Set.size . f) starts
  where
    f = findPaths Set.singleton grid

part2 :: Input -> Int
part2 (starts, grid) = getSum . mconcat $ map f starts
  where
    f = findPaths (const $ Sum 1) grid
