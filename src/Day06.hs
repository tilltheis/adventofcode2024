module Day06 (Input (..), parseInput, part1, part2) where

import Data.Foldable (foldr')
import Data.Maybe (fromJust, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set

data Input = Input
  { dimensions :: (Int, Int),
    obstructions :: Set (Int, Int),
    position :: (Int, Int),
    direction :: (Int, Int)
  }
  deriving (Show, Eq)

parseInput :: String -> Input
parseInput string =
  Input
    { dimensions = (gridWidth, gridHeight),
      obstructions = obs,
      position = fromJust start,
      direction = (0, -1)
    }
  where
    grid = lines string
    gridWidth = length (head grid)
    gridHeight = length grid
    stringWidth = gridWidth + 1
    (obs, start) = foldr' step (Set.empty, Nothing) (zip string [0 ..])
      where
        step (char, i) (obs', st) = case char of
          '#' -> (Set.insert (i `mod` stringWidth, i `div` stringWidth) obs', st)
          '^' -> (obs', Just (i `mod` stringWidth, i `div` stringWidth))
          _ -> (obs', st)

findPath :: Set ((Int, Int), (Int, Int)) -> Input -> Maybe [((Int, Int), (Int, Int))]
findPath
  visited
  Input
    { dimensions = (width, height),
      obstructions = obs,
      position = startPos,
      direction = startDir
    } = simulate startPos startDir visited
    where
      turnRight (0, -1) = (1, 0)
      turnRight (1, 0) = (0, 1)
      turnRight (0, 1) = (-1, 0)
      turnRight (-1, 0) = (0, -1)
      turnRight _ = undefined
      isOutOfBounds (x, y) = x < 0 || y < 0 || x >= width || y >= height
      simulate pos@(x, y) dir@(dx, dy) vis
        | isOutOfBounds pos = Just []
        | (pos, dir) `Set.member` vis = Nothing
        | otherwise =
            let pos' = (x + dx, y + dy)
                vis' = Set.insert (pos, dir) vis
             in if pos' `Set.member` obs
                  then ((pos, dir) :) <$> simulate pos (turnRight dir) vis'
                  else ((pos, dir) :) <$> simulate pos' dir vis'

part1 :: Input -> Int
part1 = Set.size . Set.fromList . map fst . fromJust . findPath Set.empty

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

part2 :: Input -> Int
part2 input = count isNothing . go Set.empty Set.empty . fromJust $ findPath Set.empty input
  where
    go visitedPath visitedPositions (pathEntry@(pos, dir) : pathEntry'@(pos', _) : pathEntries)
      | pos' `Set.member` visitedPositions
          || pos' == position input =
          go visitedPath visitedPositions (pathEntry' : pathEntries)
      | otherwise =
          findPath
            visitedPath
            ( input
                { obstructions = Set.insert pos' (obstructions input),
                  position = pos,
                  direction = dir
                }
            )
            : go
              (Set.insert pathEntry visitedPath)
              (Set.insert pos' visitedPositions)
              (pathEntry' : pathEntries)
    go _ _ _ = []
