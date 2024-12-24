module Day06 (Input (..), parseInput, part1, part2) where

import Data.Foldable (foldr')
import Data.Maybe (fromJust)
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

part1 :: Input -> Int
part1
  Input
    { dimensions = (width, height),
      obstructions = obs,
      position = startPos,
      direction = startDir
    } = Set.size $ simulate startPos startDir Set.empty
    where
      turnRight (0, -1) = (1, 0)
      turnRight (1, 0) = (0, 1)
      turnRight (0, 1) = (-1, 0)
      turnRight (-1, 0) = (0, -1)
      turnRight _ = undefined
      isOutOfBounds (x, y) = x < 0 || y < 0 || x >= width || y >= height
      simulate pos@(x, y) dir@(dx, dy) visited
        | isOutOfBounds pos = visited
        | otherwise =
            let pos' = (x + dx, y + dy)
             in if Set.member pos' obs
                  then simulate pos (turnRight dir) visited
                  else simulate pos' dir (Set.insert pos visited)

part2 :: Input -> Int
part2 = const 0