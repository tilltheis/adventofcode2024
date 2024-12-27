module Day05 (Input (..), parseInput, part1, part2) where

import Data.Foldable (foldr')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Util (both, splitOn, splitOn2)

data Input = Input (Map Int (Set Int)) [[Int]] deriving (Eq, Show)

parseInput :: String -> Input
parseInput string = Input subsequents updates
  where
    (subsequentsStrings, updatesStrings) = splitOn2 "" $ lines string
    subsequentsPairs = map (both read . splitOn2 '|') subsequentsStrings
    step (precedent, subsequent) = Map.insertWith Set.union precedent (Set.singleton subsequent)
    subsequents = foldr step Map.empty subsequentsPairs
    updates = map (map read . splitOn ',') updatesStrings

isValid :: Map Int (Set Int) -> [Int] -> Bool
isValid subsequents = go Set.empty
  where
    go _ [] = True
    go seen (x : xs) = case Map.lookup x subsequents of
      Just subs | not $ Set.disjoint seen subs -> False
      _ -> go (Set.insert x seen) xs

midElement :: [Int] -> Int
midElement xs = xs !! (length xs `div` 2)

part1 :: Input -> Int
part1 (Input subsequents updates) = sum . map midElement $ filter (isValid subsequents) updates

part2 :: Input -> Int
part2 (Input subsequents updates) = sum . map (midElement . rearrange) $ filter (not . isValid subsequents) updates
  where
    rearrange = reverse . foldr' step []
    step x [] = [x]
    step x (y : ys) = case Map.lookup y subsequents of
      Just subs | Set.member x subs -> x : y : ys
      _ -> y : step x ys
