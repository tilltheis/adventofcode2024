module Day05 (Input (..), parseInput, part1, part2) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Input = Input (Map Int (Set Int)) [[Int]] deriving (Eq, Show)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (a, b) = (f a, f b)

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn x ys = l : splitOn x (drop 1 r)
  where
    (l, r) = break (== x) $ dropWhile (== x) ys

splitOnPair :: (Eq a) => a -> [a] -> ([a], [a])
splitOnPair x = mapSnd (drop 1) . break (== x)

parseInput :: String -> Input
parseInput string = Input subsequents updates
  where
    (subsequentsStrings, updatesStrings) = splitOnPair "" $ lines string
    subsequentsPairs = map (mapPair read . splitOnPair '|') subsequentsStrings
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
    rearrange = reverse . Set.foldr step [] . Set.fromList
    step x [] = [x]
    step x (y : ys) = case Map.lookup y subsequents of
      Just subs | Set.member x subs -> x : y : ys
      _ -> y : step x ys
