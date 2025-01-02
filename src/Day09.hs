module Day09 (Input, parseInput, part1, part2) where

import Data.Bifunctor (Bifunctor (first))
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import qualified Data.Map as Map
import Util (parseDigit)

type Input = [Int]

parseInput :: String -> Input
parseInput = map parseDigit . dropWhileEnd isSpace

part1 :: Input -> Int
part1 input = sum . zipWith (*) [0 ..] $ go 0 expandedLBlocks (length expandedRBlocks - 1) expandedRBlocks
  where
    expandedLBlocks = expand 0 input
    expandedRBlocks = reverse expandedLBlocks
    expand _ [] = []
    expand fileId [files] = replicate files (Just fileId)
    expand fileId (files : frees : blocks) =
      replicate files (Just fileId) ++ replicate frees Nothing ++ expand (fileId + 1) blocks
    go i _ j _ | i > j = []
    go i (Just file : lblocks) j rblocks = file : go (i + 1) lblocks j rblocks
    go i (Nothing : lblocks) j (Just file : rblocks) = file : go (i + 1) lblocks (j - 1) rblocks
    go i (Nothing : lblocks) j (Nothing : rblocks) = go i (Nothing : lblocks) (j - 1) rblocks
    go _ _ _ _ = undefined

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt _ _ [] = []
modifyAt 0 f (x : xs) = f x : xs
modifyAt i f (x : xs) = x : modifyAt (i - 1) f xs

type SpaceState = [[Int]] -- len -> [start]

mkSpaceState :: [(Maybe Int, Int, Int)] -> SpaceState
mkSpaceState = foldr step $ replicate 10 []
  where
    step (Just _, _, _) spaceState = spaceState
    step (Nothing, start, len) spaceState = modifyAt len (start :) spaceState

insertIntoMatchingSpace :: Int -> SpaceState -> Maybe (Int, SpaceState)
insertIntoMatchingSpace len spaceState = do
  (idx, start) <- first (len +) <$> findSmallestStart (drop len spaceState)
  return (start, updateSpace idx (start + len) spaceState)
  where
    findSmallestStart = snd . foldl step (0, Nothing)
    step (j, Just (i, x)) (y : _) = (j + 1, if x < y then Just (i, x) else Just (j, y))
    step (j, Nothing) (y : _) = (j + 1, Just (j, y))
    step (j, acc) [] = (j + 1, acc)
    updateSpace idx start = insertSpace (idx - len) start . deleteSpace idx
    deleteSpace idx = modifyAt idx tail
    insertSpace 0 _ = id
    insertSpace idx start = modifyAt idx (insertSorted start)
    insertSorted start [] = [start]
    insertSorted start (x : xs) | start < x = start : x : xs
    insertSorted start (x : xs) = x : insertSorted start xs

part2 :: Input -> Int
part2 input = sum . zipWith (*) [0 ..] . expand 0 . moveAll $ reformat 0 0 input
  where
    reformat _ _ [] = []
    reformat start fileId [files] = [(Just fileId, start, files)]
    reformat start fileId (files : frees : blocks) =
      (Just fileId, start, files)
        : [(Nothing, start + files, frees) | frees > 0]
        ++ reformat (start + files + frees) (fileId + 1) blocks

    expand i _ | i >= sum input = []
    expand i m = case Map.lookup i m of
      Just (fileId, len) -> replicate len fileId ++ expand (i + len) m
      Nothing -> 0 : expand (i + 1) m

    moveAll blocks = fst $ foldr step (Map.empty, mkSpaceState blocks) blocks
      where
        step (Just fileId, start, len) (acc, spaceState) =
          case insertIntoMatchingSpace len spaceState of
            Just (spaceStart, spaceState')
              | spaceStart < start ->
                  (Map.insert spaceStart (fileId, len) acc, spaceState')
            _ -> (Map.insert start (fileId, len) acc, spaceState)
        step (Nothing, _, _) acc = acc
