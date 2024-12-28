module Day09 (Input, parseInput, part1, part2) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

type Input = [Int]

parseInput :: String -> Input
parseInput = map (read . (: [])) . dropWhileEnd isSpace

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

part2 :: Input -> Int
part2 = const 0