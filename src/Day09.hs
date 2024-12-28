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

replace1 :: (Eq a) => a -> a -> [a] -> [a]
replace1 _ _ [] = []
replace1 from to (x : xs) | x == from = to : xs
replace1 from to (x : xs) = x : replace1 from to xs

part2 :: Input -> Int
part2 input = sum . zipWith (*) [0 ..] . expand $ moveAll reformattedLBlocks reformattedRBlocks
  where
    reformattedLBlocks = reformat 0 input
    reformattedRBlocks = reverse reformattedLBlocks

    reformat _ [] = []
    reformat fileId [files] = [(Just fileId, files)]
    reformat fileId (files : frees : blocks) =
      (Just fileId, files) : (Nothing, frees) : reformat (fileId + 1) blocks

    expand [] = []
    expand ((Nothing, size) : blocks) = replicate size 0 ++ expand blocks
    expand ((Just ident, size) : blocks) = replicate size ident ++ expand blocks

    moveAll lblocks [] = lblocks
    moveAll lblocks ((Nothing, _) : rblocks) = moveAll lblocks rblocks
    moveAll lblocks ((Just fileId, size) : rblocks) = moveAll (moveOne lblocks fileId size) rblocks

    moveOne lblocks@((Just lid, _) : _) rid _ | lid == rid = lblocks
    moveOne ((Nothing, lsize) : lblocks) rid rsize
      | lsize >= rsize =
          (Just rid, rsize) : (Nothing, lsize - rsize) : replace1 (Just rid, rsize) (Nothing, rsize) lblocks
    moveOne (lblock : lblocks) rid rsize = lblock : moveOne lblocks rid rsize
    moveOne _ _ _ = undefined