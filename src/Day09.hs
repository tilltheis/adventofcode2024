module Day09 (Input, parseInput, part1, part2) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe, isNothing)

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

replace :: (Eq a) => a -> a -> [a] -> [a]
replace _ _ [] = []
replace from to (x : xs) =
  if x == from
    then to : replace from to xs
    else x : replace from to xs

part2 :: Input -> Int
part2 input = sum . zipWith (*) [0 ..] . map (fromMaybe 0) $ moveAll expandedLBlocks expandedRBlocks
  where
    expandedLBlocks = expand 0 input
    expandedRBlocks = reverse expandedLBlocks
    expand _ [] = []
    expand fileId [files] = replicate files (Just fileId)
    expand fileId (files : frees : blocks) =
      replicate files (Just fileId) ++ replicate frees Nothing ++ expand (fileId + 1) blocks
    moveAll lblocks [] = lblocks
    moveAll lblocks (Nothing : rblocks) = moveAll lblocks rblocks
    moveAll lblocks rblocks@(Just fileId : _) = moveAll lblocks' rblocks'
      where
        (files, rblocks') = span (== Just fileId) rblocks
        lblocks' = moveOne lblocks files
    moveOne lblocks@(lblock : _) (rblock : _) | lblock == rblock = lblocks
    moveOne (Just lfile : lblocks) rblocks = Just lfile : moveOne lblocks rblocks
    moveOne lblocks@(Nothing : _) rblocks@(rblock : _) =
      let (lfrees, lblocks') = span isNothing lblocks
       in if length lfrees >= length rblocks
            then rblocks ++ drop (length rblocks) lfrees ++ replace rblock Nothing lblocks'
            else lfrees ++ moveOne lblocks' rblocks
    moveOne _ _ = undefined