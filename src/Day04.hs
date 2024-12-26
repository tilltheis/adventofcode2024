module Day04 (Input, parseInput, part1, part2) where

import Util (count)

type Input = [[Char]]

parseInput :: String -> Input
parseInput = lines

countPatternMatches :: Input -> Input -> Int
countPatternMatches pattern input = count (== pattern) $ map mask subGrids
  where
    patternWidth = length $ head pattern
    patternHeight = length pattern
    subGrids = do
      row <- [0 .. length input - patternHeight]
      col <- [0 .. length (head input) - patternWidth]
      return $ map (take patternWidth . drop col) $ take patternHeight $ drop row input
    mask :: Input -> Input
    mask subGrid = zipWith (zipWith f) subGrid pattern
      where
        f gridChar patternChar = if patternChar == '.' then '.' else gridChar

part1 :: Input -> Int
part1 input = sum $ map (`countPatternMatches` input) patterns
  where
    patterns =
      [ lines "XMAS",
        lines "SAMX",
        lines "X\nM\nA\nS",
        lines "S\nA\nM\nX",
        lines "X...\n.M..\n..A.\n...S",
        lines "S...\n.A..\n..M.\n...X",
        lines "...X\n..M.\n.A..\nS...",
        lines "...S\n..A.\n.M..\nX..."
      ]

part2 :: Input -> Int
part2 input = sum $ map (`countPatternMatches` input) patterns
  where
    patterns =
      [ lines "M.S\n.A.\nM.S",
        lines "M.M\n.A.\nS.S",
        lines "S.M\n.A.\nS.M",
        lines "S.S\n.A.\nM.M"
      ]
