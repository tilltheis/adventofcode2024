module Day04
  ( Input,
    parseInput,
    part1,
    part2,
    leftToRight,
    rightToLeft,
    topToBottom,
    bottomToTop,
    bottomLeftToTopRight,
    topRightToBottomLeft,
    topLeftToBottomRight,
    bottomRightToTopLeft,
  )
where

import Data.List (transpose)

type Input = [[Char]]

parseInput :: String -> Input
parseInput = lines

leftToRight :: Input -> Input
leftToRight = id

rightToLeft :: Input -> Input
rightToLeft = map reverse

topToBottom :: Input -> Input
topToBottom = transpose

bottomToTop :: Input -> Input
bottomToTop = map reverse . topToBottom

bottomLeftToTopRight :: Input -> Input
bottomLeftToTopRight input =
  firstHalf ++ secondHalf
  where
    firstHalf =
      [ [ input !! (startRow - col) !! col
          | col <- [0 .. min startRow (length (head input) - 1)]
        ]
        | startRow <- [0 .. length input - 1]
      ]
    secondHalf =
      reverse $
        [ [ input !! (length input - 1 - row) !! (length (head input) - 1 - (startCol - row))
            | row <- [0 .. min startCol (length input - 1)]
          ]
          | startCol <- [0 .. length (head input) - 2]
        ]

topRightToBottomLeft :: Input -> Input
topRightToBottomLeft = reverse . map reverse . bottomLeftToTopRight

topLeftToBottomRight :: Input -> Input
topLeftToBottomRight input =
  firstHalf ++ secondHalf
  where
    firstHalf =
      [ [ input !! (length input - 1 - startRow + col) !! col
          | col <- [0 .. min startRow (length (head input) - 1)]
        ]
        | startRow <- [0 .. length input - 1]
      ]
    secondHalf =
      reverse $
        [ [ input !! row !! (length (head input) - 1 - (startCol - row))
            | row <- [0 .. min startCol (length input - 1)]
          ]
          | startCol <- [0 .. length (head input) - 2]
        ]

bottomRightToTopLeft :: Input -> Input
bottomRightToTopLeft = reverse . map reverse . topLeftToBottomRight

slidingWindows :: Int -> [a] -> [[a]]
slidingWindows n xs
  | length xs < n = []
  | otherwise = take n xs : slidingWindows n (tail xs)

part1 :: Input -> Int
part1 input =
  sum $
    map
      f
      [ leftToRight input,
        rightToLeft input,
        topToBottom input,
        bottomToTop input,
        bottomLeftToTopRight input,
        topRightToBottomLeft input,
        topLeftToBottomRight input,
        bottomRightToTopLeft input
      ]
  where
    f = sum . map g
    g = length . filter (== "XMAS") . slidingWindows 4

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

part2 :: Input -> Int
part2 input = count (`elem` patterns) $ map mask subGrids
  where
    patterns =
      [ lines "M.S\n.A.\nM.S",
        lines "M.M\n.A.\nS.S",
        lines "S.M\n.A.\nS.M",
        lines "S.S\n.A.\nM.M"
      ]
    subGrids = do
      row <- [0 .. length input - 3]
      col <- [0 .. length (head input) - 3]
      return $ map (take 3 . drop col) $ take 3 $ drop row input
    mask [[a, _, b], [_, c, _], [d, _, e]] = [[a, '.', b], ['.', c, '.'], [d, '.', e]]
    mask _ = undefined
