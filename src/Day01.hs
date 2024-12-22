module Day01 (parseInput, part1, part2) where

import Data.List (sort)

listToPair :: [a] -> (a, a)
listToPair [x, y] = (x, y)
listToPair _ = undefined

parseInput :: String -> ([Int], [Int])
parseInput = unzip . map (listToPair . map read . words) . lines

diff :: Int -> Int -> Int
diff x y = abs $ x - y

part1 :: ([Int], [Int]) -> Int
part1 (l1, l2) = sum $ zipWith diff (sort l1) (sort l2)

countElem :: (Eq a) => a -> [a] -> Int
countElem x = length . filter (x ==)

similarity :: [Int] -> Int -> Int
similarity l x = (x *) $ countElem x l

part2 :: ([Int], [Int]) -> Int
part2 (l1, l2) = sum $ map (similarity l2) l1
