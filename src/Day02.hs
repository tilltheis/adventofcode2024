module Day02 (parseInput, part1, part2) where

parseInput :: String -> [[Int]]
parseInput = map (map read . words) . lines

isSafelyInc :: Int -> Int -> Bool
isSafelyInc x y = x < y && y - x <= 3

isSafelyDec :: Int -> Int -> Bool
isSafelyDec = flip isSafelyInc

isSafe1 :: [Int] -> Bool
isSafe1 xs = isSafe1' isSafelyInc xs || isSafe1' isSafelyDec xs

isSafe1' :: (Int -> Int -> Bool) -> [Int] -> Bool
isSafe1' p (x : y : zs) = p x y && isSafe1' p (y : zs)
isSafe1' _ _ = True

part1 :: [[Int]] -> Int
part1 = length . filter isSafe1

isSafe2 :: [Int] -> Bool
isSafe2 xs@(x : y : ys) =
  ( if isSafelyInc x y
      then isSafe2' isSafelyInc xs
      else isSafe1' isSafelyInc (y : ys) || isSafe1' isSafelyInc (x : ys)
  )
    || ( if isSafelyDec x y
           then isSafe2' isSafelyDec xs
           else isSafe1' isSafelyDec (y : ys) || isSafe1' isSafelyDec (x : ys)
       )
isSafe2 _ = True

isSafe2' :: (Int -> Int -> Bool) -> [Int] -> Bool
isSafe2' p (x : y : z : zs) =
  if p y z
    then isSafe2' p (y : z : zs)
    else isSafe1' p (x : z : zs) || isSafe1' p (x : y : zs)
isSafe2' _ _ = True

part2 :: [[Int]] -> Int
part2 = length . filter isSafe2
