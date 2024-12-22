module Day03 (Instruction (..), Input, parseInput, part1, part2) where

import Data.Foldable (Foldable (foldl'))
import Text.Regex.TDFA ((=~))

data Instruction = Mul Int Int | Do | Dont deriving (Eq, Show)

type Input = [Instruction]

parseInput :: String -> Input
parseInput = map parseMatch . (=~ "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)|do|don't")
  where
    parseMatch ["do", _, _] = Do
    parseMatch ["don't", _, _] = Dont
    parseMatch [_, x, y] = Mul (read x) (read y)
    parseMatch _ = undefined

part1 :: Input -> Int
part1 = sum . map eval
  where
    eval (Mul x y) = x * y
    eval _ = 0

part2 :: Input -> Int
part2 = part1 . snd . foldl' f (True, [])
  where
    f (_, xs) Do = (True, xs)
    f (_, xs) Dont = (False, xs)
    f (True, xs) x = (True, x : xs)
    f (False, xs) _ = (False, xs)
