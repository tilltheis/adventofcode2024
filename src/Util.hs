module Util (module Util) where

import Data.Bifunctor (Bifunctor (second))

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn x ys = l : splitOn x (drop 1 r)
  where
    (l, r) = break (== x) $ dropWhile (== x) ys

splitOn2 :: (Eq a) => a -> [a] -> ([a], [a])
splitOn2 x = second (drop 1) . break (== x)