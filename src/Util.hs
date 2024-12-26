module Util (module Util) where

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p
