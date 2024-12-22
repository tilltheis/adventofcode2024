module Day02Spec (spec) where

import Day02 (parseInput, part1, part2)
import Test.Hspec

input :: [[Int]]
input =
  parseInput
    "7 6 4 2 1\n\
    \1 2 7 8 9\n\
    \9 7 6 2 1\n\
    \1 3 2 4 5\n\
    \8 6 4 4 1\n\
    \1 3 6 7 9\n"

spec :: Spec
spec = do
  describe "part1" $ do
    it "solves the official example" $ do
      part1 input `shouldBe` 2

  describe "part2" $ do
    it "solves the official example" $ do
      part2 input `shouldBe` 4
