module Day11Spec (spec) where

import Day11
import Test.Hspec

input :: Input
input = parseInput "125 17"

spec :: Spec
spec = do
  describe "part1" $ do
    it "solves the official example" $ do
      part1 input `shouldBe` 55312

  describe "part2" $ do
    it "solves the official example" $ do
      part2 input `shouldBe` 65601038650482
