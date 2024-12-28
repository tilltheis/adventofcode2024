module Day09Spec (spec) where

import Day09
import Test.Hspec

input :: Input
input = parseInput "2333133121414131402"

spec :: Spec
spec = do
  describe "part1" $ do
    it "solves the official example" $ do
      part1 input `shouldBe` 1928

  describe "part2" $ do
    it "solves the official example" $ do
      part2 input `shouldBe` 0
