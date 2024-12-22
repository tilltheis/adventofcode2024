module Day01Spec (spec) where

import Day01
import Test.Hspec

input :: Input
input =
  parseInput
    "3   4\n\
    \4   3\n\
    \2   5\n\
    \1   3\n\
    \3   9\n\
    \3   3\n"

spec :: Spec
spec = do
  describe "part1" $ do
    it "solves the official example" $ do
      part1 input `shouldBe` 11

  describe "part2" $ do
    it "solves the official example" $ do
      part2 input `shouldBe` 31
