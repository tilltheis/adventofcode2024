module Day10Spec (spec) where

import Day10
import Test.Hspec

input :: Input
input =
  parseInput
    "89010123\n\
    \78121874\n\
    \87430965\n\
    \96549874\n\
    \45678903\n\
    \32019012\n\
    \01329801\n\
    \10456732"

spec :: Spec
spec = do
  describe "part1" $ do
    it "solves the official example" $ do
      part1 input `shouldBe` 36

  describe "part2" $ do
    it "solves the official example" $ do
      part2 input `shouldBe` 81
