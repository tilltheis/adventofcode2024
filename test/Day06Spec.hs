module Day06Spec (spec) where

import Day06
import Test.Hspec

input :: Input
input =
  parseInput
    "....#.....\n\
    \.........#\n\
    \..........\n\
    \..#.......\n\
    \.......#..\n\
    \..........\n\
    \.#..^.....\n\
    \........#.\n\
    \#.........\n\
    \......#..."

spec :: Spec
spec = do
  describe "part1" $ do
    it "solves the official example" $ do
      part1 input `shouldBe` 41

  describe "part2" $ do
    it "solves the official example" $ do
      part2 input `shouldBe` 0
