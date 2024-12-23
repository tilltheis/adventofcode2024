module Day04Spec (spec) where

import Day04
import Test.Hspec

input :: Input
input =
  parseInput
    "MMMSXXMASM\n\
    \MSAMXMSMSA\n\
    \AMXSXMAAMM\n\
    \MSAMASMSMX\n\
    \XMASAMXAMM\n\
    \XXAMMXXAMA\n\
    \SMSMSASXSS\n\
    \SAXAMASAAA\n\
    \MAMMMXMMMM\n\
    \MXMXAXMASX"

spec :: Spec
spec = do
  describe "part1" $ do
    it "solves the official example" $ do
      part1 input `shouldBe` 18

  describe "part2" $ do
    it "solves the official example" $ do
      part2 input `shouldBe` 9
