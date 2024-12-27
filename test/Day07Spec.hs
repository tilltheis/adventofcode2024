module Day07Spec (spec) where

import Day07
import Test.Hspec

input :: Input
input =
  parseInput
    "190: 10 19\n\
    \3267: 81 40 27\n\
    \83: 17 5\n\
    \156: 15 6\n\
    \7290: 6 8 6 15\n\
    \161011: 16 10 13\n\
    \192: 17 8 14\n\
    \21037: 9 7 18 13\n\
    \292: 11 6 16 20"

spec :: Spec
spec = do
  describe "part1" $ do
    it "solves the official example" $ do
      part1 input `shouldBe` 3749

  describe "part2" $ do
    it "solves the official example" $ do
      part2 input `shouldBe` 11387
