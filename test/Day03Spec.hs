module Day03Spec (spec) where

import Day03
import Test.Hspec

input1 :: Input
input1 = parseInput "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

input2 :: Input
input2 = parseInput "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

spec :: Spec
spec = do
  describe "parseInput" $ do
    it "parses the 1st official example" $ do
      input1 `shouldBe` [Mul 2 4, Do, Mul 5 5, Mul 11 8, Mul 8 5]

    it "parses the 2nd official example" $ do
      input2 `shouldBe` [Mul 2 4, Dont, Mul 5 5, Mul 11 8, Do, Mul 8 5]

  describe "part1" $ do
    it "solves the 1st official example" $ do
      part1 input1 `shouldBe` 161

  describe "part2" $ do
    it "solves the 2nd official example" $ do
      part2 input2 `shouldBe` 48
