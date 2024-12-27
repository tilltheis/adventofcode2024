module Day08Spec (spec) where

import Day08
import Test.Hspec

input :: Input
input =
  parseInput
    "............\n\
    \........0...\n\
    \.....0......\n\
    \.......0....\n\
    \....0.......\n\
    \......A.....\n\
    \............\n\
    \............\n\
    \........A...\n\
    \.........A..\n\
    \............\n\
    \............"

spec :: Spec
spec = do
  describe "part1" $ do
    it "solves the official example" $ do
      part1 input `shouldBe` 14

  describe "part2" $ do
    it "solves the official example" $ do
      part2 input `shouldBe` 34
