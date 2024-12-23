module Day05Spec (spec) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Day05
import Test.Hspec

input :: Input
input =
  parseInput
    "47|53\n\
    \97|13\n\
    \97|61\n\
    \97|47\n\
    \75|29\n\
    \61|13\n\
    \75|53\n\
    \29|13\n\
    \97|29\n\
    \53|29\n\
    \61|53\n\
    \97|53\n\
    \61|29\n\
    \47|13\n\
    \75|47\n\
    \97|75\n\
    \47|61\n\
    \75|61\n\
    \47|29\n\
    \75|13\n\
    \53|13\n\
    \\n\
    \75,47,61,53,29\n\
    \97,61,53,29,13\n\
    \75,29,13\n\
    \75,97,47,61,53\n\
    \61,13,29\n\
    \97,13,75,29,47"

spec :: Spec
spec = do
  describe "parseInput" $ do
    it "parses the official example" $ do
      input
        `shouldBe` Input
          ( Map.fromList
              [ (47, Set.fromList [53, 13, 61, 29]),
                (97, Set.fromList [13, 61, 47, 29, 53, 75]),
                (75, Set.fromList [29, 53, 47, 61, 13]),
                (61, Set.fromList [13, 53, 29]),
                (29, Set.fromList [13]),
                (53, Set.fromList [29, 13])
              ]
          )
          [ [75, 47, 61, 53, 29],
            [97, 61, 53, 29, 13],
            [75, 29, 13],
            [75, 97, 47, 61, 53],
            [61, 13, 29],
            [97, 13, 75, 29, 47]
          ]

  describe "part1" $ do
    it "solves the official example" $ do
      part1 input `shouldBe` 143

  describe "part2" $ do
    it "solves the official example" $ do
      part2 input `shouldBe` 123
