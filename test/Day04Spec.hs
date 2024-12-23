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
  describe "leftToRight" $ do
    it "transforms the input correctly" $ do
      leftToRight (parseInput "123\n456") `shouldBe` parseInput "123\n456"
    it "transforms the input correctly2" $ do
      leftToRight (parseInput "1234\n5678\n90ab\ncdef") `shouldBe` parseInput "1234\n5678\n90ab\ncdef"

  describe "rightToLeft" $ do
    it "transforms the input correctly" $ do
      rightToLeft (parseInput "123\n456") `shouldBe` parseInput "321\n654"
    it "transforms the input correctly2" $ do
      rightToLeft (parseInput "1234\n5678\n90ab\ncdef") `shouldBe` parseInput "4321\n8765\nba09\nfedc"

  describe "topToBottom" $ do
    it "transforms the input correctly" $ do
      topToBottom (parseInput "123\n456") `shouldBe` parseInput "14\n25\n36"
    it "transforms the input correctly2" $ do
      topToBottom (parseInput "1234\n5678\n90ab\ncdef") `shouldBe` parseInput "159c\n260d\n37ae\n48bf"

  describe "bottomToTop" $ do
    it "transforms the input correctly" $ do
      bottomToTop (parseInput "123\n456") `shouldBe` parseInput "41\n52\n63"
    it "transforms the input correctly2" $ do
      bottomToTop (parseInput "1234\n5678\n90ab\ncdef") `shouldBe` parseInput "c951\nd062\nea73\nfb84"

  describe "bottomLeftToTopRight" $ do
    it "transforms the input correctly" $ do
      bottomLeftToTopRight (parseInput "123\n456") `shouldBe` parseInput "1\n42\n53\n6"
    it "transforms the input correctly2" $ do
      bottomLeftToTopRight (parseInput "1234\n5678\n90ab\ncdef") `shouldBe` parseInput "1\n52\n963\nc074\nda8\neb\nf"

  describe "topRightToBottomLeft" $ do
    it "transforms the input correctly" $ do
      topRightToBottomLeft (parseInput "123\n456") `shouldBe` parseInput "6\n35\n24\n1"
    it "transforms the input correctly2" $ do
      topRightToBottomLeft (parseInput "1234\n5678\n90ab\ncdef") `shouldBe` parseInput "f\nbe\n8ad\n470c\n369\n25\n1"

  describe "topLeftToBottomRight" $ do
    it "transforms the input correctly" $ do
      topLeftToBottomRight (parseInput "123\n456") `shouldBe` parseInput "4\n15\n26\n3"
    it "transforms the input correctly2" $ do
      topLeftToBottomRight (parseInput "1234\n5678\n90ab\ncdef") `shouldBe` parseInput "c\n9d\n50e\n16af\n27b\n38\n4"

    describe "bottomRightToTopLeft" $ do
      it "transforms the input correctly" $ do
        bottomRightToTopLeft (parseInput "123\n456") `shouldBe` parseInput "3\n62\n51\n4"
      it "transforms the input correctly2" $ do
        bottomRightToTopLeft (parseInput "1234\n5678\n90ab\ncdef") `shouldBe` parseInput "4\n83\nb72\nfa61\ne05\nd9\nc"

    describe "part1" $ do
      it "solves the official example" $ do
        part1 input `shouldBe` 18

    describe "part2" $ do
      it "solves the official example" $ do
        part2 input `shouldBe` 9
