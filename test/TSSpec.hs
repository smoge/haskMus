module TSSpec
  ( specTS
  ) where

import Control.Lens ( (&), (.~) )
import Data.Ratio ( (%) )
import Music.Time.TS
    ( TimeSignature(..),
      applyFunctionToTS,
      (//),
      applyFunctionToTS',
      fromDur,
      fromDur',
      upper,
      lower ) 

import Test.Hspec ( Spec, describe, it, shouldBe, hspec )


specTS :: Spec
specTS = do
  describe "applyFunctionToTS" $ do
    it "applies (+ 1%8) to 4//8 and keeps the same denominator" $ do
      let result = Music.Time.TS.applyFunctionToTS (+ (1 % 8)) (Music.Time.TS.TimeSignature 4 8)
      result `shouldBe` Music.Time.TS.TimeSignature 5 8
    it "applies (+ 3%8) to 4//8 and keeps the same denominator" $ do
      let result = Music.Time.TS.applyFunctionToTS (+ (3 % 8)) (4 Music.Time.TS.// 8)
      result `shouldBe` (7 Music.Time.TS.// 8)
    it "applies (* 5%8) to 3//4 and keeps the same denominator" $ do
      let result = Music.Time.TS.applyFunctionToTS (* (5 % 8)) (3 Music.Time.TS.// 4)
      result `shouldBe` (15 Music.Time.TS.// 32)
    it "applies (+ 3%8) to 4//4 and finds the best denominator" $ do
      let result = Music.Time.TS.applyFunctionToTS (+ (3 % 8)) (4 Music.Time.TS.// 4)
      result `shouldBe` (11 Music.Time.TS.// 8)
  describe "applyFunctionToTS'" $ do
    it "applies (+ 1%8) to 4//4 with preferred denominator 4" $ do
      let result = Music.Time.TS.applyFunctionToTS' (+ (1 % 8)) (Just 4) (4 Music.Time.TS.// 4)
      result `shouldBe` (9 Music.Time.TS.// 8)
    it "applies (* 2) to 5//8 without specifying a denominator" $ do
      let result = Music.Time.TS.applyFunctionToTS' (* 2) Nothing (5 Music.Time.TS.// 8)
      result `shouldBe` (10 Music.Time.TS.// 8)
    it "applies (+ 1%4) to 3//4 without specifying a denominator" $ do
      let result = Music.Time.TS.applyFunctionToTS' (+ (1 % 4)) Nothing (3 Music.Time.TS.// 4)
      result `shouldBe` (4 Music.Time.TS.// 4)
  describe "fromDur" $ do
    it "converts 4%8 to 8 with a denominator of 8" $ do
      let result = Music.Time.TS.fromDur (4 % 8) 8
      result `shouldBe` (4 Music.Time.TS.// 8)
    it "converts 5%8 to 4 with a denominator of 4" $ do
      let result = Music.Time.TS.fromDur (5 % 8) 4
      result `shouldBe` (5 Music.Time.TS.// 8)
    it "converts 1%2 to 16 with a denominator of 16" $ do
      let result = Music.Time.TS.fromDur (1 % 2) 16
      result `shouldBe` (8 Music.Time.TS.// 16)
  describe "fromDur'" $ do
    it "converts 4%8 to 8 with a preferred denominator of 8" $ do
      let result = Music.Time.TS.fromDur' (4 % 8) (Just 8)
      result `shouldBe` (4 Music.Time.TS.// 8)
    it "converts 5%8 to 8 without specifying a denominator" $ do
      let result = Music.Time.TS.fromDur' (5 % 8) Nothing
      result `shouldBe` (5 Music.Time.TS.// 8)
    it "converts 1%2 to 4 with a preferred denominator of 4" $ do
      let result = Music.Time.TS.fromDur' (1 % 2) (Just 4)
      result `shouldBe` (2 Music.Time.TS.// 4)
  describe "modifying TimeSignature" $ do
    it "modifies num and den of 4//4 to 7//8" $ do
      let result = (4 Music.Time.TS.// 4) & Music.Time.TS.upper .~ 7 & Music.Time.TS.lower .~ 8
      result `shouldBe` (7 Music.Time.TS.// 8)

main :: IO ()
main = hspec specTS
