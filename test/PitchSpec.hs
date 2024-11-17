{-# LANGUAGE OverloadedStrings #-}

module PitchSpec where

import Control.Lens
import Data.Ratio
import Pitch.Accidental
import Pitch.Pitch
import Pitch.PitchClass
import Test.Hspec
import Pitch.Parser
import qualified Data.Text as T
import Control.Lens hiding (elements)
import qualified Pitch.Pitch as P
import qualified Pitch.PitchClass as PC


spec :: Spec
spec = do
  describe "PitchClass operations" $ do
    it "can view noteName and accidental from a PitchClass" $ do
      let c = PC.PitchClass C Natural
      (PC.noteName c) `shouldBe` C
      (PC.accidental c) `shouldBe` Natural
  
  describe "Pitch Parser" $ do
    it "parses correctly" $ do
      parsePitches (T.pack "cqs' cqf,  gqs''") `shouldBe` Right [Pitch C QuarterSharp (Octave 5), Pitch C QuarterFlat (Octave 3), Pitch G QuarterSharp (Octave 6)]

{-  it "can modify the accidental of a PitchClass" $ do
    let c = PitchClass C Natural
    let modifiedC = c & accidental .~ Sharp
    show modifiedC `shouldBe` "C Sharp"

  it "can modify the accidental using a function" $ do
    let c = PitchClass C Natural
    let modifiedC = c & accidental %~ (\x -> addAccidental x (1%2))
    show modifiedC `shouldBe` "C QuarterSharp"

  it "can modify all pitch classes in a list" $ do
    let pitchClasses = map (`PitchClass` Natural) [C .. B]
    let modifiedList = pitchClasses & each . accidental .~ Flat
    (show <$> modifiedList) `shouldBe` ["C Flat", "D Flat", "E Flat", "F Flat", "G Flat", "A Flat", "B Flat"]

  it "can check if a PitchClass has a specific accidental" $ do
    let c = PitchClass C Natural
    has (accidental . only Natural) c `shouldBe` True

  it "can conditionally modify a PitchClass" $ do
    let c = PitchClass C Natural
    let modifiedC = c & accidental . filtered (== Natural) .~ Flat
    show modifiedC `shouldBe` "C Flat"

describe "Pitch operations" $ do
  it "can view noteName, accidental and octave from a Pitch" $ do
    let p = Pitch C Natural (Octave 4)
    p ^. noteName `shouldBe` C
    p ^. accidental `shouldBe` Natural
    show (p ^. octave) `shouldBe` "Octave 4"

  it "can modify the accidental of a Pitch" $ do
    let p = Pitch C Natural (Octave 4)
    let modifiedP = p & accidental .~ Sharp
    show modifiedP `shouldBe` "C Sharp Octave 4"

  it "can modify the accidental using a function on a Pitch" $ do
    let p = Pitch C Natural (Octave 4)
    let modifiedP = p & accidental %~ (\x -> addAccidental x (1%2))
    show modifiedP `shouldBe` "C QuarterSharp Octave 4"

  it "can modify all pitches in a list" $ do
    let pitches = map (\x -> Pitch x Natural (Octave 4)) [C .. B]
    let modifiedList = pitches & each . accidental .~ Flat
    (show <$> modifiedList) `shouldBe` ["C Flat Octave 4", "D Flat Octave 4", "E Flat Octave 4", "F Flat Octave 4", "G Flat Octave 4", "A Flat Octave 4", "B Flat Octave 4"]

  it "can change the octave of a Pitch" $ do
    let p = Pitch C Natural (Octave 4)
    let modifiedP = p & octave .~ Octave 5
    show modifiedP `shouldBe` "C Natural Octave 5"

  it "can modify the octave using a function" $ do
    let p = Pitch C Natural (Octave 4)
    let modifiedP = p & octave %~ (\(Octave o) -> Octave (o + 1))
    show modifiedP `shouldBe` "C Natural Octave 5" -}

main :: IO ()
main = hspec spec

{-

spec :: Spec
spec = do
  describe "PitchClass operations" $ do
    let c = PitchClass C Natural

    it "can view noteName and accidental from a PitchClass" $ do
      c ^. noteName `shouldBe` C
      c ^. accidental `shouldBe` Natural

    it "can modify the accidental of a PitchClass" $ do
      show (c & accidental .~ Sharp) `shouldBe` "C Sharp"

    it "can modify the accidental using a function" $ do
      show (c & accidental %~ (\x -> addAccidental x (1%2))) `shouldBe` "C QuarterSharp"

    it "can modify all pitch classes in a list" $ do
      let pitchClasses = map (`PitchClass` Natural) [C .. B]
      (show <$> (pitchClasses & each . accidental .~ Flat)) `shouldBe` ["C Flat", "D Flat", "E Flat", "F Flat", "G Flat", "A Flat", "B Flat"]

    it "can check if a PitchClass has a specific accidental" $
      has (accidental . only Natural) c `shouldBe` True

    it "can conditionally modify a PitchClass" $
      show (c & accidental . filtered (== Natural) .~ Flat) `shouldBe` "C Flat"

  describe "Pitch operations" $ do
    let p = Pitch C Natural (Octave 4)

    it "can view noteName, accidental and octave from a Pitch" $ do
      p ^. noteName `shouldBe` C
      p ^. accidental `shouldBe` Natural
      show (p ^. octave) `shouldBe` "Octave 4"

    it "can modify the accidental of a Pitch" $
      show (p & accidental .~ Sharp) `shouldBe` "C Sharp Octave 4"

    it "can modify the accidental using a function on a Pitch" $
      show (p & accidental %~ (\x -> addAccidental x (1%2))) `shouldBe` "C QuarterSharp Octave 4"

    it "can modify all pitches in a list" $ do
      let pitches = map (\x -> Pitch x Natural (Octave 4)) [C .. B]
      (show <$> (pitches & each . accidental .~ Flat)) `shouldBe` ["C Flat Octave 4", "D Flat Octave 4", "E Flat Octave 4", "F Flat Octave 4", "G Flat Octave 4", "A Flat Octave 4", "B Flat Octave 4"]

    it "can change the octave of a Pitch" $
      show (p & octave .~ Octave 5) `shouldBe` "C Natural Octave 5"

    it "can increment the octave using a function" $
      show (p & octave %~ (\(Octave o) -> Octave (o + 1))) `shouldBe` "C Natural Octave 5"

    it "can decrement the octave using a function" $
      show (p & octave %~ (\(Octave o) -> Octave (o - 1))) `shouldBe` "C Natural Octave 3"

    -- Negative scenarios
    it "should not change octave when decreasing beyond limit" $ do
      let pLow = Pitch C Natural (Octave 0)
      show (pLow & octave %~ (\(Octave o) -> if o > 0 then Octave (o - 1) else Octave o)) `shouldBe` "C Natural Octave 0"

    it "should not change octave when increasing beyond limit" $ do
      let pHigh = Pitch C Natural (Octave 8) -- Assuming 8 is the highest octave
      show (pHigh & octave %~ (\(Octave o) -> if o < 8 then Octave (o + 1) else Octave o)) `shouldBe` "C Natural Octave 8"

 -}
