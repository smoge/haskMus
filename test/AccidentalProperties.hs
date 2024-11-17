{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module AccidentalProperties where

import Control.Lens hiding (elements)
import Data.List (sort)
import Data.String (IsString (..))
import Pitch.Accidental
import Pitch.Pitch
import Pitch.PitchClass
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

prop_accidentalToSemitones :: Accidental -> Bool
prop_accidentalToSemitones acc =
  acc == semitonesToAccidental (accidentalToSemitones acc)

-- Round-trip Test
prop_roundTripAccidental :: Accidental -> Bool
prop_roundTripAccidental acc =
  semitonesToAccidental (accidentalToSemitones acc) == acc

-- Consistency Test
prop_consistencyOrder :: Bool
prop_consistencyOrder =
  allSemitones == fmap accidentalToSemitones (sort allAccidentals)

-- prop_consistencyOrder = allSemitones == map accidentalToSemitones (fromList allAccidentals)
-- Addition Property
prop_addition :: Accidental -> Property
prop_addition acc =
  forAll (elements allSemitones) $ \validSemitone ->
    let modified = validSemitone + accidentalToSemitones acc
     in modified `elem` allSemitones ==>
          addAccidental acc validSemitone
            == semitonesToAccidental modified

-- Multiplication Property
prop_multiplication :: Accidental -> Rational -> Property
prop_multiplication acc factor =
  factor /= 0 ==>
    let modified = factor * accidentalToSemitones acc
     in modified `elem` allSemitones ==>
          modifyAccidental acc (* factor)
            == semitonesToAccidental modified

-- | Checks whether adding an accidental to its inverse results in the natural accidental.
prop_addAccidentalInverse :: Accidental -> Bool
prop_addAccidentalInverse acc =
  addAccidental acc (negate $ accidentalToSemitones acc) == Natural

-- prop_fromStringValid :: AccidentalString -> Bool
-- prop_fromStringValid (AccidentalString str) = case str of
--   "ff" -> (fromString str :: Accidental) == DoubleFlat
--   "tqf" -> (fromString str :: Accidental) == ThreeQuartersFlat
--   "f" -> (fromString str :: Accidental) == Flat
--   "qf" -> (fromString str :: Accidental) == QuarterFlat
--   "" -> (fromString str :: Accidental) == Natural
--   "n" -> (fromString str :: Accidental) == Natural
--   "qs" -> (fromString str :: Accidental) == QuarterSharp
--   "s" -> (fromString str :: Accidental) == Sharp
--   "tqs" -> (fromString str :: Accidental) == ThreeQuartersSharp
--   "ss" -> (fromString str :: Accidental) == DoubleSharp
--   "sharp" -> (fromString str :: Accidental) == Sharp
--   "flat" -> (fromString str :: Accidental) == Flat
--   "natural" -> (fromString str :: Accidental) == Natural
--   "quartersharp" -> (fromString str :: Accidental) == QuarterSharp
--   "semisharp" -> (fromString str :: Accidental) == QuarterSharp
--   "quarterflat" -> (fromString str :: Accidental) == QuarterFlat
--   "semiflat" -> (fromString str :: Accidental) == QuarterFlat
--   _ -> True

return []

runTests :: IO Bool
runTests = $quickCheckAll

instance Arbitrary Accidental where
  arbitrary =
    frequency
      [ (10, elements allAccidentals), -- Picking from the predefined list
        (1, Custom <$> arbitrary) -- Picking a custom accidental
      ]

-- Newtype wrapper for specific accidental strings
newtype AccidentalString
  = AccidentalString String
  deriving (Show)

-- Arbitrary instance for AccidentalString (QuickCheck)
instance Arbitrary AccidentalString where
  arbitrary =
    AccidentalString
      <$> elements
        [ "ff",
          "tqf",
          "f",
          "qf",
          "",
          "n",
          "qs",
          "s",
          "tqs",
          "ss",
          "sharp",
          "flat",
          "natural",
          "quartersharp",
          "semisharp",
          "quarterflat",
          "semiflat",
          "♭",
          "♯",
          "♮",
          "𝄫",
          "𝄪",
          "𝄳",
          "𝄲"
        ]

instance Arbitrary NoteName where
  arbitrary = elements [C, D, E, F, G, A, B]

instance Arbitrary PitchClass where
  arbitrary = PitchClass <$> arbitrary <*> arbitrary

instance Arbitrary Octave where
  arbitrary :: Gen Octave
  arbitrary = Octave <$> arbitrary

instance Arbitrary Pitch where
  arbitrary = Pitch <$> arbitrary <*> arbitrary <*> arbitrary