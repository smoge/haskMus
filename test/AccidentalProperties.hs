{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module AccidentalProperties where

import Data.List (sort)
import Data.String (IsString (..))
import Pitch.Accidental
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
  allSemitones == map accidentalToSemitones (sort allAccidentals)

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
