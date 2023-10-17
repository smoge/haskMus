{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module AccidentalProperties where

import           Data.List              (sort)
import           Music.Pitch.Accidental
import           Test.QuickCheck

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
     in modified `elem` allSemitones ==> addAccidental acc validSemitone ==
        semitonesToAccidental modified

-- Multiplication Property
prop_multiplication :: Accidental -> Rational -> Property
prop_multiplication acc factor =
  factor /= 0 ==>
  let modified = factor * accidentalToSemitones acc
   in modified `elem` allSemitones ==> modifyAccidental acc (* factor) ==
      semitonesToAccidental modified

-- | Checks whether adding an accidental to its inverse results in the natural accidental.
prop_addAccidentalInverse :: Accidental -> Bool
prop_addAccidentalInverse acc =
  addAccidental acc (negate $ accidentalToSemitones acc) == Natural

return []

runTests :: IO Bool
runTests = $quickCheckAll
