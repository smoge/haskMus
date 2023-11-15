{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# HLINT ignore "Redundant id" #-}

module PitchProperties where

import Control.Lens hiding (elements)
import Data.Ratio ((%))
import Pitch.Accidental
import Pitch.Pitch
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

-- Properties for PitchClass and Pitch:

-- When we set an accidental to a specific value, it should always be that value.
prop_setAccidental :: Accidental -> PitchClass -> Bool
prop_setAccidental a pc = (pc & accidental .~ a) ^. accidental == a

-- Similarly, for Pitch:
prop_setAccidentalPitch :: Accidental -> Pitch -> Bool
prop_setAccidentalPitch a p = (p & accidental .~ a) ^. accidental == a

-- invertAccidental' :: Accidental -> Accidental
-- invertAccidental' Natural = Natural
-- invertAccidental' Sharp = Flat
-- invertAccidental' Flat = Sharp
-- invertAccidental' QuarterSharp = QuarterFlat
-- invertAccidental' QuarterFlat = QuarterSharp
-- invertAccidental' DoubleFlat = DoubleSharp
-- invertAccidental' DoubleSharp = DoubleFlat
-- invertAccidental' ThreeQuartersFlat = ThreeQuartersSharp
-- invertAccidental' ThreeQuartersSharp = ThreeQuartersFlat
-- invertAccidental' (Custom a) = negate (Custom a)

-- prop_modifyAccidental :: Accidental -> PitchClass -> Bool
-- prop_modifyAccidental a pc =
--   let modifiedPC = pc & accidental .~ a & accidental %~ invertAccidental'
--    in modifiedPC ^. accidental == invertAccidental' a

-- prop_setAndModifyAccidental :: Accidental -> PitchClass -> Bool
-- prop_setAndModifyAccidental a pc =
--   let modifiedPC1 = pc & accidental .~ a & accidental %~ accToNatural
--       modifiedPC2 = pc & accidental %~ accToNatural
--       accToNatural :: Accidental -> Accidental
--       accToNatural _ = Natural
--    in modifiedPC1 == modifiedPC2

-- prop_invertTwiceIsIdentity :: Accidental -> Bool
-- prop_invertTwiceIsIdentity a =
--   let modifiedA = invertAccidental' a
--    in invertAccidental' modifiedA == a

prop_identityAccidentalIsUnchanged :: Accidental -> Bool
prop_identityAccidentalIsUnchanged a =
    let modifiedA = a & id
     in modifiedA == a

-- prop_modifyAccidentalCommutative :: Accidental -> PitchClass -> Bool
-- prop_modifyAccidentalCommutative a pc =
--   let modifiedPC1 = pc & accidental %~ invertAccidental''
--       modifiedPC2 =
--         pc & accidental %~ (\x -> invertAccidental'' (a & accidental .~ x))
--    in modifiedPC1 == modifiedPC2

return []

runTests :: IO Bool
runTests = $quickCheckAll

----------------------------------------------------------------------------- -}

-- QuickCheck MOVE ------------------------------------------------------------

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
