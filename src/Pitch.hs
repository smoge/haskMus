{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveLift             #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedRecordDot    #-}
-- Needed for lens operations
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

{-# LANGUAGE OverloadedRecordDot #-}

module Pitch where

import           Pitch.Accidental
import           Pitch.Interval
import           Pitch.LilyPitch
import           Pitch.Parser
import           Pitch.Pitch
import           Pitch.PitchClass
import           Pitch.PitchLike
import           Pitch.QuasiQuoter
import qualified Pitch.Pitch as P
import Control.Lens

{- ---------------------------- playground -----------------------------------

c = PitchClass C Natural
c ^. noteName
c ^. accidental

c.noteName

c.accidental


c4 = Pitch C Natural (Octave 4)

pitchToRational c4

splitFraction $ pitchToRational $  Pitch G QuarterSharp (Octave 5)

fromRational $ pitchToRational $  Pitch E QuarterSharp (Octave 3)

-- Changes the accidental of 'c' to Sharp

c4 & Pitch.Pitch.accidental .~ Sharp

import Control.Lens

c4 & accidental_ %~ (\x -> addAccidental x (1%2))


--C Sharp

c & accidental %~ (\x -> addAccidental x (1%2))
-- C QuarterSharp

pitchClasses = map (\x -> PitchClass x Natural) [C .. B]

-- Changes the accidental of every PitchClass in the list to Flat

pitchClasses & each . accidental .~ Flat
--[C Flat,D Flat,E Flat,F Flat,G Flat,A Flat,B Flat]

-- Checks if 'c' has an accidental ojustf Natural
has (accidental . only Natural) c
--True

-- If the accidental is Natural, change it to Flat.
c & accidental . filtered (== Natural) .~ Flat
C Flat

p = Pitch C Natural (Octave 4)

p ^. noteName
-- C

p ^. accidental
-- Natural

p ^. octave
-- Octave 4

p & accidental .~ Sharp  -- Changes the accidental of 'p' to Sharp
-- C Sharp Octave 4

p & accidental %~ (\x -> addAccidental x (1%2))
-- C QuarterSharp Octave 4

pitches = map (\x -> Pitch x Natural (Octave 4)) [C .. B]
pitches & each . accidental .~ Flat  -- Changes the accidental of every Pitch in the list to Flat

-- [C Flat Octave 4,D Flat Octave 4,E Flat Octave 4,F Flat Octave 4,G Flat Octave 4,A Flat Octave 4,B Flat Octave 4]

has (accidental . only Natural) p  -- Checks if 'p' has an accidental of Natural

-- True

p & accidental . filtered (== Natural) .~ Flat  -- If the accidental is Natural, change it to Flat.

-- C Flat Octave 4

p & octave .~ Octave 5  -- Change the octave of 'p' to 5

-- C Natural Octave 5

p & octave %~ (\(Octave o) -> Octave (o + 1))  -- Increment the octave by 1

-- C Natural Octave 5

--------------------------------------------------------------------------------

-}
