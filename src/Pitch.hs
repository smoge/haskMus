{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveLift             #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedRecordDot    #-}

{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TypeApplications #-}

-- Needed for lens operations
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Pitch where

import           Pitch.Accidental
import           Pitch.Interval
import           Pitch.LilyPitch
import           Pitch.Parser
import           Pitch.Pitch
import           Pitch.PitchClass
import qualified Pitch.PitchClass as PC
import           Pitch.PitchLike
import           Pitch.QuasiQuoter
import qualified Pitch.Pitch as P
import Control.Lens


mkPitchClass :: NoteName -> Accidental -> PitchClass
mkPitchClass n a = PitchClass { PC.noteName = n, PC.accidental = a }

mkPitch :: NoteName -> Accidental -> Octave -> Pitch
mkPitch n acc o = Pitch { P.noteName = n, P.accidental = acc, P.octave = o }

updatePCNoteName :: NoteName -> PitchClass -> PitchClass
updatePCNoteName n pc = pc { PC.noteName = n }

updatePNoteName :: NoteName -> Pitch -> Pitch
updatePNoteName n p = p { P.noteName = n }

class HasNoteName a where
  getNoteName :: a -> NoteName
  setNoteName :: NoteName -> a -> a

instance HasNoteName PitchClass where
  getNoteName = PC.noteName
  setNoteName newName pc = pc { PC.noteName = newName }

instance HasNoteName Pitch where
  getNoteName = P.noteName
  setNoteName :: NoteName -> Pitch -> Pitch
  setNoteName newName p = p { P.noteName = newName }

updateNoteName :: (HasNoteName a) => NoteName -> a -> a
updateNoteName = setNoteName

{- ---------------------------- playground -----------------------------------

c = PitchClass C Natural

updatePCNoteName D c  -- This will work without type application

updateNoteName @PitchClass D c  -- This will work with type application

c = PitchClass C Natural
c :: PitchClass

updateNoteNamePC @PitchClass D c

updateNoteName @Pitch D p


c.noteName

c.accidental

c { noteName = D } :: PitchClass

c4 = Pitch C Natural (Octave 4)


c ^. noteName

c ^. accidental

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

-- In Pitch.PitchClass module
updatePCNoteName :: NoteName -> PitchClass -> PitchClass
updatePCNoteName newName pc = pc { noteName = newName }

-- Usage
updatePCNoteName D c

-}
