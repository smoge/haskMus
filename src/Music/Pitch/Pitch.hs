{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Music.Pitch.Pitch where

import Control.Lens hiding (elements)
import Data.Ratio ((%))
import Music.Pitch.Accidental
import Data.String
import qualified Data.Text as T
import Test.QuickCheck

-- | Represents a note name (C, D, E, etc.).
data NoteName = C | D | E | F | G | A | B
  deriving (Eq, Ord, Show, Enum, Bounded)

instance IsString NoteName where
  fromString :: String -> NoteName
  fromString "c" = C
  fromString "d" = D
  fromString "e" = E
  fromString "f" = F
  fromString "g" = G
  fromString "a" = A
  fromString "b" = B
  fromString s = error $ "Invalid NoteName string: " ++ s

-- |
-- This type represents standard basis for intervals.
data IntervalBasis = Chromatic | Diatonic
  deriving (Eq, Ord, Show, Enum)

-- | Represents a pitch class (C, C#, D, D#, etc.).
data PitchClass where
  PitchClass ::
    { _noteName :: NoteName,
      -- | ^ The note name (C, D, E, etc.).
      _accidental :: Accidental
    } ->
    -- \^ The accidental for the note.
    PitchClass
  deriving (Eq, Ord)

makeLenses ''PitchClass

instance IsString PitchClass where
  fromString :: String -> PitchClass
  fromString "C" = PitchClass C Natural
  fromString "c" = PitchClass C Natural
  fromString "C#" = PitchClass C Sharp
  fromString "c#" = PitchClass C Sharp
  fromString "D" = PitchClass D Natural
  fromString "d" = PitchClass D Natural
  fromString "D#" = PitchClass D Sharp
  fromString "d#" = PitchClass D Sharp
  fromString "E" = PitchClass E Natural
  fromString "e" = PitchClass E Natural
  fromString "E#" = PitchClass E Sharp
  fromString "e#" = PitchClass E Sharp
  fromString "F" = PitchClass F Natural
  fromString "f" = PitchClass F Natural
  fromString "F#" = PitchClass F Sharp
  fromString "f#" = PitchClass F Sharp
  fromString "G" = PitchClass G Natural
  fromString "g" = PitchClass G Natural
  fromString "G#" = PitchClass G Sharp
  fromString "g#" = PitchClass G Sharp
  fromString "A" = PitchClass A Natural
  fromString "a" = PitchClass A Natural
  fromString "A#" = PitchClass A Sharp
  fromString "a#" = PitchClass A Sharp
  fromString "B" = PitchClass B Natural
  fromString "b" = PitchClass B Natural
  fromString s = error $ "Invalid PitchClass string: " ++ s

instance Show PitchClass where
  show :: PitchClass -> String
  show (PitchClass name acc) = show name ++ " " ++ show acc

-- | Represents an octave.
newtype Octave = Octave {getOctaves :: Int}
  deriving (Eq, Ord)

instance Show Octave where
  show (Octave o) = "Octave " ++ show o

data Pitch where
  Pitch ::
    { -- | The note name (C, D, E, etc.).
      _noteName :: NoteName,
      -- | The accidental for the note.
      _accidental :: Accidental,
      _octave :: Octave
    } ->
    Pitch
  deriving (Eq, Ord)

instance Show Pitch where
  show :: Pitch -> String
  show (Pitch name acc oct) = show name ++ " " ++ show acc ++ " " ++ show oct

class HasNoteName a where
  noteName :: Lens' a NoteName

class HasAccidental a where
  accidental :: Lens' a Accidental

class HasPitchClass a where
  pitchClass :: Lens' a PitchClass

class HasOctave a where
  octave :: Lens' a Octave

instance HasNoteName Pitch where
  noteName f (Pitch nn acc o) = (\nn' -> Pitch nn' acc o) <$> f nn

instance HasAccidental Pitch where
  accidental f (Pitch nn acc o) = (\acc' -> Pitch nn acc' o) <$> f acc

instance HasPitchClass Pitch where
  pitchClass :: Lens' Pitch PitchClass
  pitchClass f (Pitch nn acc o) = (\(PitchClass nn' acc') -> Pitch nn' acc' o) <$> f (PitchClass nn acc)

instance HasNoteName PitchClass where
  noteName f (PitchClass nn acc) = (`PitchClass` acc) <$> f nn

instance HasAccidental PitchClass where
  accidental f (PitchClass nn acc) = PitchClass nn <$> f acc

instance HasAccidental Accidental where
  accidental = id

instance HasOctave Octave where
  octave = id

instance HasOctave Pitch where
  octave :: Lens' Pitch Octave
  octave = lens _octave (\(Pitch nn acc _) o -> Pitch nn acc o)

{-
>>> c = PitchClass C Natural
>>> c ^. noteName
>>> c ^. accidental
C
Natural

-- Changes the accidental of 'c' to Sharp
>>> c & accidental .~ Sharp
C Sharp

>>> c = PitchClass C Natural
>>> c & accidental %~ (\x -> addAccidental x (1%2))
C QuarterSharp

>>> pitchClasses = map (\x -> PitchClass x Natural) [C .. B]
-- Changes the accidental of every PitchClass in the list to Flat
>>> pitchClasses & each . accidental .~ Flat
[C Flat,D Flat,E Flat,F Flat,G Flat,A Flat,B Flat]

-- Checks if 'c' has an accidental of Natural
>>> has (accidental . only Natural) c
True

-- If the accidental is Natural, change it to Flat.
>>> c & accidental . filtered (== Natural) .~ Flat
C Flat

-}

{-

>>> p = Pitch C Natural (Octave 4)
>>> p ^. noteName
C
>>> p ^. accidental
Natural
>>> p ^. octave
Octave 4

>>> p & accidental .~ Sharp  -- Changes the accidental of 'p' to Sharp
C Sharp Octave 4

>>> p & accidental %~ (\x -> addAccidental x (1%2))
C QuarterSharp Octave 4

>>> pitches = map (\x -> Pitch x Natural (Octave 4)) [C .. B]
>>> pitches & each . accidental .~ Flat  -- Changes the accidental of every Pitch in the list to Flat
[C Flat Octave 4,D Flat Octave 4,E Flat Octave 4,F Flat Octave 4,G Flat Octave 4,A Flat Octave 4,B Flat Octave 4]

>>> has (accidental . only Natural) p  -- Checks if 'p' has an accidental of Natural
True

>>> p & accidental . filtered (== Natural) .~ Flat  -- If the accidental is Natural, change it to Flat.
C Flat Octave 4

>>> p & octave .~ Octave 5  -- Change the octave of 'p' to 5
C Natural Octave 5

>>> p & octave %~ (\(Octave o) -> Octave (o + 1))  -- Increment the octave by 1
C Natural Octave 5
-}

---------------------------------------------------------------------------------
-- QuickCheck
---------------------------------------------------------------------------------

instance Arbitrary NoteName where
  arbitrary = elements [C, D, E, F, G, A, B]

instance Arbitrary PitchClass where
  arbitrary = PitchClass <$> arbitrary <*> arbitrary

instance Arbitrary Octave where
  arbitrary :: Gen Octave
  arbitrary = Octave <$> arbitrary

instance Arbitrary Pitch where
  arbitrary = Pitch <$> arbitrary <*> arbitrary <*> arbitrary


  
  
