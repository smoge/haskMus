{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Music.Pitch.Pitch where

import Control.Lens hiding (elements)
import Music.Pitch.Accidental
-- import Data.Ratio ((%))

import Test.QuickCheck

-- | Represents a note name (C, D, E, etc.).
data NoteName = C | D | E | F | G | A | B
  deriving (Eq, Ord, Show, Enum, Bounded)

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

class HasPitchClass t where
  pitchClass :: Lens' t PitchClass
  noteName :: Lens' t NoteName
  accidental :: Lens' t Accidental

-- class HasPitchClass t where
--     noteName :: Lens' t NoteName
--     accidental :: Lens' t Accidental

instance HasPitchClass Pitch where
  pitchClass :: Lens' Pitch PitchClass
  pitchClass f (Pitch nn acc o) = (\(PitchClass nn' acc') -> Pitch nn' acc' o) <$> f (PitchClass nn acc)

  noteName :: Lens' Pitch NoteName
  noteName f (Pitch nn acc o) = (\nn' -> Pitch nn' acc o) <$> f nn

  accidental :: Lens' Pitch Accidental
  accidental f (Pitch nn acc o) = (\acc' -> Pitch nn acc' o) <$> f acc

instance HasPitchClass PitchClass where
  pitchClass :: Lens' PitchClass PitchClass
  pitchClass = id
  noteName :: Lens' PitchClass NoteName
  noteName f (PitchClass nn acc) = (`PitchClass` acc) <$> f nn

  -- noteName = lens noteName (\(PitchClass _ acc) nn -> PitchClass nn acc)
  accidental :: Lens' PitchClass Accidental
  accidental f (PitchClass nn acc) = PitchClass nn <$> f acc

-- ! FIXME

instance HasPitchClass Accidental where
  accidental :: Lens' Accidental Accidental
  accidental = id

  pitchClass :: Lens' Accidental PitchClass
  pitchClass = lens getPitchClass setPitchClass
    where
      getPitchClass :: Accidental -> PitchClass
      getPitchClass = PitchClass C

      setPitchClass :: Accidental -> PitchClass -> Accidental
      setPitchClass _ pc = pc ^. accidental

class HasAccidental a where
  accidentalLens :: Lens' a Accidental

instance HasAccidental Pitch where
  accidentalLens = lens getAccidental setAccidental
    where
      getAccidental :: Pitch -> Accidental
      getAccidental (Pitch _ acc _) = acc

      setAccidental :: Pitch -> Accidental -> Pitch
      setAccidental (Pitch nn _ o) acc' = Pitch nn acc' o

instance HasAccidental PitchClass where
  accidentalLens = lens getAccidental setAccidental
    where
      getAccidental :: PitchClass -> Accidental
      getAccidental (PitchClass _ acc) = acc

      setAccidental :: PitchClass -> Accidental -> PitchClass
      setAccidental (PitchClass nn _) = PitchClass nn

octave :: Lens' Pitch Octave
octave = lens _octave (\(Pitch nn acc _) o -> Pitch nn acc o)

{-

// ALTERNATIVE

class HasNoteName a where
    noteName :: Lens' a NoteName

class HasAccidental a where
    accidental :: Lens' a Accidental

class HasPitchClass a where
    pitchClass :: Lens' a PitchClass

instance HasNoteName Pitch where
    noteName f (Pitch nn acc o) = (\nn' -> Pitch nn' acc o) <$> f nn

instance HasAccidental Pitch where
    accidental f (Pitch nn acc o) = (\acc' -> Pitch nn acc' o) <$> f acc

instance HasPitchClass Pitch where
    pitchClass f (Pitch nn acc o) = (\(PitchClass nn' acc') -> Pitch nn' acc' o) <$> f (PitchClass nn acc)

instance HasNoteName PitchClass where
    noteName f (PitchClass nn acc) = (`PitchClass` acc) <$> f nn

instance HasAccidental PitchClass where
    accidental f (PitchClass nn acc) = PitchClass nn <$> f acc

 -}

{-
>>> c = PitchClass C Natural
>>> c ^. noteName
>>> c ^. accidental
C
Natural

>>> c & accidental .~ Sharp  -- Changes the accidental of 'c' to Sharp
C Sharp

>>> c & accidental %~ (\x -> addAccidental x (1%2))
C QuarterSharp

>>> pitchClasses = map (\x -> PitchClass x Natural) [C .. B]
>>> pitchClasses & each . accidental .~ Flat  -- Changes the accidental of every PitchClass in the list to Flat
[C Flat,D Flat,E Flat,F Flat,G Flat,A Flat,B Flat]

>>> has (accidental . only Natural) c  -- Checks if 'c' has an accidental of Natural
True

>>> c & accidental . filtered (== Natural) .~ Flat  -- If the accidental is Natural, change it to Flat.
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
-- Music.Pitch.Pitch

instance Arbitrary NoteName where
  arbitrary = elements [C, D, E, F, G, A, B]

instance Arbitrary PitchClass where
  arbitrary = PitchClass <$> arbitrary <*> arbitrary

instance Arbitrary Octave where
  arbitrary :: Gen Octave
  arbitrary = Octave <$> arbitrary

instance Arbitrary Pitch where
  arbitrary = Pitch <$> arbitrary <*> arbitrary <*> arbitrary
