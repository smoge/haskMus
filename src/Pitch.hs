{-# LANGUAGE DeriveLift             #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}


module Pitch (
  HasNoteName(..),
  HasAccidental(..),
  Updatable(..),
  addOctave,
  incrementOctave,
  decrementOctave,
  modifyListWithComprehension,
  modifyOctavesWithGuards
) where

import           Pitch.Accidental
import           Pitch.Pitch
import qualified Pitch.Pitch      as P
import           Pitch.PitchClass
import qualified Pitch.PitchClass as PC

-- Type class for types that have a NoteName
class HasNoteName a where
  getNoteName :: a -> NoteName
  setNoteName :: NoteName -> a -> a

instance HasNoteName Pitch where
  getNoteName = P.noteName
  setNoteName newName p = p { P.noteName = newName }

instance HasNoteName PitchClass where
  getNoteName = PC.noteName
  setNoteName newName pc = pc { PC.noteName = newName }

class HasAccidental a where
  getAccidental :: a -> Accidental
  setAccidental :: Accidental -> a -> a

instance HasAccidental Pitch where
  getAccidental = P.accidental
  setAccidental newAcc p = p { P.accidental = newAcc }

instance HasAccidental PitchClass where
  getAccidental = PC.accidental
  setAccidental newAcc pc = pc { PC.accidental = newAcc }

type family IsList a where
  IsList [x] = True
  IsList x   = False

class Updatable b a where
  (=:) :: b -> a -> a

instance (HasNoteName a, IsList a ~ False) => Updatable NoteName a where
  (=:) = setNoteName

instance (HasAccidental a, IsList a ~ False) => Updatable Accidental a where
  (=:) = setAccidental

instance (Updatable b a, IsList a ~ True) => Updatable b [a] where
  (=:) x = fmap (x =:)


-- Define a custom infix operator to update a list of Pitches
--infixl 4 |=
--(|=) :: Accidental -> [Pitch] -> [Pitch]
--acc |= ps = ps & each . accidental .~ acc


addOctave :: Pitch -> Int -> Pitch
addOctave pitch delta = pitch { octave = Octave (pitch.octave.unOctave + delta) }

incrementOctave :: Pitch -> Pitch
incrementOctave pitch = pitch { octave = Octave (pitch.octave.unOctave + 1) }

decrementOctave :: Pitch -> Pitch
decrementOctave pitch = pitch { octave = Octave (pitch.octave.unOctave - 1) }

modifyListWithComprehension :: [Pitch] -> [Pitch]
modifyListWithComprehension pitches = [if p.noteName == D then addOctave p 1 else p | p <- pitches]

modifyPitch :: Pitch -> Pitch
modifyPitch p@(Pitch C _ _)    = addOctave p 1  -- Only modify if noteName is C
modifyPitch p@(Pitch _ Flat _) = addOctave p (-1) -- Decrease octave if accidental is Flat
modifyPitch p                  = p  -- Leave other pitches unchanged

modifyOctavesWithGuards :: [Pitch] -> [Pitch]
modifyOctavesWithGuards = fmap modifyPitch

