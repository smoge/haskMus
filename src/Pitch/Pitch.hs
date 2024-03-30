{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | The Pitch module defines core types for representing musical pitch:
--
-- * 'NoteName' - The letter names of the musical scale (C, D, E etc.)
-- * 'Accidental' - Alterations to the pitch like sharps/flats
-- * 'PitchClass' - Combination of a 'NoteName' and 'Accidental'
-- * 'Octave' - The octave number
-- * 'Pitch' - Combination of 'PitchClass' and 'Octave'
--
-- This module also provides:
--
-- * Lenses for accessing the components of pitch-related types
-- * Typeclass instances for common pitch operations
-- * Utility functions for converting between pitch representations
module Pitch.Pitch where

import Control.Applicative
import Control.Lens hiding (elements)
-- import Test.QuickCheck (Arbitrary (arbitrary), Gen, elements)

import Control.Monad (forM)
import Data.Char (toLower)
import Data.Data
import Data.Fixed (mod')
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.String
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Pitch.Accidental
import Util.Fraction (splitFraction)

data NoteName = C | D | E | F | G | A | B
  deriving (Eq, Ord, Show, Enum, Bounded, Lift, Data)

data IntervalBasis = Chromatic | Diatonic
  deriving (Eq, Ord, Show, Enum)

data PitchClass where
  PitchClass ::
    { _noteName :: NoteName,
      _accidental :: Accidental
    } ->
    PitchClass
  deriving (Eq, Lift, Data)

data Pitch where
  Pitch ::
    { _noteName :: NoteName,
      _accidental :: Accidental,
      _octave :: Octave
    } ->
    Pitch
  deriving (Eq, Lift, Data)

newtype Octave = Octave {unOctave :: Int}
  deriving (Eq, Ord, Lift, Data)

-- deriving instance Data Pitch

mkPitch :: NoteName -> Accidental -> Octave -> Pitch
mkPitch = Pitch

mkPitch' :: PitchClass -> Octave -> Pitch
mkPitch' pc = Pitch (pc ^. noteName) (pc ^. accidental)

data SomeNote = forall notename. (IsNoteName notename) => SomeNote notename

-------------------------------------------------------------------------------------
--  Type classes
-------------------------------------------------------------------------------------

class NoteClass (noteName :: NoteName) where
  sayNote :: String

class IsNoteName a where
  toNoteName :: a -> NoteName

class HasNoteName a where
  noteName :: Lens' a NoteName

class HasAccidental a where
  accidental :: Lens' a Accidental

class HasPitchClass a where
  pitchClass :: Lens' a PitchClass

class HasOctave a where
  octave :: Lens' a Octave

-------------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------------

-- | Typeclass instance for retrieving the note name of a Pitch.
instance HasNoteName Pitch where
  -- \| Extracts the note name from a Pitch and applies a function to it.
  noteName f (Pitch nn acc o) = (\nn' -> Pitch nn' acc o) <$> f nn

instance HasOctave Pitch where
  octave f (Pitch nn acc o) = (Pitch nn acc) <$> f o

-- | Typeclass that represents a type with an accidental.
instance HasAccidental Pitch where
  -- \| Modifies the accidental of a Pitch using the provided function.
  accidental f (Pitch nn acc o) = (\acc' -> Pitch nn acc' o) <$> f acc

-- | Typeclass that represents a type with a pitch class.
instance HasPitchClass Pitch where
  -- \| Lens that focuses on the pitch class of a Pitch.
  pitchClass :: Lens' Pitch PitchClass
  pitchClass f (Pitch nn acc o) = (\(PitchClass nn' acc') -> Pitch nn' acc' o) <$> f (PitchClass nn acc)

-- | Typeclass that represents a type with a note name.
instance HasNoteName PitchClass where
  -- \| Modifies the note name of a PitchClass using the provided function.
  noteName f (PitchClass nn acc) = (`PitchClass` acc) <$> f nn

-- | Typeclass that represents a type with an accidental.
instance HasAccidental PitchClass where
  -- \| Modifies the accidental of a PitchClass using the provided function.
  accidental f (PitchClass nn acc) = PitchClass nn <$> f acc

-- | Typeclass that represents a type that can be converted to a NoteName.
instance IsNoteName SomeNote where
  -- \| Converts a SomeNote to a NoteName.
  toNoteName :: SomeNote -> NoteName
  toNoteName (SomeNote nn) = toNoteName nn

instance Show SomeNote where
  show = show . toNoteName

instance NoteClass C where
  sayNote = "c"

instance NoteClass D where
  sayNote = "d"

instance NoteClass E where
  sayNote = "e"

instance NoteClass F where
  sayNote = "f"

instance NoteClass G where
  sayNote = "g"

instance NoteClass A where
  sayNote = "a"

instance NoteClass B where
  sayNote = "b"

instance IsString NoteName where
  fromString :: String -> NoteName
  fromString "c" = C
  fromString "d" = D
  fromString "e" = E
  fromString "f" = F
  fromString "g" = G
  fromString "a" = A
  fromString "b" = B
  fromString s = error $ "Invalid NoteName string: " <> s

instance Show PitchClass where
  show (PitchClass name acc) = show name <> " " <> show acc

instance Show Octave where
  show (Octave o) = "Octave " <> show o

instance Show Pitch where
  show :: Pitch -> String
  show (Pitch name acc oct) = show name <> " " <> show acc <> " " <> show oct

-- Functions
makeLensesFor
  [ ("PitchClass", "_noteName"),
    ("PitchClass", "_accidental"),
    ("Pitch", "_noteName"),
    ("Pitch", "_accidental"),
    ("Pitch", "_octave")
  ]
  ''PitchClass

pcToRational :: PitchClass -> Rational
pcToRational pc = base + acVal
  where
    base = case Prelude.lookup nm noteNameToRational' of
      Just val -> val
      Nothing -> error "NoteName not found"
    acVal = accidentalToSemitones ac :: Rational
    nm = pc ^. noteName
    ac = pc ^. accidental

(=~) :: PitchClass -> PitchClass -> Bool
pc1 =~ pc2 = (pcToRational pc1 `mod'` 12) == (pcToRational pc2 `mod'` 12)

noteNameToRationalMap :: Map.Map NoteName Rational
noteNameToRationalMap = Map.fromList [(C, 0), (D, 2), (E, 4), (F, 5), (G, 7), (A, 9), (B, 11)]



noteNameToRational :: NoteName -> Rational
noteNameToRational name = fromMaybe (error ("NoteName " <> show name <> " not found")) (Map.lookup name noteNameToRationalMap)


noteNameToRational' :: [(NoteName, Rational)]
noteNameToRational' = [(C, 0), (D, 2), (E, 4), (F, 5), (G, 7), (A, 9), (B, 11)]

{-
noteNameToRational :: NoteName -> Rational
noteNameToRational name = case Prelude.lookup name noteNameToRational' of
  Just val -> val
  Nothing -> error ("NoteName " <> show name <> " not found")
 -}

pitchToRational :: Pitch -> Rational
pitchToRational (Pitch nm ac oct) = pcToRational (PitchClass nm ac) + fromIntegral (unOctave oct + 1) * 12

rationalToFloat :: Rational -> Float
rationalToFloat = fromRational

pitchToFloat :: Pitch -> Float
pitchToFloat = rationalToFloat . pitchToRational

allPitchClasses :: [PitchClass]
allPitchClasses = liftA2 PitchClass [C, D, E, F, G, A, B] allAccidentals

allPCRationals :: [Rational]
allPCRationals = fmap pcToRational allPitchClasses

enharmonicPCEquivs :: Rational -> [(Rational, PitchClass)]
enharmonicPCEquivs val =
  [(v, pc) | pc <- liftA2 PitchClass [C, D, E, F, G, A, B] allAccidentals, let v = pcToRational pc, v `mod'` 12 == val `mod'` 12]

enharmonicPCEquivs' :: PitchClass -> [(Rational, PitchClass)]
enharmonicPCEquivs' pc =
  [(v, pc') | pc' <- liftA2 PitchClass [C, D, E, F, G, A, B] allAccidentals, let v = pcToRational pc', v `mod'` 12 == pcToRational pc `mod'` 12]

type EnharmonicMapping = [(Rational, [PitchClass])]

enharmonicMapping :: [Rational] -> EnharmonicMapping
enharmonicMapping = fmap (\r -> (r, snd <$> enharmonicPCEquivs r))

enharmonics :: PitchClass -> [PitchClass]
enharmonics pc = fromMaybe [pc] (lookup (pcToRational pc) out)
  where
    out = enharmonicMapping [pcToRational pc]

allEnharmonics :: [[PitchClass]]
allEnharmonics = fmap enharmonics allPitchClasses

allEnharmonicsMapping :: [(PitchClass, [PitchClass])]
allEnharmonicsMapping = zip allPitchClasses allEnharmonics

{- ---------------------------- playground -----------------------------------

c = PitchClass C Natural
c ^. noteName
c ^. accidental

c4 = Pitch C Natural (Octave 4)

pitchToRational c4

splitFraction $ pitchToRational $  Pitch G QuarterSharp (Octave 5)

fromRational $ pitchToRational $  Pitch E QuarterSharp (Octave 3)

-- Changes the accidental of 'c' to Sharp
c & accidental .~ Sharp
--C Sharp

c & accidental %~ (\x -> addAccidental x (1%2))
-- C QuarterSharp

pitchClasses = map (\x -> PitchClass x Natural) [C .. B]

-- Changes the accidental of every PitchClass in the list to Flat

pitchClasses & each . accidental .~ Flat
--[C Flat,D Flat,E Flat,F Flat,G Flat,A Flat,B Flat]

-- Checks if 'c' has an accidental of Natural
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

-------------------------------------------------------------------------------- -}

-- -- !FIXME: MOVE TO TESTS

-- instance Arbitrary NoteName where
--   arbitrary = elements [C, D, E, F, G, A, B]

-- instance Arbitrary PitchClass where
--   arbitrary = PitchClass <$> arbitrary <*> arbitrary

-- instance Arbitrary Octave where
--   arbitrary :: Gen Octave
--   arbitrary = Octave <$> arbitrary

-- instance Arbitrary Pitch where
--   arbitrary = Pitch <$> arbitrary <*> arbitrary <*> arbitrary

-- -- PitchClass C Flat

notes :: [NoteName]
notes = [C, D, E, F, G, A, B]

-- Function to create a map of pitch names to pitch values
createPitchMap :: [NoteName] -> Map.Map String Pitch
createPitchMap = foldr (Map.union . createPitchesForNote) Map.empty

createPitchesForNote :: NoteName -> Map.Map String Pitch
createPitchesForNote note = Map.fromList $ do
  acc <- [Natural, Sharp, Flat, QuarterSharp, QuarterFlat, ThreeQuartersFlat, ThreeQuartersSharp, DoubleFlat, DoubleSharp]
  let modifier = case acc of
        Sharp -> "is"
        Flat -> "es"
        QuarterSharp -> "ih"
        QuarterFlat -> "eh"
        Natural -> ""
        ThreeQuartersFlat -> "eseh"
        ThreeQuartersSharp -> "isih"
        DoubleFlat -> "eses"
        DoubleSharp -> "isis"
        _ -> ""
  (octaveSuffix, oct) <- [("", 4), ("'", 5), ("''", 6), ("'''", 7), ("_", 3), ("__", 2), ("___", 1)]
  pure (fmap toLower (show note) <> modifier <> octaveSuffix, Pitch note acc (Octave oct))

-- Create pitch map
pitchMap :: Map.Map String Pitch
pitchMap = createPitchMap notes

concatForM :: (Monad m) => [a] -> (a -> m [b]) -> m [b]
concatForM xs action = concat <$> forM xs action

generatePitchVars :: [String] -> Q [Dec]
generatePitchVars pitchNames =
  concatForM pitchNames $ \name -> do
    let varName = mkName name
    let pitchVal = AppE (VarE 'fromString) (LitE (StringL name))
    pure [SigD varName (ConT ''Pitch), ValD (VarP varName) (NormalB pitchVal) []]
