module Pitch.Pitch where

import Control.Applicative (liftA2)
import Control.Lens hiding (elements)
import Data.Fixed (mod')
import Data.Ratio ((%))
import Data.String
import Pitch.Accidental
import Data.Maybe (fromMaybe)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, elements)
import Util.Fraction (splitFraction)

data NoteName = C | D | E | F | G | A | B
  deriving (Eq, Ord, Show, Enum, Bounded)

class NoteClass (noteName :: NoteName) where
  sayNote :: String

class IsNoteName a where
  toNoteName :: a -> NoteName

data SomeNote = forall notename. (IsNoteName notename) => SomeNote notename

instance IsNoteName SomeNote where
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
  fromString s = error $ "Invalid NoteName string: " ++ s

data IntervalBasis = Chromatic | Diatonic
  deriving (Eq, Ord, Show, Enum)

data PitchClass where
  PitchClass ::
    { _noteName :: NoteName,
      _accidental :: Accidental
    } ->
    PitchClass

instance Show PitchClass where
  show (PitchClass name acc) = show name ++ " " ++ show acc

newtype Octave = Octave {getOctaves :: Int}
  deriving (Eq, Ord)

instance Show Octave where
  show (Octave o) = "Octave " ++ show o

data Pitch where
  Pitch ::
    { _noteName :: NoteName,
      _accidental :: Accidental,
      _octave :: Octave
    } ->
    Pitch

instance Show Pitch where
  show :: Pitch -> String
  show (Pitch name acc oct) = show name ++ " " ++ show acc ++ " " ++ show oct

makeLensesFor
  [ ("PitchClass", "_noteName"),
    ("PitchClass", "_accidental"),
    ("Pitch", "_noteName"),
    ("Pitch", "_accidental"),
    ("Pitch", "_octave")
  ]
  ''PitchClass

-- | Converts a `PitchClass` to a `Rational` value.
pcToRational :: PitchClass -> Rational
pcToRational pc = base + acVal
  where
    base = case Prelude.lookup nm noteNameToRational' of
      Just val -> val
      Nothing -> error "NoteName not found"
    acVal = accidentalToSemitones ac :: Rational
    nm = pc ^. noteName
    ac = pc ^. accidental

-- | Checks if two `PitchClass` values are enharmonic equivalents.
--
-- >>> PitchClass C Sharp =~ PitchClass D Flat 
-- True
(=~) :: PitchClass -> PitchClass -> Bool
pc1 =~ pc2 = (pcToRational pc1 `mod'` 12) == (pcToRational pc2 `mod'` 12)

-- | Lookup table for converting `NoteName` to `Rational` values.
noteNameToRational' :: [(NoteName, Rational)]
noteNameToRational' = [(C, 0), (D, 2), (E, 4), (F, 5), (G, 7), (A, 9), (B, 11)]

-- | Converts a `NoteName` to a `Rational` value.
noteNameToRational :: NoteName -> Rational
noteNameToRational name = case Prelude.lookup name noteNameToRational' of
  Just val -> val
  Nothing -> error ("NoteName " ++ show name ++ " not found")

-- | List of all possible `PitchClass` values.
allPitchClasses :: [PitchClass]
allPitchClasses = liftA2 PitchClass [C, D, E, F, G, A, B] allAccidentals

-- | List of all `PitchClass` values converted to `Rational`.
allPCRationals :: [Rational]
allPCRationals = map pcToRational allPitchClasses

-- | Returns a list of enharmonic equivalents for a given `Rational` value.
--
-- >>> enharmonicPCEquivs (3%2)
-- [(3 % 2,C ThreeQuartersSharp),(3 % 2,D QuarterFlat)]
enharmonicPCEquivs :: Rational -> [(Rational, PitchClass)]
enharmonicPCEquivs val =
  [(v, pc) | pc <- liftA2 PitchClass [C, D, E, F, G, A, B] allAccidentals, let v = pcToRational pc, v `mod'` 12 == val `mod'` 12]

-- | Returns a list of enharmonic equivalents for a given `PitchClass` value.
--
-- >>> enharmonicPCEquivs (PitchClass C Natural)
--
enharmonicPCEquivs' :: PitchClass -> [(Rational, PitchClass)]
enharmonicPCEquivs' pc =
  [(v, pc') | pc' <- liftA2 PitchClass [C, D, E, F, G, A, B] allAccidentals, let v = pcToRational pc', v `mod'` 12 == pcToRational pc `mod'` 12]

-- | Type alias for the mapping of `Rational` values to lists of `PitchClass` values.
type EnharmonicMapping = [(Rational, [PitchClass])]

-- | Creates an enharmonic mapping for a list of `Rational` values.
enharmonicMapping :: [Rational] -> EnharmonicMapping
enharmonicMapping = map (\r -> (r, snd <$> enharmonicPCEquivs r))



{- | Using the 'enharmonicMapping' function, this utility generates a list of
all enharmonic representations for a given 'PitchClass'. If no enharmonic
equivalents are found, it returns a list containing the original 'PitchClass'.

For instance, for a 'PitchClass' corresponding to C Sharp:

enharmonics (PitchClass C Sharp) 
-- [C Sharp,D Flat,B DoubleSharp]

-}
enharmonics :: PitchClass -> [PitchClass]
enharmonics pc = fromMaybe [pc] (lookup (pcToRational pc) out)
    where
        out = enharmonicMapping [pcToRational pc]

allEnharmonics :: [[PitchClass]]
allEnharmonics = map enharmonics allPitchClasses

allEnharmonicsMapping :: [(PitchClass, [PitchClass])]
allEnharmonicsMapping = zip allPitchClasses allEnharmonics


{- -----------------------------------  ----------------------------------------------------------

----------------------------------------------------------------------------------------------------- -} 


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

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

-- note1 :: NoteName
-- note1 =  "c"

-- ex1 :: (String, String, String)
-- ex1 = (sayNote @C, sayNote @D, sayNote  @E)

-- list = C :+ D

{-ghci> 

:kind C
C :: NoteName

:kind! C'
C :: NoteName
= C
  -}

{-
c = PitchClass C Natural
c ^. noteName
c ^. accidental
C
Natural

-- Changes the accidental of 'c' to Sharp
>>> c & accidental .~ Sharp
C Sharp

>>> c = PitchClass C Natural
>>> c & accidental %~ (\x -> addAccidental x (1%2))
C QuarterSharp

pitchClasses = map (\x -> PitchClass x Natural) [C .. B]
-- Changes the accidental of every PitchClass in the list to Flat
pitchClasses & each . accidental .~ Flat
[C Flat,D Flat,E Flat,F Flat,G Flat,A Flat,B Flat]

-- Checks if 'c' has an accidental of Natural
has (accidental . only Natural) c
--True

-- If the accidental is Natural, change it to Flat.
c & accidental . filtered (== Natural) .~ Flat
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

------------------------------
-------- ===TESTS=== ---------
------------------------------

instance Arbitrary NoteName where
  arbitrary = elements [C, D, E, F, G, A, B]

instance Arbitrary PitchClass where
  arbitrary = PitchClass <$> arbitrary <*> arbitrary

instance Arbitrary Octave where
  arbitrary :: Gen Octave
  arbitrary = Octave <$> arbitrary

instance Arbitrary Pitch where
  arbitrary = Pitch <$> arbitrary <*> arbitrary <*> arbitrary
