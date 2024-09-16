{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveLift             #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedRecordDot    #-}
{-# LANGUAGE TemplateHaskell        #-}

module Pitch.PitchClass where

import           Control.Lens               hiding (elements)
import           Data.Data                  (Data)
import           Data.Fixed                 (mod')
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe)
import           Data.Ratio                 ((%))
import           Data.String                (IsString (..))
import           Language.Haskell.TH.Syntax
import           Pitch.Accidental

data NoteName = C | D | E | F | G | A | B
  deriving (Eq, Ord, Show, Enum, Bounded, Lift, Data)

data PitchClass = PitchClass
  { noteName   :: NoteName
  , accidental :: Accidental
  } deriving (Eq, Lift, Data, Ord)

instance Show PitchClass where
  show (PitchClass name acc) = show name <> " " <> show acc

instance IsString NoteName where
  fromString s = case s of
    "c" -> C
    "d" -> D
    "e" -> E
    "f" -> F
    "g" -> G
    "a" -> A
    "b" -> B
    _   -> error $ "Invalid NoteName string: " <> s

makeFields ''PitchClass

pcToRational :: PitchClass -> Rational
pcToRational pitchclass = base + acVal
  where
    base = fromMaybe (error "NoteName not found") $ Map.lookup pitchclass.noteName noteNameToRationalMap
    acVal = accidentalToSemitones pitchclass.accidental

(=~) :: PitchClass -> PitchClass -> Bool
pc1 =~ pc2 = (pcToRational pc1 `mod'` 12) == (pcToRational pc2 `mod'` 12)

noteNameToRationalMap :: Map.Map NoteName Rational
noteNameToRationalMap = Map.fromList [(C, 0), (D, 2), (E, 4), (F, 5), (G, 7), (A, 9), (B, 11)]

noteNameToRational :: NoteName -> Rational
noteNameToRational = (noteNameToRationalMap Map.!)

-- Enharmonic equivalence
allPitchClasses :: [PitchClass]
allPitchClasses = PitchClass <$> [C .. B] <*> allAccidentals

allPCRationals :: [Rational]
allPCRationals = pcToRational <$> allPitchClasses

enharmonicPCEquivs :: Rational -> [(Rational, PitchClass)]
enharmonicPCEquivs val = filter (\(v, _) -> v `mod'` 12 == val `mod'` 12) $ zip (pcToRational <$> allPitchClasses) allPitchClasses

enharmonicPCEquivs' :: PitchClass -> [(Rational, PitchClass)]
enharmonicPCEquivs' = enharmonicPCEquivs . pcToRational

type EnharmonicMapping = [(Rational, [PitchClass])]

enharmonicMapping :: [Rational] -> EnharmonicMapping
enharmonicMapping = fmap (\r -> (r, snd <$> enharmonicPCEquivs r))

enharmonics :: PitchClass -> [PitchClass]
enharmonics pc = fromMaybe [pc] $ lookup (pcToRational pc) $ enharmonicMapping allPCRationals

allEnharmonics :: [[PitchClass]]
allEnharmonics = enharmonics <$> allPitchClasses

allEnharmonicsMapping :: [(PitchClass, [PitchClass])]
allEnharmonicsMapping = zip allPitchClasses allEnharmonics
