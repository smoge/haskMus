module Pitch.Scale (
    Interval(..),
    Scale(..),
    Sieve(..),
    simpleSieve,
    unionSieve,
    intersectSieve,
    complementSieve,
    xenakisSieve,
    generateIntervals,
    scaleFromSieve,
    quarterToneScale,
    scaleToPitches
    ) where


import Data.List (intercalate)
import Data.Ratio
import Pitch.Accidental
import Pitch.Pitch
import Util.Fraction
import Data.Fixed (mod')

newtype Interval = Interval {getInterval :: Rational} deriving (Eq, Ord, Num)

instance Show Interval where
  show :: Interval -> String
  show (Interval i) = show i

data Scale = Scale
  { scaleName :: String,
    scaleIntervals :: [Interval],
    scaleMode :: Maybe Int
  }

instance Show Scale where
  show :: Scale -> String
  show (Scale name intervals mode) =
    name
      <> " ["
      <> intercalate "," (fmap (show . getInterval) intervals)
      <> "]"
      <> maybe "" (\m -> " (Mode: " <> show m <> ")") mode


newtype Sieve = Sieve {getSieve :: Interval -> Bool}

simpleSieve :: Interval -> Interval -> Sieve
simpleSieve modulus residue = Sieve (\n -> getInterval n `modRational` getInterval modulus == getInterval residue)
  where
    modRational x y = x `mod'` y

unionSieve :: Sieve -> Sieve -> Sieve
unionSieve (Sieve s1) (Sieve s2) = Sieve (\n -> s1 n || s2 n)

intersectSieve :: Sieve -> Sieve -> Sieve
intersectSieve (Sieve s1) (Sieve s2) = Sieve (\n -> s1 n && s2 n)

complementSieve :: Sieve -> Sieve
complementSieve (Sieve s) = Sieve (not . s)


xenakisSieve :: Sieve
xenakisSieve =
  unionSieve
    (simpleSieve (Interval (3 % 2)) (Interval 0))
    ( intersectSieve
        (simpleSieve (Interval (1 % 2)) (Interval 0))
        (simpleSieve (Interval (5 % 2)) (Interval 0))
    )

generateIntervals :: Interval -> Interval -> Interval -> [Interval]
generateIntervals start step end = takeWhile (<= end) $ iterate (+ step) start

scaleFromSieve :: String -> Sieve -> Interval -> Interval -> Scale
scaleFromSieve name sieve step range =
  Scale name (filter (getSieve sieve) $ generateIntervals (Interval 0) step range) Nothing

quarterToneScale :: Scale
quarterToneScale = scaleFromSieve "QuarterTone" xenakisSieve (Interval (1 % 2)) (Interval 12)

-- >>> quarterToneScale
-- QuarterTone [0 % 1,3 % 2,5 % 2,3 % 1,9 % 2,5 % 1,6 % 1,15 % 2,9 % 1,10 % 1,21 % 2,12 % 1]


scaleToPitches :: Scale -> Pitch -> [Pitch]
scaleToPitches (Scale _ intervals _) rootPitch =
  scanl addQuarterToneInterval rootPitch intervals
  where
    addQuarterToneInterval :: Pitch -> Interval -> Pitch
    addQuarterToneInterval pitch (Interval interval) =
      let quarterToneSteps = realToFrac interval * 2
       in iterateQuarterTone pitch quarterToneSteps

    iterateQuarterTone :: Pitch -> Double -> Pitch
    iterateQuarterTone pitch steps
      | steps >= 1 = iterateQuarterTone (modifyPitchQuarterTone succ pitch) (steps - 1)
      -- | steps > 0  = modifyPitchQuarterTone (\acc -> addQuarterTone acc steps) pitch
      | otherwise  = pitch



modifyPitchQuarterTone :: (Accidental -> Accidental) -> Pitch -> Pitch
modifyPitchQuarterTone f (Pitch noteName_ acc octave_) =
  let (semitonesCrossed, remainder) = splitFraction $ accidentalToSemitones (f acc) / 2

      newNoteName = case noteName_ of
                      B | semitonesCrossed > 0 -> C
                      _ -> iterate succ noteName_ !! fromInteger semitonesCrossed

      newOctave = if noteName_ == B && semitonesCrossed > 0 then succ octave_ else octave_

      (finalNoteName, newAcc) = preferredEnharmonic newNoteName (semitonesToAccidental (remainder * 2))
   in Pitch finalNoteName newAcc newOctave


preferredEnharmonic :: NoteName -> Accidental -> (NoteName, Accidental)
preferredEnharmonic A Sharp  = (B, Flat)
preferredEnharmonic G Flat   = (F, Sharp)
preferredEnharmonic E Sharp  = (F, Natural)
preferredEnharmonic B Sharp  = (C, Natural)
preferredEnharmonic G ThreeQuartersSharp = (A, QuarterSharp)
preferredEnharmonic F ThreeQuartersSharp = (G, QuarterSharp)
preferredEnharmonic D Sharp  = (E, Flat)
preferredEnharmonic note acc = (note, acc)


------------------------------------------------------------------------------------------------------------------------
-- ** Examples/Tests**
------------------------------------------------------------------------------------------------------------------------

majorScale :: Scale
majorScale = Scale "Major" [Interval 2, Interval 2, Interval 1, Interval 2, Interval 2, Interval 2, Interval 1] Nothing

minorScale :: Scale
minorScale = Scale "Minor" [Interval 2, Interval 1, Interval 2, Interval 2, Interval 1, Interval 2, Interval 2] Nothing


exampleScale :: Scale
exampleScale = Scale "Example" [Interval 2, Interval 2, Interval 1, Interval 2, Interval 2, Interval 2, Interval 1] Nothing

exampleQuarterToneScale :: Scale
exampleQuarterToneScale = Scale "Quarter-Tone Example" [Interval (1 % 2), Interval 1, Interval (1%4), Interval (3 % 2), Interval 1, Interval (1 %2), Interval (3 % 2)] Nothing

c4 :: Pitch
c4 = Pitch C Natural (Octave 4)

halfTonePitches :: [Pitch]
halfTonePitches = scaleToPitches exampleScale c4

quarterTonePitches :: [Pitch]
quarterTonePitches = scaleToPitches exampleQuarterToneScale c4


chromaticScale :: Scale
chromaticScale = Scale "Chromatic" (Interval <$> replicate 12 (1 % 1)) Nothing


chromaticPitches :: [Pitch]
chromaticPitches = scaleToPitches chromaticScale c4


-- >>> scaleToPitches   chromaticScale c4
-- [C Natural Octave 4,C Sharp Octave 4,D Natural Octave 4,E Flat Octave 4,E Natural Octave 4,F Natural Octave 4,F Sharp Octave 4,G Sharp Octave 4,B Flat Octave 4,B Natural Octave 4,C Natural Octave 4,C Sharp Octave 4,D Natural Octave 4]
