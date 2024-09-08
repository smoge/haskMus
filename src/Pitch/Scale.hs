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
    scaleToHalfTonePitches,
    scaleToQuarterTonePitches
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

-- quarterToneScale

scaleToHalfTonePitches :: Scale -> Pitch -> [Pitch]
scaleToHalfTonePitches (Scale _ intervals _) rootPitch =
  scanl addInterval rootPitch intervals
  where
    addInterval :: Pitch -> Interval -> Pitch
    addInterval pitch (Interval interval) =
      let semitoneSteps = realToFrac interval * 2 -- Convert fractional steps to semitones
       in case drop (round semitoneSteps) (iterate (modifyPitch succ) pitch) of
            x : _ -> x
            [] -> error "scaleToHalfTonePitches error"


modifyPitch :: (Accidental -> Accidental) -> Pitch -> Pitch
modifyPitch f (Pitch noteName_ acc o) =
  let (quotient, remainder) = splitFraction $ accidentalToSemitones (f acc)
      -- Only change note name if necessary based on the quotient of semitones
      newNoteName = if quotient > 0
                      then case noteName_ of
                             B -> C -- Wrap from B to C if crossing the boundary
                             _ -> succ noteName_
                      else noteName_ -- Stay on the same note if quotient is 0 or negative
      newAcc = semitonesToAccidental remainder
      -- Adjust octave if we cross from B to C
      newOctave = if noteName_ == B && quotient > 0 then succ o else o
   in Pitch newNoteName newAcc newOctave

scaleToQuarterTonePitches :: Scale -> Pitch -> [Pitch]
scaleToQuarterTonePitches (Scale _ intervals _) rootPitch =
  scanl addQuarterToneInterval rootPitch intervals
  where
    addQuarterToneInterval :: Pitch -> Interval -> Pitch
    addQuarterToneInterval pitch (Interval interval) =
      let quarterTones = realToFrac interval * 4 -- Convert interval to quarter-tone steps
       in case drop (round quarterTones) (iterate (modifyPitchQuarterTone succ) pitch) of
            x : _ -> x
            [] -> error "scaleToQuarterTonePitches error"


modifyPitchQuarterTone :: (Accidental -> Accidental) -> Pitch -> Pitch
modifyPitchQuarterTone f (Pitch noteName_ acc octave_) =
  let (semitonesCrossed, remainder) = splitFraction $ accidentalToSemitones (f acc) / 2
      -- Adjust the note name based on the number of semitones crossed
      newNoteName = case noteName_ of
                      B | semitonesCrossed > 0 -> C  -- Wrap from B to C
                      _ -> iterate succ noteName_ !! fromInteger semitonesCrossed
      -- Increment octave if crossing from B to C
      newOctave = if noteName_ == B && semitonesCrossed > 0 then succ octave_ else octave_
      -- Apply enharmonic preferences
      (finalNoteName, newAcc) = preferredEnharmonic newNoteName (semitonesToAccidental (remainder * 2))
   in Pitch finalNoteName newAcc newOctave

      
preferredEnharmonic :: NoteName -> Accidental -> (NoteName, Accidental)
preferredEnharmonic A Sharp  = (B, Flat)   
preferredEnharmonic G Flat   = (F, Sharp)   
preferredEnharmonic E Sharp  = (F, Natural) 
preferredEnharmonic B Sharp  = (C, Natural) 
preferredEnharmonic G ThreeQuartersSharp = (A, QuarterSharp) 
preferredEnharmonic D Sharp  = (E, Flat)    -- Prefer Eb over D# etc
preferredEnharmonic note acc = (note, acc)  -- Default: Keep the note and accidental unchanged



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
halfTonePitches = scaleToHalfTonePitches exampleScale c4

quarterTonePitches :: [Pitch]
quarterTonePitches = scaleToQuarterTonePitches exampleQuarterToneScale c4

chromaticScale :: Scale
chromaticScale = Scale "Chromatic" (Interval <$> replicate 12 (1%1)) Nothing

chromaticPitches :: [Pitch]
chromaticPitches = scaleToHalfTonePitches chromaticScale c4
-- [C Natural Octave 4,D Natural Octave 4,E Natural Octave 4,F Natural Octave 4,G Natural Octave 4,A Natural Octave 4,B Natural Octave 4,C Natural Octave 5,D Natural Octave 5,E Natural Octave 5,F Natural Octave 5,G Natural Octave 5,A Natural Octave 5]


-- > scaleToQuarterTonePitches exampleQuarterToneScale c4
-- [C Natural Octave 4,C Sharp Octave 4,D Sharp Octave 4,E QuarterSharp Octave 4,F Sharp Octave 4,G Sharp Octave 4,A QuarterSharp Octave 4,B Natural Octave 4]