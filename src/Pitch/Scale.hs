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
    -- modRational x y = x - y * realToFrac (floor $ realToFrac x / realToFrac y)

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

-- >quarterToneScale
-- QuarterTone [0 % 1,3 % 2,5 % 2,3 % 1,9 % 2,5 % 1,6 % 1,15 % 2,9 % 1,10 % 1,21 % 2,12 % 1]

scaleToHalfTonePitches :: Scale -> Pitch -> [Pitch]
scaleToHalfTonePitches (Scale _ intervals _) rootPitch =
  scanl addInterval rootPitch intervals
  where -- 0 for root position, 1 for first mode, etc.
    addInterval :: Pitch -> Interval -> Pitch
    addInterval pitch (Interval interval) =
      let semitones = round $ interval * 2 --  to semitones
       in (case drop semitones (iterate (modifyPitch succ) pitch) of
         x : _ -> x
         [] -> error "scaleToHalfTonePitches error" )

modifyPitch :: (Accidental -> Accidental) -> Pitch -> Pitch
modifyPitch f (Pitch noteName_ acc o) =
  let (quotient, remainder) = splitFraction $ accidentalToSemitones (f acc)
      newNoteName = (case drop (fromInteger quotient) (iterate succ noteName_) of
         x : _ -> x
         [] -> error "modifyPitch error" )
      newAcc = semitonesToAccidental remainder
      newOctave = if newNoteName < noteName_ then succ o else o
   in Pitch newNoteName newAcc newOctave

scaleToQuarterTonePitches :: Scale -> Pitch -> [Pitch]
scaleToQuarterTonePitches (Scale _ intervals _) rootPitch =
  scanl addQuarterToneInterval rootPitch intervals
  where
    addQuarterToneInterval :: Pitch -> Interval -> Pitch
    addQuarterToneInterval pitch (Interval interval) =
      let quarterTones = round $ interval * 4 --  to quarter-tones
       in (case drop quarterTones (iterate (modifyPitchQuarterTone succ) pitch) of
         x : _ -> x
         [] -> error "scaleToQuarterTonePitches error" )

modifyPitchQuarterTone :: (Accidental -> Accidental) -> Pitch -> Pitch
modifyPitchQuarterTone f (Pitch noteName_ acc octave_) =
  let (quotient, remainder) = splitFraction $ (accidentalToSemitones (f acc) / 2)
      newNoteName = (case drop (fromInteger quotient) (iterate succ noteName_) of
         x : _ -> x
         [] -> error "modifyPitchQuarterTone error" )
      newAcc = semitonesToAccidental (remainder * 2)
      newOctave = if newNoteName < noteName_ then succ octave_ else octave_
   in Pitch newNoteName newAcc newOctave


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
exampleQuarterToneScale = Scale "Quarter-Tone Example" [Interval (1 % 2), Interval 1, Interval (3 % 4), Interval (5 % 4), Interval 1, Interval (3 % 4), Interval (3 % 4)] Nothing

c4 :: Pitch
c4 = Pitch C Natural (Octave 4)

halfTonePitches :: [Pitch]
halfTonePitches = scaleToHalfTonePitches exampleScale c4

quarterTonePitches :: [Pitch]
quarterTonePitches = scaleToQuarterTonePitches exampleQuarterToneScale c4
