{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveLift             #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedRecordDot    #-}
-- Needed for lens operations
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Pitch.Interval where

import           Data.Data                  (Data)
import           Data.Fixed                 (mod')
import           Data.List                  (find)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe)
import           Data.String                (IsString (..))
import           Language.Haskell.TH        ()
import           Language.Haskell.TH.Syntax (Lift)
import Text.Megaparsec
    ( oneOf,
      runParser,
      errorBundlePretty,
      choice,
      some,
      Parsec,
      MonadParsec(eof, try),
      ParseErrorBundle )

import Data.Void ( Void )
import Pitch.Accidental
    ( Accidental(Custom, Natural, QuarterSharp, QuarterFlat, Sharp,
                 Flat, ThreeQuartersSharp, ThreeQuartersFlat, DoubleSharp,
                 DoubleFlat) )
import Pitch.Pitch
    ( Octave(Octave),
      Pitch(accidental, Pitch, noteName),
      pitchToRational )            
import Pitch.PitchClass ( PitchClass(PitchClass), NoteName(..) )   
import Text.Megaparsec.Char ( digitChar, string )

newtype Interval = Interval { semitones :: Rational }
  deriving (Eq, Ord, Lift, Data)


instance Show Interval where
  show :: Interval -> String
  show interval = case nameFromInterval interval of
    Just name -> intervalNameToNotation name
    Nothing   -> "Interval(" <> show (fromRational (semitones interval) :: Double) <> " semitones)"


instance Num Interval where
  (+) (Interval a) (Interval b) = Interval (a + b)
  (-) (Interval a) (Interval b) = Interval (a - b)
  (*) (Interval a) (Interval b) = Interval (a * b)
  negate (Interval a) = Interval (negate a)
  abs (Interval a) = Interval (abs a)
  signum (Interval a) = Interval (signum a)
  fromInteger n = Interval (fromInteger n)

instance Semigroup Interval where
  (<>) = (+)

instance Monoid Interval where
  mempty = Interval 0

instance Fractional Interval where
  (/) (Interval a) (Interval b) = Interval (a / b)
  fromRational = Interval

instance Real Interval where
  toRational (Interval r) = r

instance RealFrac Interval where
  properFraction (Interval r) = (fromInteger i, Interval f)
    where (i, f) = properFraction r

-- | Represents named musical intervals.
data IntervalName =
    Unison
  | MinorSecond | MajorSecond | AugmentedSecond
  | DiminishedThird | MinorThird | MajorThird | AugmentedThird
  | DiminishedFourth | PerfectFourth | AugmentedFourth
  | DiminishedFifth | PerfectFifth | AugmentedFifth
  | MinorSixth | MajorSixth | AugmentedSixth
  | DiminishedSeventh | MinorSeventh | MajorSeventh | AugmentedSeventh
  | PerfectOctave
  deriving (Eq, Ord, Show, Enum, Bounded, Data)

intervalMap :: Map.Map IntervalName Rational
intervalMap = Map.fromList
  [ (Unison, 0)
  , (MinorSecond, 1)
  , (MajorSecond, 2)
  , (AugmentedSecond, 3)
  , (DiminishedThird, 2)
  , (MinorThird, 3)
  , (MajorThird, 4)
  , (AugmentedThird, 5)
  , (DiminishedFourth, 4)
  , (PerfectFourth, 5)
  , (AugmentedFourth, 6)
  , (DiminishedFifth, 6)
  , (PerfectFifth, 7)
  , (AugmentedFifth, 8)
  , (MinorSixth, 8)
  , (MajorSixth, 9)
  , (AugmentedSixth, 10)
  , (DiminishedSeventh, 9)
  , (MinorSeventh, 10)
  , (MajorSeventh, 11)
  , (AugmentedSeventh, 12)
  , (PerfectOctave, 12)
  ]

intervalFromName :: IntervalName -> Interval
intervalFromName name = Interval $ fromMaybe (error $ "Unknown interval: " <> show name) (Map.lookup name intervalMap)

nameFromInterval :: Interval -> Maybe IntervalName
nameFromInterval (Interval s) = fst <$> find (\(_, semis) -> semis == s) (Map.toList intervalMap)

invertInterval :: Interval -> Interval
invertInterval (Interval s) = Interval (12 - (s `mod'` 12))

simplifyInterval :: Interval -> Interval
simplifyInterval (Interval s) = Interval (s `mod'` 12)

isCompoundInterval :: Interval -> Bool
isCompoundInterval (Interval s) = s >= 12

complementInterval :: Interval -> Interval
complementInterval (Interval s) = Interval (12 - (s `mod'` 12))

unison, minorSecond, majorSecond, augmentedSecond, minorThird, majorThird, perfectFourth, augmentedFourth :: Interval
diminishedFifth, perfectFifth, minorSixth, majorSixth, minorSeventh, majorSeventh, perfectOctave :: Interval

unison            = intervalFromName Unison
minorSecond       = intervalFromName MinorSecond
majorSecond       = intervalFromName MajorSecond
augmentedSecond   = intervalFromName AugmentedSecond
minorThird        = intervalFromName MinorThird
majorThird        = intervalFromName MajorThird
perfectFourth     = intervalFromName PerfectFourth
augmentedFourth   = intervalFromName AugmentedFourth
diminishedFifth   = intervalFromName DiminishedFifth
perfectFifth      = intervalFromName PerfectFifth
minorSixth        = intervalFromName MinorSixth
majorSixth        = intervalFromName MajorSixth
minorSeventh      = intervalFromName MinorSeventh
majorSeventh      = intervalFromName MajorSeventh
perfectOctave     = intervalFromName PerfectOctave

-- | Parser for intervals using standard musical notation (e.g., "M3", "P5").
-- Supports qualities: P (Perfect), M (Major), m (Minor), A (Augmented), d (Diminished).
type Parser = Parsec Void String


parseQuality :: Parser String
parseQuality = choice parsers
  where
    parsers :: [Parser String]
    parsers =
      [ try (string "dd")
      , try (string "AA")
      , (:[]) <$> oneOf ("PpMmAaDd" :: String)
      ]

parseNumber :: Parser Int
parseNumber = read <$> some digitChar

parseInterval :: String -> Either (ParseErrorBundle String Void) Interval
parseInterval = runParser intervalParser ""

intervalParser :: Parser Interval
intervalParser = do
  quality <- parseQuality
  number <- parseNumber
  eof
  case intervalFromNotation quality number of
    Just interval -> pure interval
    Nothing       -> fail $ "Invalid interval: " <> quality <> show number


-- | Maps notation to 'Interval's.
intervalFromNotation :: String -> Int -> Maybe Interval
intervalFromNotation quality number = do
  name <- intervalNameFromNotation quality number
  pure $ intervalFromName name

-- | Maps notation to 'IntervalName's.
intervalNameFromNotation :: String -> Int -> Maybe IntervalName
intervalNameFromNotation quality number = Map.lookup (quality, number) notationMap

-- | Notation map for interval parsing.
notationMap :: Map.Map (String, Int) IntervalName
notationMap = Map.fromList
  [ (("P", 1), Unison)
  , (("m", 2), MinorSecond)
  , (("M", 2), MajorSecond)
  , (("A", 2), AugmentedSecond)
  , (("d", 3), DiminishedThird)
  , (("m", 3), MinorThird)
  , (("M", 3), MajorThird)
  , (("A", 3), AugmentedThird)
  , (("d", 4), DiminishedFourth)
  , (("P", 4), PerfectFourth)
  , (("A", 4), AugmentedFourth)
  , (("d", 5), DiminishedFifth)
  , (("P", 5), PerfectFifth)
  , (("A", 5), AugmentedFifth)
  , (("m", 6), MinorSixth)
  , (("M", 6), MajorSixth)
  , (("A", 6), AugmentedSixth)
  , (("d", 7), DiminishedSeventh)
  , (("m", 7), MinorSeventh)
  , (("M", 7), MajorSeventh)
  , (("A", 7), AugmentedSeventh)
  , (("P", 8), PerfectOctave)
  ]

-- | Allows string literals to represent 'Interval's using the 'IsString' type class.
instance IsString Interval where
  fromString s = case parseInterval s of
    Right interval -> interval
    Left err       -> error $ errorBundlePretty err


-- | Converts an 'IntervalName' to its standard notation.
intervalNameToNotation :: IntervalName -> String
intervalNameToNotation name = fromMaybe (error "Unknown interval name") $ Map.lookup name notationToIntervalName

-- | Mapping from 'IntervalName's to their standard notation.
notationToIntervalName :: Map.Map IntervalName String
notationToIntervalName = Map.fromList
  [ (Unison, "P1")
  , (MinorSecond, "m2")
  , (MajorSecond, "M2")
  , (AugmentedSecond, "A2")
  , (DiminishedThird, "d3")
  , (MinorThird, "m3")
  , (MajorThird, "M3")
  , (AugmentedThird, "A3")
  , (DiminishedFourth, "d4")
  , (PerfectFourth, "P4")
  , (AugmentedFourth, "A4")
  , (DiminishedFifth, "d5")
  , (PerfectFifth, "P5")
  , (AugmentedFifth, "A5")
  , (MinorSixth, "m6")
  , (MajorSixth, "M6")
  , (AugmentedSixth, "A6")
  , (DiminishedSeventh, "d7")
  , (MinorSeventh, "m7")
  , (MajorSeventh, "M7")
  , (AugmentedSeventh, "A7")
  , (PerfectOctave, "P8")
  ]

-- | Utility function to create an 'Interval' from semitones.
fromSemitones :: Rational -> Interval
fromSemitones = Interval

-- | Applies an 'Interval' to a starting pitch. (Requires 'Pitch.Pitch.Pitch' module)
-- transposeUp :: Pitch -> Interval -> Pitch
-- transposeUp = (+.)

-- | Calculates the interval between two pitches. (Requires 'Pitch.Pitch.Pitch' module)
getInterval :: Pitch.Pitch.Pitch -> Pitch.Pitch.Pitch -> Interval
getInterval p1 p2 = Interval (Pitch.Pitch.pitchToRational p2 - Pitch.Pitch.pitchToRational p1)





(-:) :: Pitch -> Pitch -> Interval
p1 -: p2 = Interval (pitchToRational p2 - pitchToRational p1)
--
(+.) :: Pitch -> Interval -> Pitch
p +. i = rationalToPitch (pitchToRational p + semitones i)

(-.) :: Pitch -> Interval -> Pitch
p -. i = rationalToPitch (pitchToRational p - semitones i)


rationalToPitch :: Rational -> Pitch
rationalToPitch semitones_ =
  let
    octave_ = Octave (floor (semitones_ / 12) - 1)
    semitoneInOctave = semitones_ `mod'` 12
    (noteName_, naturalSemitone) = closestNoteName semitoneInOctave
    accidental_ = computeAccidental (semitoneInOctave - naturalSemitone)
  in
    Pitch noteName_ accidental_ octave_



computeAccidental :: Rational -> Accidental
computeAccidental x
  | absx <= 2  = case absx of
      _ | absx < 0.5  -> Natural
        | absx < 0.75 -> selectAccidental QuarterSharp QuarterFlat
        | absx < 1.25 -> selectAccidental Sharp Flat
        | absx < 1.75 -> selectAccidental ThreeQuartersSharp ThreeQuartersFlat
        | otherwise   -> selectAccidental DoubleSharp DoubleFlat
  | otherwise  = Custom "Custom" x
  where
    absx = abs x
    selectAccidental sharp flat = if x > 0 then sharp else flat


rationalToPitchClass :: Rational -> PitchClass
rationalToPitchClass semitones_ =
  let pitch = rationalToPitch semitones_
  in PitchClass pitch.noteName pitch.accidental


closestNoteName :: Rational -> (NoteName, Rational)
closestNoteName semitone =
  let noteRationals = [(C, 0), (D, 2), (E, 4), (F, 5), (G, 7), (A, 9), (B, 11)] :: [(NoteName, Rational)]
  in fromMaybe (error "Invalid semitone value") $ find (\(_, s) -> s <= semitone) noteRationals


{-



newtype Interval = Interval { semitones :: Rational }
  deriving (Eq, Ord, Show, Lift, Data)

instance Num Interval where
  (+) (Interval a) (Interval b) = Interval (a + b)
  (-) (Interval a) (Interval b) = Interval (a - b)
  (*) (Interval a) (Interval b) = Interval (a * b)
  negate (Interval a) = Interval (negate a)
  abs (Interval a) = Interval (abs a)
  signum (Interval a) = Interval (signum a)
  fromInteger :: Integer -> Interval
  fromInteger n = Interval (fromInteger n)

instance Semigroup Interval where
  (<>) :: Interval -> Interval -> Interval
  (<>) = (+)

instance Monoid Interval where
  mempty :: Interval
  mempty = Interval 0


rationalToPitch :: Rational -> Pitch
rationalToPitch semitones_ =
  let
    octave_ = Octave (floor (semitones_ / 12) - 1)
    semitoneInOctave = semitones_ `mod'` 12
    (noteName_, naturalSemitone) = closestNoteName semitoneInOctave
    accidental_ = computeAccidental (semitoneInOctave - naturalSemitone)
  in
    Pitch noteName_ accidental_ octave_

closestNoteName :: Rational -> (NoteName, Rational)
closestNoteName semitone =
  let noteRationals = [(C, 0), (D, 2), (E, 4), (F, 5), (G, 7), (A, 9), (B, 11)] :: [(NoteName, Rational)]
  in fromMaybe (error "Invalid semitone value") $ find (\(_, s) -> s <= semitone) noteRationals



computeAccidental :: Rational -> Accidental
computeAccidental x
  | absx <= 2  = case absx of
      _ | absx < 0.5  -> Natural
        | absx < 0.75 -> selectAccidental QuarterSharp QuarterFlat
        | absx < 1.25 -> selectAccidental Sharp Flat
        | absx < 1.75 -> selectAccidental ThreeQuartersSharp ThreeQuartersFlat
        | otherwise   -> selectAccidental DoubleSharp DoubleFlat
  | otherwise  = Custom "Custom" x
  where
    absx = abs x
    selectAccidental sharp flat = if x > 0 then sharp else flat


rationalToPitchClass :: Rational -> PitchClass
rationalToPitchClass semitones_ =
  let pitch = rationalToPitch semitones_
  in PitchClass pitch.noteName pitch.accidental




pitchToInterval :: Pitch -> Pitch -> Interval
pitchToInterval p1 p2 = Interval (pitchToRational p2 - pitchToRational p1)

invertInterval :: Interval -> Interval
invertInterval (Interval s) = Interval (12 - (s `mod'` 12))

isCompoundInterval :: Interval -> Bool
isCompoundInterval (Interval s) = s >= 12

simplifyInterval :: Interval -> Interval
simplifyInterval (Interval s) = Interval (s `mod'` 12)

(=~=) :: Pitch -> Pitch -> Bool
p1 =~= p2 = pitchToRational p1 `mod'` 12 == pitchToRational p2 `mod'` 12

getInterval :: Pitch -> Pitch -> Interval
getInterval = (-:)

transposeUp :: Pitch -> Interval -> Pitch
transposeUp = (+.)

transposeDown :: Pitch -> Interval -> Pitch
transposeDown = (-.)

createScale :: Pitch -> [Interval] -> [Pitch]
createScale = scanl transposeUp


data IntervalName =
    Unison
  | AugmentedUnison
  | MinorSecond | NeutralSecond | MajorSecond
  | AugmentedSecond
  | MinorThird | NeutralThird | MajorThird
  | AugmentedThird
  | DiminishedFourth | PerfectFourth | AugmentedFourth
  | DiminishedFifth | Tritone | AugmentedFourth'
  | DiminishedFifth' | PerfectFifth | AugmentedFifth
  | MinorSixth | NeutralSixth | MajorSixth
  | AugmentedSixth
  | DiminishedSeventh | MinorSeventh | NeutralSeventh | MajorSeventh
  | PerfectOctave
  | AugmentedOctave
  | MinorNinth | NeutralNinth | MajorNinth
  | AugmentedNinth
  | MinorTenth | NeutralTenth | MajorTenth
  | AugmentedTenth
  | PerfectEleventh | AugmentedEleventh
  | DiminishedTwelfth | PerfectTwelfth | AugmentedTwelfth
  | MinorThirteenth | NeutralThirteenth | MajorThirteenth
  | AugmentedThirteenth
  | MinorFourteenth | NeutralFourteenth | MajorFourteenth
  | DoubleOctave
  deriving (Eq, Ord, Show, Enum, Bounded)

intervalMap :: Map.Map IntervalName Rational
intervalMap = Map.fromList
  [ (Unison, 0)
  , (AugmentedUnison, 1/2)
  , (MinorSecond, 1)
  , (NeutralSecond, 3/2)
  , (MajorSecond, 2)
  , (AugmentedSecond, 5/2)
  , (MinorThird, 3)
  , (NeutralThird, 7/2)
  , (MajorThird, 4)
  , (AugmentedThird, 9/2)
  , (DiminishedFourth, 4)
  , (PerfectFourth, 5)
  , (AugmentedFourth, 11/2)
  , (DiminishedFifth, 6)
  , (Tritone, 6)
  , (AugmentedFourth', 13/2)
  , (DiminishedFifth', 13/2)
  , (PerfectFifth, 7)
  , (AugmentedFifth, 15/2)
  , (MinorSixth, 8)
  , (NeutralSixth, 17/2)
  , (MajorSixth, 9)
  , (AugmentedSixth, 19/2)
  , (DiminishedSeventh, 9)
  , (MinorSeventh, 10)
  , (NeutralSeventh, 21/2)
  , (MajorSeventh, 11)
  , (PerfectOctave, 12)
  , (AugmentedOctave, 25/2)
  , (MinorNinth, 13)
  , (NeutralNinth, 27/2)
  , (MajorNinth, 14)
  , (AugmentedNinth, 29/2)
  , (MinorTenth, 15)
  , (NeutralTenth, 31/2)
  , (MajorTenth, 16)
  , (AugmentedTenth, 33/2)
  , (PerfectEleventh, 17)
  , (AugmentedEleventh, 35/2)
  , (DiminishedTwelfth, 18)
  , (PerfectTwelfth, 19)
  , (AugmentedTwelfth, 39/2)
  , (MinorThirteenth, 20)
  , (NeutralThirteenth, 41/2)
  , (MajorThirteenth, 21)
  , (AugmentedThirteenth, 43/2)
  , (MinorFourteenth, 22)
  , (NeutralFourteenth, 45/2)
  , (MajorFourteenth, 23)
  , (DoubleOctave, 24)
  ]

intervalFromName :: IntervalName -> Interval
intervalFromName name = case Map.lookup name intervalMap of
  Just semitones -> Interval semitones
  Nothing -> error $ "Klarenz ERROR: in Pitch.Pitch function intervalFromName: Unknown interval: " <> show name


nameFromInterval :: Interval -> Maybe IntervalName
nameFromInterval (Interval semitones) =
  fmap fst $ find (\(_, s) -> s == semitones) $ Map.toList intervalMap


unison, augmentedUnison, minorSecond, neutralSecond, majorSecond, augmentedSecond, minorThird :: Interval
neutralThird, majorThird, augmentedThird, diminishedFourth, perfectFourth :: Interval
augmentedFourth, diminishedFifth, tritone, augmentedFourth', diminishedFifth' :: Interval
perfectFifth, augmentedFifth, minorSixth, neutralSixth, majorSixth, augmentedSixth :: Interval
diminishedSeventh, minorSeventh, neutralSeventh, majorSeventh, perfectOctave :: Interval
augmentedOctave, minorNinth, neutralNinth, majorNinth, augmentedNinth, minorTenth :: Interval
neutralTenth, majorTenth, augmentedTenth, perfectEleventh, augmentedEleventh :: Interval
diminishedTwelfth, perfectTwelfth, augmentedTwelfth, minorThirteenth, neutralThirteenth :: Interval
majorThirteenth, augmentedThirteenth, minorFourteenth, neutralFourteenth :: Interval
majorFourteenth, doubleOctave :: Interval

unison = intervalFromName Unison
augmentedUnison = intervalFromName AugmentedUnison
minorSecond = intervalFromName MinorSecond
neutralSecond = intervalFromName NeutralSecond
majorSecond = intervalFromName MajorSecond
augmentedSecond = intervalFromName AugmentedSecond
minorThird = intervalFromName MinorThird
neutralThird = intervalFromName NeutralThird
majorThird = intervalFromName MajorThird
augmentedThird = intervalFromName AugmentedThird
diminishedFourth = intervalFromName DiminishedFourth
perfectFourth = intervalFromName PerfectFourth
augmentedFourth = intervalFromName AugmentedFourth
diminishedFifth = intervalFromName DiminishedFifth
tritone = intervalFromName Tritone
augmentedFourth' = intervalFromName AugmentedFourth'
diminishedFifth' = intervalFromName DiminishedFifth'
perfectFifth = intervalFromName PerfectFifth
augmentedFifth = intervalFromName AugmentedFifth
minorSixth = intervalFromName MinorSixth
neutralSixth = intervalFromName NeutralSixth
majorSixth = intervalFromName MajorSixth
augmentedSixth = intervalFromName AugmentedSixth
diminishedSeventh = intervalFromName DiminishedSeventh
minorSeventh = intervalFromName MinorSeventh
neutralSeventh = intervalFromName NeutralSeventh
majorSeventh = intervalFromName MajorSeventh
perfectOctave = intervalFromName PerfectOctave
augmentedOctave = intervalFromName AugmentedOctave
minorNinth = intervalFromName MinorNinth
neutralNinth = intervalFromName NeutralNinth
majorNinth = intervalFromName MajorNinth
augmentedNinth = intervalFromName AugmentedNinth
minorTenth = intervalFromName MinorTenth
neutralTenth = intervalFromName NeutralTenth
majorTenth = intervalFromName MajorTenth
augmentedTenth = intervalFromName AugmentedTenth
perfectEleventh = intervalFromName PerfectEleventh
augmentedEleventh = intervalFromName AugmentedEleventh
diminishedTwelfth = intervalFromName DiminishedTwelfth
perfectTwelfth = intervalFromName PerfectTwelfth
augmentedTwelfth = intervalFromName AugmentedTwelfth
minorThirteenth = intervalFromName MinorThirteenth
neutralThirteenth = intervalFromName NeutralThirteenth
majorThirteenth = intervalFromName MajorThirteenth
augmentedThirteenth = intervalFromName AugmentedThirteenth
minorFourteenth = intervalFromName MinorFourteenth
neutralFourteenth = intervalFromName NeutralFourteenth
majorFourteenth = intervalFromName MajorFourteenth
doubleOctave = intervalFromName DoubleOctave


-- Apply intervals to a starting pitch after modifying each interval
applyModifiedIntervals :: Pitch -> [Interval] -> [Pitch]
applyModifiedIntervals start intervals = mkPitchesFromIntervals start modifiedIntervals
  where
    modifiedIntervals = intervals & each %~ (*2)

allIntervalNames :: [IntervalName]
allIntervalNames = enumerate

allIntervals :: [Interval]
allIntervals = fmap intervalFromName allIntervalNames

instance Fractional Interval where
  (/) :: Interval -> Interval -> Interval
  (/) (Interval a) (Interval b) = Interval (a / b)
  fromRational :: Rational -> Interval
  fromRational = Interval

instance Real Interval where
  toRational :: Interval -> Rational
  toRational (Interval r) = r

instance RealFrac Interval where
  properFraction :: Integral b => Interval -> (b, Interval)
  properFraction (Interval r) = (fromInteger i, Interval f)
    where (i, f) = properFraction r

complementInterval :: Interval -> Interval
complementInterval (Interval semitones_) = Interval (12 - (semitones_ `mod'` 12))

createNamedScale :: Pitch -> [IntervalName] -> [Pitch]
createNamedScale start intervalNames =
  scanl (+.) start (fmap intervalFromName intervalNames)

majorScaleIntervalNames :: [IntervalName]
majorScaleIntervalNames = [Unison, MajorSecond, MajorThird, PerfectFourth, PerfectFifth, MajorSixth, MajorSeventh]

minorScaleIntervals :: [IntervalName]
minorScaleIntervals = [Unison, MajorSecond, MinorThird, PerfectFourth, PerfectFifth, MinorSixth, MinorSeventh]

minorScale :: Pitch -> [Pitch]
minorScale start = createNamedScale start minorScaleIntervals


majorScale :: Pitch -> [Pitch]
majorScale start = createNamedScale start majorScaleIntervalNames

majorScaleInterval :: [Interval]
majorScaleInterval = fmap intervalFromName majorScaleIntervalNames

minorScaleInterval :: [Interval]
minorScaleInterval = fmap intervalFromName minorScaleIntervals


--
-- >majorScaleInterval
--[Interval {semitones = 0 % 1},Interval {semitones = 2 % 1},Interval {semitones = 4 % 1},Interval {semitones = 5 % 1},Interval {semitones = 7 % 1},Interval {semitones = 9 % 1},Interval {semitones = 11 % 1}]


intervalsToNames :: [Interval] -> [IntervalName]
intervalsToNames intervals = fromMaybe (error "Invalid interval") . nameFromInterval <$> intervals


mkPitchesFromIntervals :: Pitch -> [Interval] -> [Pitch]
mkPitchesFromIntervals start intervals =
  addInterval start <$> intervals
  where
    addInterval pitch (Interval semitones_) =
      rationalToPitch (pitchToRational pitch + semitones_)


cMinorScale :: [PitchClass]
cMinorScale =
  [ PitchClass C Natural
  , PitchClass D Natural
  , PitchClass E Flat
  , PitchClass F Natural
  , PitchClass G Natural
  , PitchClass A Flat
  , PitchClass B Flat
  ]

cMinorEnharmonicMapping :: EnharmonicMapping
cMinorEnharmonicMapping = enharmonicMapping $ pcToRational <$> cMinorScale

prettyPrintEnharmonicMapping :: EnharmonicMapping -> IO ()
prettyPrintEnharmonicMapping  = mapM_ printEntry
  where
    printEntry (ratio, pcs) = do
      putStrLn $ "Semitones from C: " <> show (fromRational ratio :: Double)
      putStrLn "Enharmonic equivalents:"
      mapM_ (\pc_ -> putStrLn $ "  " <> show pc_) pcs
      putStrLn ""


-------------------------------------------------------------

{-

p :: Pitch
p = Pitch C Natural (Octave 4)

p2 :: Pitch
p2 = p & pitchClass .~ PitchClass D Sharp

pc1 :: PitchClass
pc1 = p ^. pitchClass


-}

-}
