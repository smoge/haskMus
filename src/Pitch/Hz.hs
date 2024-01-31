module Pitch.Hz where

-- | The Pitch.Hz module provides types and functions to work with musical pitches in terms of semitones and Hertz.

-- `newtype` wrappers for `Semitone` and `Hz` provide type safety.
newtype Semitone = Semitone {unSemitone :: Double} deriving (Eq, Num, Ord)
newtype Hz = Hz {unHz :: Double} deriving (Eq, Num, Ord)

instance Show Semitone where
  show (Semitone n) = show n <> " Semitone(s)"

instance Show Hz where
  show (Hz hz) = show hz <> " Hz"


-- | Standard pitch (A4) definition in Hertz.
-- 
-- >>> pitchStandard
-- 440.0 Hz
pitchStandard :: Hz
pitchStandard = Hz 440.0

-- | Convert a `Semitone` or `Double` representing semitones to `Hz`.
-- Works for both integral and fractional semitones.
--
-- >>> semitoneToHz 12
-- 880.0 Hz
semitoneToHz :: Double -> Hz
semitoneToHz n = pitchStandard * Hz (2 ** (n / 12.0))

-- | Convert a frequency in `Hz` to its equivalent in semitones relative to `pitchStandard`.
-- Calculated using the logarithm base 2.
--
-- >>> hzToDouble (Hz 880.0)
-- 12.0
hzToDouble :: Hz -> Double
hzToDouble (Hz hz) = 12.0 * logBase 2 (hz / unHz pitchStandard)

-- | Use `hzToDouble` to convert Hz to `Semitone`.
--
-- >>> hzToSemitone (Hz 880.0)
-- 12.0 Semitone(s)
hzToSemitone :: Hz -> Semitone
hzToSemitone = Semitone . hzToDouble

-- | Directly retrieve the underlying `Double` from a `Semitone`.
--
-- >>> semitoneToDouble (Semitone 12.0)
-- 12.0
semitoneToDouble :: Semitone -> Double
semitoneToDouble (Semitone n) = n

-- | Wrap a `Double` as a `Semitone`.
--
-- >>> doubleToSemitone 12.0
-- 12.0 Semitone(s)
doubleToSemitone :: Double -> Semitone
doubleToSemitone = Semitone
