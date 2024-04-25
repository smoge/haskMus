module Pitch.Hz
  ( -- * Types
    Semitone (..),
    Hz (..),

    -- * Functions
    semitoneToHz,
    hzToSemitone,
    hzToMidi,
    roundHzToMidi,
  )
where

-- | The Pitch.Hz module provides types and functions to work with musical pitches in terms of semitones and Hertz.

-- `newtype` wrappers for `Semitone` and `Hz` provide type safety.
newtype Semitone = Semitone {unSemitone :: Double} deriving (Eq, Num, Ord)

newtype Hz = Hz {unHz :: Double} deriving (Eq, Num, Ord)

instance Show Semitone where
  show (Semitone n) = show n <> " Semitone(s)"

instance Show Hz where
  show (Hz hz) = show hz <> " Hz"

-- Standard pitch (A4) definition in Hertz.
pitchStandard :: Hz
pitchStandard = Hz 440.0

middleCinHz :: Hz
middleCinHz = Hz 261.625565301

-- Convert a `Semitone` or `Double` representing semitones to `Hz`.
semitoneToHz :: Double -> Hz
semitoneToHz n = Hz $ unHz middleCinHz * 2 ** (n / 12.0)

-- Convert a frequency in `Hz` to its equivalent in semitones relative to `middleCinHz`.
hzToDouble :: Hz -> Double
hzToDouble (Hz hz) = 12.0 * logBase 2 (hz / unHz middleCinHz)

-- Use `unHz` to convert Hz to `Semitone` for consistency.
hzToSemitone :: Hz -> Semitone
hzToSemitone = Semitone . hzToDouble

-- MIDI note number to Hz
midiToHz :: Double -> Hz
midiToHz n = Hz $ 2 ** ((n - 69) / 12) * 440

-- Hz to MIDI note number
hzToMidi :: Hz -> Double
hzToMidi (Hz hz) = 12 * logBase 2 (hz / 440) + 69

-- Convert a `Semitone` to its equivalent in MIDI note number.
semitoneToMidi :: Semitone -> Double
semitoneToMidi = hzToMidi . semitoneToHz . unSemitone

-- Function to round a `Double` to a specified number of decimal places.
roundTo :: Int -> Double -> Double
roundTo n f = fromInteger (round $ f * (10 ^ n)) / (10.0 ^^ n)

-- Example usage with rounding: Convert Hz to MIDI and round to 2 decimal places.
-- >>> roundHzToMidi middleCinHz
-- 60.0
roundHzToMidi :: Hz -> Double
roundHzToMidi = roundTo 5 . hzToMidi
