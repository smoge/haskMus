{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pitch.PitchLike ( PitchLike(..) ) where

import           Pitch.Accidental (Accidental (Natural))
import           Pitch.Interval   
import           Pitch.Pitch      hiding (Rule (..))
import           Pitch.PitchClass


class PitchLike a where
  toPitch :: a -> Pitch
  fromPitch :: Pitch -> a

  pitchToSemitones :: a -> Rational
  pitchToSemitones = pitchToRational . toPitch

  transpose :: Interval -> a -> a
  transpose interval = fromPitch . (+. interval) . toPitch

  intervalBetween :: a -> a -> Interval
  intervalBetween a1 a2 = Interval $ pitchToSemitones a2 - pitchToSemitones a1

instance PitchLike Pitch where
  toPitch :: Pitch -> Pitch
  toPitch = id

  fromPitch :: Pitch -> Pitch
  fromPitch = id

instance PitchLike PitchClass where
  toPitch :: PitchClass -> Pitch
  toPitch pitchclass = Pitch pitchclass.noteName pitchclass.accidental (Octave 4)

  fromPitch :: Pitch -> PitchClass
  fromPitch pitch = PitchClass pitch.noteName pitch.accidental

instance PitchLike NoteName where
  toPitch :: NoteName -> Pitch
  toPitch nn = Pitch nn Natural (Octave 4)

  fromPitch :: Pitch -> NoteName
  fromPitch pit = pit.noteName


-- fromPitch @PitchClass (toPitch pc)
-- C Natural

