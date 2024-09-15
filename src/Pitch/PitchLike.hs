{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Pitch.PitchLike where

import Pitch.Pitch hiding (Rule (..))
import Pitch.Accidental ( Accidental(Natural) )


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
