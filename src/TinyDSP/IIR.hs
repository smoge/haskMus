{-# LANGUAGE NamedFieldPuns #-}

-- TODO use CFloat before implementing more modules

-- | This module implements tiny IIR filters.
--
-- See: http://shepazu.github.io/Audio-EQ-Cookbook/audio-eq-cookbook.html
--  Or: https://www.w3.org/TR/audio-eq-cookbook/
module TinyDSP.IIR
  ( -- * Usage
    filterSamples,
    IIRParams,
    initialIIRState,
    IIRState,

    -- * Design
    lowPassFilter,
    highPassFilter,
    bandPassFilter,
    bandPassSkirtFilter,
    notchFilter,
    lowShelfFilter,
    highShelfFilter,

    -- * Analyze
    RMSInfo (rmsVolume),
    mkRMSInfo,
    updateInfo,
  )
where

import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Functor.Identity (Identity (runIdentity))
import Data.Vector.Storable qualified as SV
import GHC.Float (powerFloat)
import TinyDSP.Samples

data IIRParams = IIRParams
  { b0 :: {-# UNPACK #-} Float,
    b1 :: {-# UNPACK #-} Float,
    b2 :: {-# UNPACK #-} Float,
    a0 :: {-# UNPACK #-} Float,
    a1 :: {-# UNPACK #-} Float,
    a2 :: {-# UNPACK #-} Float
  }
  deriving (Show)

-- | A low-pass filter using cutoff frequency and resonance.
-- lowPassFilter :: Float -> Float -> IIRParams
-- lowPassFilter freq q =
--    IIRParams
--        { b0
--        , b1 = 1 - cos w0
--        , b2 = b0
--        , a0 = 1 + α
--        , a1 = - (2 * cos w0)
--        , a2 = 1 - α
--        }
--  where
--    b0 = (1 - cos w0) / 2
--    w0 = calcW0 freq
--    α = calcAQ w0 q
lowPassFilter :: Float -> Float -> IIRParams
lowPassFilter freq q =
  let w0 = calcW0 freq
      α = calcAQ w0 q
      cosW0 = cos w0
      sinW0 = sin w0
      b0 = sinW0 / 2
      commonFactor = 1 / (1 + α)
   in IIRParams
        { b0 = b0,
          b1 = (1 - cosW0) * commonFactor,
          b2 = b0,
          a0 = 1 + α,
          a1 = (-2) * cosW0 * commonFactor,
          a2 = (1 - α) * commonFactor
        }

-- highPassFilter :: Float -> Float -> IIRParams
-- highPassFilter freq q =
--     IIRParams
--         { b0
--         , b1 = (-1) * (1 + cos w0)
--         , b2 = b0
--         , a0 = 1 + α
--         , a1 = (-2) * cos w0
--         , a2 = 1 - α
--         }
--   where
--     b0 = (1 + cos w0) / 2
--     w0 = calcW0 freq
--     α = calcAQ w0 q

highPassFilter :: Float -> Float -> IIRParams
highPassFilter freq q =
  let w0 = calcW0 freq
      α = calcAQ w0 q
      cosW0 = cos w0
      b0 = (1 + cosW0) / 2
      neg2CosW0 = (-2) * cosW0
      oneMinusAlpha = 1 - α
   in IIRParams
        { b0 = b0,
          b1 = (-1) * (1 + cosW0),
          b2 = b0,
          a0 = 1 + α,
          a1 = neg2CosW0,
          a2 = oneMinusAlpha
        }

-- | BPF (constant skirt gain, peak gain = Q)
bandPassSkirtFilter :: Float -> Float -> IIRParams
bandPassSkirtFilter freq q =
  IIRParams
    { b0,
      b1 = 0,
      b2 = (-1) * b0,
      a0 = 1 + α,
      a1 = (-2) * cos w0,
      a2 = 1 - α
    }
  where
    b0 = sin w0 / 2
    w0 = calcW0 freq
    α = calcAQ w0 q

bandPassFilter :: Float -> Float -> IIRParams
bandPassFilter freq q =
  IIRParams
    { b0 = α,
      b1 = 0,
      b2 = (-1) * α,
      a0 = 1 + α,
      a1 = (-2) * cos w0,
      a2 = 1 - α
    }
  where
    w0 = calcW0 freq
    α = calcAQ w0 q

notchFilter :: Float -> Float -> IIRParams
notchFilter freq q =
  IIRParams
    { b0 = 1,
      b1 = (-2) * cos w0,
      b2 = 1,
      a0 = 1 + α,
      a1 = (-2) * cos w0,
      a2 = 1 - α
    }
  where
    w0 = calcW0 freq
    α = calcAQ w0 q

lowShelfFilter :: Float -> Float -> IIRParams
lowShelfFilter freq q =
  IIRParams
    { b0 = bigA * ((bigA + 1) - (bigA - 1) * cos w0 + bigAsq),
      b1 = 2 * bigA * ((bigA - 1) - (bigA + 1) * cos w0),
      b2 = bigA * ((bigA + 1) - (bigA - 1) * cos w0 - bigAsq),
      a0 = (bigA + 1) + (bigA - 1) * cos w0 + bigAsq,
      a1 = (-2) * ((bigA - 1) + (bigA + 1) * cos w0),
      a2 = (bigA + 1) + (bigA - 1) * cos w0 - bigAsq
    }
  where
    bigAsq = 2 * sqrt (bigA) * α
    w0 = calcW0 freq
    α = calcAQ w0 q

-- highShelfFilter :: Float -> Float -> IIRParams
-- highShelfFilter freq q =
--     IIRParams
--         { b0 = bigA * ((bigA + 1) + (bigA - 1) * cos w0 + bigAsq)
--         , b1 = 2 * bigA * ((bigA - 1) + (bigA + 1) * cos w0)
--         , b2 = bigA * ((bigA + 1) + (bigA - 1) * cos w0 - bigAsq)
--         , a0 = (bigA + 1) - (bigA - 1) * cos w0 + bigAsq
--         , a1 = 2 * ((bigA - 1) - (bigA + 1) * cos w0)
--         , a2 = (bigA + 1) - (bigA - 1) * cos w0 - bigAsq
--         }
--   where
--     bigAsq = 2 * sqrt (powerFloat 10 (dbGain / 40)) * α
--     w0 = calcW0 freq
--     α = calcAQ w0 q

highShelfFilter :: Float -> Float -> IIRParams
highShelfFilter freq q =
  let w0 = calcW0 freq
      α = calcAQ w0 q
      bigAsq = 2 * sqrt (powerFloat 10 (dbGain / 40)) * α
      bigA = bigAsq - 1
      cosW0 = cos w0
      b0Coeff = bigA * ((bigA + 1) + (bigA - 1) * cosW0 + bigAsq)
      b1Coeff = 2 * bigA * ((bigA - 1) + (bigA + 1) * cosW0)
      b2Coeff = bigA * ((bigA + 1) + (bigA - 1) * cosW0 - bigAsq)
      a0Coeff = (bigA + 1) - (bigA - 1) * cosW0 + bigAsq
      a1Coeff = 2 * ((bigA - 1) - (bigA + 1) * cosW0)
      a2Coeff = (bigA + 1) - (bigA - 1) * cosW0 - bigAsq
   in IIRParams
        { b0 = b0Coeff,
          b1 = b1Coeff,
          b2 = b2Coeff,
          a0 = a0Coeff,
          a1 = a1Coeff,
          a2 = a2Coeff
        }

dbGain, bigA :: Float
dbGain = -24
bigA = powerFloat 10 (dbGain / 40)

calcW0 :: Float -> Float
calcW0 freq = 2 * pi * freq / 44100

calcAQ :: Float -> Float -> Float
calcAQ w0 q = sin w0 / (2 * q)

data IIRState = IIRState
  { x0 :: {-# UNPACK #-} Float,
    x1 :: {-# UNPACK #-} Float,
    x2 :: {-# UNPACK #-} Float,
    y0 :: {-# UNPACK #-} Float,
    y1 :: {-# UNPACK #-} Float,
    y2 :: {-# UNPACK #-} Float
  }
  deriving (Show)

initialIIRState :: IIRState
initialIIRState = IIRState 0 0 0 0 0 0

-- applyIIR :: IIRParams -> Float -> IIRState -> IIRState
-- applyIIR (IIRParams b0 b1 b2 a0 a1 a2) x0 (IIRState x1 x2 _ y1 y2 _) = newState
--   where
--     newState = IIRState x0 x1 x2 newSample y1 y2
--     newSample = (b0 / a0) * x0 + (b1 / a0) * x1 + (b2 / a0) * x2 - (a1 / a0) * y1 - (a2 / a0) * y2

applyIIR :: IIRParams -> Float -> IIRState -> IIRState
applyIIR (IIRParams b0 b1 b2 a0 a1 a2) x0 (IIRState x1 x2 _ y1 y2 _) =
  let newSample = (b0 / a0) * x0 + (b1 / a0) * x1 + (b2 / a0) * x2 - (a1 / a0) * y1 - (a2 / a0) * y2
   in IIRState x0 x1 x2 newSample y1 y2

filterSamplesState ::
  forall m. (Monad m) => IIRParams -> Samples -> StateT IIRState m Samples
filterSamplesState params = SV.mapM doApplyIIR
  where
    doApplyIIR :: Float -> StateT IIRState m Float
    doApplyIIR curSample = StateT \curState -> do
      let newState = applyIIR params curSample curState
      pure (newState.y0, newState)

filterSamples :: IIRParams -> Samples -> IIRState -> (Samples, IIRState)
filterSamples params samples = runIdentity . runStateT (filterSamplesState @Identity params samples)

data RMSInfo = RMSInfo
  { state :: IIRState,
    params :: IIRParams,
    rmsVolume :: Float,
    rmsDecay :: Float
  }

mkRMSInfo :: IIRParams -> RMSInfo
mkRMSInfo params =
  RMSInfo
    { params,
      state = initialIIRState,
      rmsVolume = 0,
      rmsDecay = 0
    }

updateInfo :: RMSInfo -> Samples -> RMSInfo
updateInfo info samples =
  let (state, newVolume) = SV.foldl' doUpdateInfo (info.state, info.rmsVolume) samples
      (rmsVolume, rmsDecay)
        | newVolume > info.rmsVolume = (newVolume, newVolume / 4)
        | otherwise = (newVolume - info.rmsDecay, info.rmsDecay)
   in RMSInfo {state, params = info.params, rmsVolume, rmsDecay}
  where
    doUpdateInfo (prevState, prevVolume) sample =
      let newState = applyIIR info.params sample prevState
          curVolume = newState.y0 * newState.y0
       in (newState, max prevVolume curVolume)
