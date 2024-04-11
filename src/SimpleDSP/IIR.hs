{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | This module implements IIR filter.
--
--  See: http://shepazu.github.io/Audio-EQ-Cookbook/audio-eq-cookbook.html
--   Or: https://www.w3.org/TR/audio-eq-cookbook/

module SimpleDSP.IIR
  ( 
    filterSamples,
    IIRParams,
    initialIIRState,
    IIRState,

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
import SimpleDSP.Samples (Samples)

data IIRParams = IIRParams
  { b0 :: {-# UNPACK #-} Float,
    b1 :: {-# UNPACK #-} Float,
    b2 :: {-# UNPACK #-} Float,
    a0 :: {-# UNPACK #-} Float,
    a1 :: {-# UNPACK #-} Float,
    a2 :: {-# UNPACK #-} Float
  }
  deriving (Show)

dbGain, bigA :: Float
dbGain = -24
bigA = powerFloat 10 (dbGain / 40)

{-# INLINE calcW0 #-}
calcW0 :: Float -> Float
calcW0 freq = 2 * pi * freq / 44100

{-# INLINE calcAQ #-}
calcAQ :: Float -> Float -> Float
calcAQ w0 q = sin w0 / (2 * q)


lowPassFilter :: Float -> Float -> IIRParams
lowPassFilter freq q =
  IIRParams
    { b0 = b0_calc,
      b1 = 1 - cosW0,
      b2 = b0_calc,
      a0 = 1 + alpha,
      a1 = (-2) * cosW0,
      a2 = 1 - alpha
    }
  where
    w0 = calcW0 freq
    cosW0 = cos w0
    alpha = calcAQ w0 q
    b0_calc = (1 - cosW0) / 2

highPassFilter :: Float -> Float -> IIRParams
highPassFilter freq q =
  IIRParams
    { b0,
      b1 = (-1) * (1 + cosWO),
      b2 = b0,
      a0 = 1 + α,
      a1 = (-2) * cosWO,
      a2 = 1 - α
    }
  where
    b0 = (1 + cosWO) / 2
    w0 = calcW0 freq
    α = calcAQ w0 q
    cosWO = cos w0

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
      b1 = (-2) * cosWO,
      b2 = 1,
      a0 = 1 + α,
      a1 = (-2) * cosWO,
      a2 = 1 - α
    }
  where
    w0 = calcW0 freq
    α = calcAQ w0 q
    cosWO = cos w0

lowShelfFilter :: Float -> Float -> IIRParams
lowShelfFilter freq q =
  IIRParams
    { b0 = bigA * ((bigA + 1) - (bigA - 1) * cosWO + bigAsq),
      b1 = 2 * bigA * ((bigA - 1) - (bigA + 1) * cosWO),
      b2 = bigA * ((bigA + 1) - (bigA - 1) * cosWO - bigAsq),
      a0 = (bigA + 1) + (bigA - 1) * cosWO + bigAsq,
      a1 = (-2) * ((bigA - 1) + (bigA + 1) * cosWO),
      a2 = (bigA + 1) + (bigA - 1) * cosWO - bigAsq
    }
  where
    bigAsq = 2 * sqrt bigA * α
    w0 = calcW0 freq
    α = calcAQ w0 q
    cosWO = cos w0

highShelfFilter :: Float -> Float -> IIRParams
highShelfFilter freq q =
  IIRParams
    { b0 = bigA * ((bigA + 1) + (bigA - 1) * cosWO + bigAsq),
      b1 = 2 * bigA * ((bigA - 1) + (bigA + 1) * cosWO),
      b2 = bigA * ((bigA + 1) + (bigA - 1) * cosWO - bigAsq),
      a0 = (bigA + 1) - (bigA - 1) * cosWO + bigAsq,
      a1 = 2 * ((bigA - 1) - (bigA + 1) * cosWO),
      a2 = (bigA + 1) - (bigA - 1) * cosWO - bigAsq
    }
  where
    bigAsq = 2 * sqrt bigA * α
    w0 = calcW0 freq
    α = calcAQ w0 q
    cosWO = cos w0


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

-- Apply IIR filter to a single sample.
{-# INLINE applyIIR #-}
applyIIR :: IIRParams -> Float -> IIRState -> IIRState
applyIIR (IIRParams b0 b1 b2 a0 a1 a2) x0 (IIRState x1 x2 _ y1 y2 _) = newState
  where
    newState = IIRState x0 x1 x2 newSample y1 y2
    newSample = (b0 / a0) * x0 + (b1 / a0) * x1 + (b2 / a0) * x2 - (a1 / a0) * y1 - (a2 / a0) * y2

-- Filter samples with StateT monad
filterSamplesState :: forall m. (Monad m) => IIRParams -> Samples -> StateT IIRState m Samples
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
