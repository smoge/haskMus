{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Music.Time.TS where

import           Control.Lens
import           Data.Bits    ((.&.))
import           Data.Ratio

type Dur = Rational -- ^ Duration

-- | TimeSignature
data TS = TS
    { _num :: Integer -- ^ Numerator
    , _den :: Integer -- ^ Denominator
    } deriving (Eq, Ord)

makeLenses ''TS

instance Show TS where
    show (TS n d) = " " ++ show n ++ "//" ++ show d

infixr 7 //

-- | Time Signature
(//) :: Integer -> Integer -> TS
n // d = TS n d

-- | Convert TimeSignature to duration
timeSigToDur :: TS -> Dur
timeSigToDur (TS n d) = n % d

-- | Convert TimeSignature to duration
toDur :: TS -> Dur
toDur (TS n d) = n % d

-- | Convert duration to TimeSignature
fromDur :: Dur -> Integer -> TS
fromDur dur preferredDenominator
    | numeratorRatio == 0 = error "Cannot convert zero duration to time signature"
    | otherwise = durToTimeSig dur targetDenominator
    where
        numeratorRatio = numerator dur
        targetDenominator = max (denominator dur) preferredDenominator

-- | Apply a function to a TimeSignature
applyFunctionToTS :: (Dur -> Dur) -> TS -> TS
applyFunctionToTS f ts =
    let dur = timeSigToDur ts
        targetDenominator = findBestDenominator f dur
     in fromDur (f dur) targetDenominator

-- | Apply a function to a TimeSignature with a preferred denominator
applyFunctionToTS' :: (Dur -> Dur) -> Maybe Integer -> TS -> TS
applyFunctionToTS' f maybePreferredDenominator ts =
    case maybePreferredDenominator of
        Just preferredDenominator -> fromDur (f $ toDur ts) preferredDenominator
        Nothing -> fromDur (f $ toDur ts)  (ts ^. den)

-- | Convert duration to TimeSignature with a preferred denominator
fromDur' :: Dur -> Maybe Integer -> TS
fromDur' dur maybePreferredDenominator =
    case maybePreferredDenominator of
        Just preferredDenominator -> fromDur dur preferredDenominator
        Nothing -> fromDur dur (denominator dur)

-- | Convert duration to TimeSignature with a preferred denominator
durToTimeSig :: Dur -> Integer -> TS
durToTimeSig dur preferredDenominator
    | denominatorRatio == preferredDenominator =
        TS numeratorRatio denominatorRatio
    | otherwise = TS (numeratorRatio * multiplier) preferredDenominator
    where
        numeratorRatio = numerator dur
        denominatorRatio = denominator dur
        multiplier = preferredDenominator `div` denominatorRatio

-- | Find the best denominator for a given function and duration
findBestDenominator :: (Dur -> Dur) -> Dur -> Integer
findBestDenominator f dur =
    let currentDenominator = denominator dur
            newDur = f dur
            targetDenominator = denominator newDur
     in lcm currentDenominator targetDenominator


isValid :: TS -> Bool
isValid (TS n d) = n > 0 && d > 0

-- | Check if an integer is a power of two
isPowOfTwo :: Integer -> Bool
isPowOfTwo n = n > 0 && n Data.Bits..&. (n - 1) == 0

-- | Check if the denominator of a TimeSignature is a power of two
isTSDenPowOfTwo :: TS -> Bool
isTSDenPowOfTwo (TS _ d) = isPowOfTwo d

-- | Check if the denominator of a TimeSignature is a power of two
checkPowerOfTwo :: TS -> Bool
checkPowerOfTwo ts = isPowOfTwo $ ts ^. den

