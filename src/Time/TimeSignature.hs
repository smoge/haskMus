{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Time.TimeSignature (
    TimeSignature (..),
    time,
    (//),
    durToTimeSig,
    durToTimeSig8,
    timeSigToDur,
    HasDur (..),
) where

import Data.Default
import Data.Ratio
import Time.Dur

data TimeSignature = TimeSignature
    { upper :: Integer,
      lower :: Integer
    }
    deriving (Eq, Ord)

instance Show TimeSignature where
    show :: TimeSignature -> String
    show (TimeSignature n d) = show n <> "/" <> show d

infixr 7 //

(//) :: Integer -> Integer -> TimeSignature
n // d = time n d

time :: (Integral a) => a -> a -> TimeSignature
time x y = TimeSignature (fromIntegral x) (fromIntegral y)

instance Default TimeSignature where def = TimeSignature 4 8

instance HasDur TimeSignature where
    toDur :: TimeSignature -> Dur
    toDur = timeSigToDur

    setDur :: TimeSignature -> Dur -> TimeSignature
    setDur ts d = durToTimeSig (denominator $ unDur $ toDur ts) d

timeSigToDur :: TimeSignature -> Dur
timeSigToDur (TimeSignature n d) = Dur $ n % d

durToTimeSig :: (Integral a) => a -> Dur -> TimeSignature
durToTimeSig preferredDenominator d
    | denominatorRatio == preferredDenominator' =
        TimeSignature numeratorRatio denominatorRatio
    | otherwise =
        TimeSignature (numeratorRatio * multiplier) preferredDenominator'
    where
        preferredDenominator' = toInteger preferredDenominator
        numeratorRatio = numerator $ unDur d
        denominatorRatio = denominator $ unDur d
        multiplier = preferredDenominator' `quot` denominatorRatio

durToTimeSig8 :: Dur -> TimeSignature
durToTimeSig8 durat
    | den < 8 = durToTimeSig (8 :: Integer) durat
    | otherwise = durToTimeSig den durat
    where
        den = (denominator . unDur) durat
