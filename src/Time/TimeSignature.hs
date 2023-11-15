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
    { 
      upper :: Integer,
      
      lower :: Integer
    }
    deriving (Eq, Ord)

instance Show TimeSignature where
    show :: TimeSignature -> String
    show (TimeSignature n d) = "TimeSignature " <> show n <> "/" <> show d

infixr 7 //

{- | Infix constructor for 'TimeSignature'.
 >>> 4 // 4
 TimeSignature 4/4
-}
(//) :: Integer -> Integer -> TimeSignature
n // d = time n d

{- | Creates a 'TimeSignature' from two integers.
 >>> time 3 4
 TimeSignature 3/4
-}
time :: (Integral a) => a -> a -> TimeSignature
time x y = TimeSignature (toInteger x) (toInteger y)

{- | Default value for 'TimeSignature'
 >>> def :: TimeSignature
 TimeSignature 4/8
-}
instance Default TimeSignature where def = TimeSignature 4 8

-- | Conversion between 'TimeSignature' and 'Dur'.
instance HasDur TimeSignature where
    toDur :: TimeSignature -> Dur
    toDur = timeSigToDur

    setDur ::  TimeSignature -> Dur -> TimeSignature
    setDur ts d = durToTimeSig (denominator $ unDur $ toDur ts) d

{- | Converts a 'TimeSignature' to a 'Dur'.
 >>> timeSigToDur (3 // 4)
 Dur (3 % 4)

 >>> timeSigToDur (6 // 8)
 Dur (3 % 4)
-}
timeSigToDur :: TimeSignature -> Dur
timeSigToDur (TimeSignature n d) = Dur $ n % d

{- | Converts a 'Dur' to a 'TimeSignature' with a preferred denominator.

 >>> durToTimeSig 4 (Dur (1 % 2))
 TimeSignature 2/4

 >>> durToTimeSig 8 (Dur (3 % 4))
 TimeSignature 6/8
-}

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
        multiplier = preferredDenominator' `div` denominatorRatio

{- | Converts a 'Dur' to a 'TimeSignature', defaulting the denominator to 8.

 ==== __Examples__

 >>> durToTimeSig8 (Dur (1 % 2))
 >>> durToTimeSig8 (Dur (3 % 16))
 TimeSignature 4/8
 TimeSignature 3/16
-}

durToTimeSig8 :: Dur -> TimeSignature
durToTimeSig8 durat
    | den <= 8 = durToTimeSig (8 :: Integer) durat
    | otherwise = durToTimeSig den durat
    where
        den = (denominator . unDur) durat

