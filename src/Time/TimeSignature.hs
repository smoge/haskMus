{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{- | This module defines a representation of musical time signatures and
 provides functionality to convert between 'TimeSignature' and 'Dur'
 (duration) types.
-}
module Time.TimeSignature (
    TimeSignature (..),
    time,
    (//),
    durToTimeSig,
    durToTimeSig8,
    timeSigToDur,
    HasDur (..),
) where

import Data.Bits ((.&.))
import Data.Default
import Data.Ratio
import Time.Dur

{- | A data type representing a time signature in music, which specifies the
 number of beats in a measure ('upper') and the note value that constitutes
 one beat ('lower').
-}
data TimeSignature = TimeSignature
    { -- | The number of beats in a measure.
      upper :: Integer,
      -- | The note value that constitutes one beat.
      lower :: Integer
    }
    deriving (Eq, Ord)

instance Show TimeSignature where
    show :: TimeSignature -> String
    show (TimeSignature n d) = "TimeSignature " <> show n <> "/" <> show d

infixr 7 //

{- | Infix constructor for 'TimeSignature'.

 ==== __Examples__

 >>> 4 // 4
 TimeSignature 4/4

 >>> 6 // 8
 TimeSignature 6/8
-}
(//) :: Integer -> Integer -> TimeSignature
n // d = time n d

{- | Creates a 'TimeSignature' from two integers.

 ==== __Examples__

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

    fromDur :: Dur -> TimeSignature
    fromDur d = numerator (unDur d) // denominator (unDur d)

{- | Converts a 'TimeSignature' to a 'Dur'.

 ==== __Examples__

 >>> timeSigToDur (3 // 4)
 Dur (3 % 4)

 >>> timeSigToDur (6 // 8)
 Dur (3 % 4)
-}
timeSigToDur :: TimeSignature -> Dur
timeSigToDur (TimeSignature n d) = Dur $ n % d

{- | Converts a 'Dur' to a 'TimeSignature' with a preferred denominator.

 ==== __Examples__

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

powersOfTwo :: [Integer]
powersOfTwo = fmap (2 ^) ([0 .. 10] :: [Integer])

isPowerOfTwo :: Integer -> Bool
isPowerOfTwo n = n > 0 && (n .&. (n - 1)) == 0
