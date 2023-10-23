{-# LANGUAGE GADTs #-}


module Music.Time.TS where

import Data.Ratio
import Data.Bits ((.&.))


type Dur = Rational -- ^ Duration

data TS = TS { num :: Integer, den :: Integer }
    deriving (Eq, Ord, Show)


isValid :: TS -> Bool
isValid (TS n d) = n > 0 && d > 0

isPowOfTwo :: Integer -> Bool
isPowOfTwo n = n > 0 && n .&. (n - 1) == 0

isTSDenPowOfTwo :: TS -> Bool
isTSDenPowOfTwo (TS _ d) = isPowOfTwo d

toDur :: TS -> Dur
toDur (TS n d) = n % d
