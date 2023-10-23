{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Music.Time.TS where

import           Control.Lens
import           Data.Bits    ((.&.))
import           Data.Ratio

type Duration = Rational

-- | Represents a time signature with an upper and lower number.
data TimeSignature = TimeSignature
    { _upper :: Integer -- ^ TS Upper Number
    , _lower :: Integer -- ^ TS Lower Number
    } deriving (Eq, Ord, Show)

makeLenses ''TimeSignature

-- instance Show TimeSignature where
--     show (TimeSignature n d) = " " ++ show n ++ "//" ++ show d

-- | Create a TimeSignature from two integers.
infixr 7 //
(//) :: Integer -> Integer -> TimeSignature
n // d = TimeSignature n d


-- | Convert a TimeSignature to a Duration.
--
-- >>> toDur $ TimeSignature 4 8
-- 1 % 2
toDur :: TimeSignature -> Duration
toDur (TimeSignature n d) = n % d

-- | Convert a Duration to a TimeSignature with a preferred denominator.
--
-- >>> fromDur (1%2) 8
--  4//8
fromDur :: Duration -> Integer -> TimeSignature
fromDur dur preferredDenominator
    | numeratorRatio == 0 = error "Cannot convert zero Duration to TimeSignature"
    | otherwise = durToTimeSig dur targetDenominator
    where
        numeratorRatio = numerator dur
        targetDenominator = max (denominator dur) preferredDenominator

-- | Convert a TimeSignature to a Duration.
timeSigToDur :: TimeSignature -> Duration
timeSigToDur (TimeSignature n d) = n % d


-- | Convert duration to TimeSignature with a preferred denominator
durToTimeSig :: Duration -> Integer -> TimeSignature
durToTimeSig dur preferredDenominator
    | denominatorRatio == preferredDenominator =
      TimeSignature numeratorRatio denominatorRatio
    | otherwise = TimeSignature (numeratorRatio * multiplier) preferredDenominator
    where
        numeratorRatio = numerator dur
        denominatorRatio = denominator dur
        multiplier = preferredDenominator `div` denominatorRatio

-- | Convert a Duration to a TimeSignature with an optional preferred denominator.
--
-- >>> fromDur' (3%4) (Just 8)
--  6//8
fromDur' :: Duration -> Maybe Integer -> TimeSignature
fromDur' dur maybePreferredDenominator =
    case maybePreferredDenominator of
        Just preferredDenominator -> fromDur dur preferredDenominator
        Nothing -> fromDur dur (denominator dur)


-- | Convert a Duration to a TimeSignature returns Maybe TimeSignature.
--
-- >>> fromDur'' (0%2) 8
-- Nothing
-- >>> fromDur'' (1%4) 8 
fromDur'' :: Duration -> Integer -> Maybe TimeSignature
fromDur'' dur preferredDenominator
    | numeratorRatio == 0 = Nothing
    | otherwise = Just $ fromDur dur targetDenominator
    where
        numeratorRatio = numerator dur
        targetDenominator = max (denominator dur) preferredDenominator



-- | Convert a Duration to a TimeSignature returns Maybe TimeSignature.
--
-- >>> fromDur'' (0%1) 8

-- >>> fromDur'' (1%4) 8 
-- | Apply a function to a TimeSignature.
--
-- >>> applyFunctionToTS (+ (1%8)) (4//8)
--  5//8
applyFunctionToTS :: (Duration -> Duration) -> TimeSignature -> TimeSignature
applyFunctionToTS f ts =
    let dur = timeSigToDur ts
        targetDenominator = findBestDenominator f dur
    in fromDur (f dur) targetDenominator

-- | Apply a function to a TimeSignature with a preferred denominator.
--
-- >>> applyFunctionToTS' (+ (1%8)) (Just 16) (4//8)
--  10//16
applyFunctionToTS' :: (Duration -> Duration) -> Maybe Integer -> TimeSignature -> TimeSignature
applyFunctionToTS' f maybePreferredDenominator ts =
    case maybePreferredDenominator of
        Just preferredDenominator -> fromDur (f $ toDur ts) preferredDenominator
        Nothing -> fromDur (f $ toDur ts)  (ts ^. lower)


-- | Find the best denominator for a given function and duration.
--
-- >>> findBestDenominator (+ (1%8)) (4/8)
-- 8
findBestDenominator :: (Duration -> Duration) -> Duration -> Integer
findBestDenominator f dur =
    let currentDenominator = denominator dur
        newDur = f dur
        targetDenominator = denominator newDur
    in lcm currentDenominator targetDenominator


-- | Check if a TimeSignature is valid.
--
-- >>> isValid (4//4)
-- True
-- >>> isValid (TimeSignature (-4) 4)
-- False
-- >>> isValid (TimeSignature 4 0)
-- False
isValid :: TimeSignature -> Bool
isValid (TimeSignature n d) = n > 0 && d > 0 && isPowOfTwo d


-- | Check if an integer is a power of two.
isPowOfTwo :: Integer -> Bool
isPowOfTwo n = n > 0 && n Data.Bits..&. (n - 1) == 0


-- | Check if the denominator of a TimeSignature is a power of two.
tsLowerPowerOfTwo :: TimeSignature -> Bool
tsLowerPowerOfTwo ts = isPowOfTwo $ ts ^. lower


----------------------------------------------------------------------------
-- # SECTION Examples
----------------------------------------------------------------------------
{- 
-- | Create a TimeSignature.
tsA :: TimeSignature
tsA = 4 // 4

-- | Apply a function (+ 1%8) to tsA.
tsB :: TimeSignature
tsB = applyFunctionToTS (+ (1%8)) tsA

-- | Apply a function (* 2) to tsA with a preferred denominator of 8.
tsC :: TimeSignature
tsC = applyFunctionToTS' (* 2) (Just 8) tsA

-- | Convert a Duration (4%8) to a TimeSignature with a preferred denominator of 8.
tsD :: TimeSignature
tsD = fromDur (4%8) 8

-- | Convert a Duration (1%2) to a TimeSignature without specifying a denominator.
tsE :: TimeSignature
tsE = fromDur' (1%2) Nothing
 -}