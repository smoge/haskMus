{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isNothing" #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Music.Time.TS where

import           Control.Lens    hiding (elements)
import           Data.Bits       ((.&.))
import           Data.Ratio
import           Test.QuickCheck (Arbitrary (arbitrary), Gen,
                                  Positive (Positive, getPositive), Property,
                                  elements, quickCheck, suchThat, verboseCheck,
                                  (==>))

newtype Duration = Duration
  { unDuration :: Rational
  } deriving (Eq, Ord, Num, Fractional)

instance Show Duration where
  show (Duration r) = show (numerator r) ++ " %/ " ++ show (denominator r)

class HasDuration a where
  toDuration :: a -> Duration
  fromDuration :: Duration -> a
  default fromDuration :: (a ~ Duration) => Duration -> a
  fromDuration = id

instance HasDuration Duration where
  toDuration = id
  fromDuration = id

dur :: Rational -> Duration
dur = Duration

infix 7 %/

(%/) :: Integer -> Integer -> Duration
n %/ d = dur (n % d)

-- >>> [1, 1, 1, 1, 1]|/ 8 :: [Duration]
-- [1 %/ 8,1 %/ 8,1 %/ 8,1 %/ 8,1 %/ 8]
-- >>> [1%/2, 1%/2, 1%/2]|/ 8
-- [1 %/ 16,1 %/ 16,1 %/ 16]
-- >>> [1%/2, 1%/2, 1%/2]|/ 8
-- [1 %/ 16,1 %/ 16,1 %/ 16]
(|/) :: (HasDuration a) => [a] -> Integer -> [a]
durations |/ d =
  map
    (\durVal ->
       let durRational = durationToRational (toDuration durVal)
           divisor = fromInteger d
        in fromDuration $ dur (durRational / divisor))
    durations

-- >>> [1%/64, 1%/64, 1%/64]|* 2 :: [Duration]
-- [1 %/ 32,1 %/ 32,1 %/ 32]
(|*) :: (HasDuration a) => [a] -> Integer -> [a]
durations |* d =
  map
    (\durVal ->
       let durRational = durationToRational (toDuration durVal)
           divisor = fromInteger d
        in fromDuration $ dur (durRational * divisor))
    durations

durationToRational :: Duration -> Rational
durationToRational (Duration r) = r

-- | Represents a time signature with an upper and lower number.
data TimeSignature = TimeSignature
  { _upper :: Integer -- ^ TimeSignature Upper Number
  , _lower :: Integer -- ^ TimeSignature Lower Number
  } deriving (Eq, Ord)

makeLenses ''TimeSignature

time :: Integer -> Integer -> TimeSignature
time = TimeSignature

instance Show TimeSignature where
  show (TimeSignature n d) = "TS " ++ show n ++ "//" ++ show d

infixr 7 //

-- | Create a TimeSignature from two integers.
(//) :: Integer -> Integer -> TimeSignature
n // d = TimeSignature n d

-- | Convert a TimeSignature to a Duration.
--
-- >>> toDur $ TimeSignature 4 8
-- 1 %/ 2
toDur :: TimeSignature -> Duration
toDur (TimeSignature n d) = Duration $ n % d

-- | Convert a Duration to a TimeSignature with a preferred denominator.
--
-- >>> fromDur (1%/2) 8
-- TS 4//8
fromDur :: Duration -> Integer -> TimeSignature
fromDur d preferredDenominator
  | numeratorRatio == 0 = error "Cannot convert zero Duration to TimeSignature"
  | otherwise = durToTimeSig d targetDenominator
  where
    numeratorRatio = numerator $ unDuration d
    targetDenominator = max (denominator $ unDuration d) preferredDenominator

-- | Convert a TimeSignature to a Duration.
timeSigToDur :: TimeSignature -> Duration
timeSigToDur (TimeSignature n d) = Duration $ n % d

-- | Convert duration to TimeSignature with a preferred denominator
durToTimeSig :: Duration -> Integer -> TimeSignature
durToTimeSig d preferredDenominator
  | denominatorRatio == preferredDenominator =
    TimeSignature numeratorRatio denominatorRatio
  | otherwise = TimeSignature (numeratorRatio * multiplier) preferredDenominator
  where
    numeratorRatio = numerator $ unDuration d
    denominatorRatio = denominator $ unDuration d
    multiplier = preferredDenominator `div` denominatorRatio

-- | Convert a Duration to a TimeSignature with an optional preferred denominator.
--
-- >>> fromDur' (3%/4) (Just 8)
-- TS 6//8
fromDur' :: Duration -> Maybe Integer -> TimeSignature
fromDur' d maybePreferredDenominator =
  case maybePreferredDenominator of
    Just preferredDenominator -> fromDur d preferredDenominator
    Nothing                   -> fromDur d denom
  where
    denom = denominator $ unDuration d

-- | Convert a Duration to a TimeSignature returns Maybe TimeSignature.
--
-- >>> fromDur'' (dur 0/2) 8
-- Nothing
-- >>> fromDur'' (dur 1/4) 8
-- Just TS 2//8
fromDur'' :: Duration -> Integer -> Maybe TimeSignature
fromDur'' d preferredDenominator
  | numeratorRatio == 0 = Nothing
  | otherwise = Just $ fromDur d targetDenominator
  where
    numeratorRatio = numerator (unDuration d)
    targetDenominator = max (denominator $ unDuration d) preferredDenominator

-- | Convert a Duration to a TimeSignature returns Maybe TimeSignature.
--
-- >>> fromDur'' (dur 0/1) 8
-- Nothing
-- >>> fromDur'' (dur 1/4) 8
-- Just TS 2//8
--
-- >>> applyFunctionToTS (+ (dur 1/8)) (4//8)
-- TS 5//8
applyFunctionToTS :: (Duration -> Duration) -> TimeSignature -> TimeSignature
applyFunctionToTS f ts =
  let d = timeSigToDur ts
      targetDenominator = findBestDenominator f d
   in fromDur (f d) targetDenominator

-- | Apply a function to a TimeSignature with a preferred denominator.
--
-- >>> applyFunctionToTS' (+ (dur 1/8)) (Just 16) (4//8)
-- TS 10//16
applyFunctionToTS' ::
     (Duration -> Duration) -> Maybe Integer -> TimeSignature -> TimeSignature
applyFunctionToTS' f maybePreferredDenominator ts =
  case maybePreferredDenominator of
    Just preferredDenominator -> fromDur (f $ toDur ts) preferredDenominator
    Nothing                   -> fromDur (f $ toDur ts) (ts ^. lower)

-- | Find the best denominator for a given function and duration.
--
-- >>> findBestDenominator (+ dur (1/8)) (dur (4/8))
-- 8
findBestDenominator :: (Duration -> Duration) -> Duration -> Integer
findBestDenominator f d =
  let currentDenominator = denominator $ unDuration d
      newDur = f d
      targetDenominator = denominator $ unDuration newDur
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

--------------------------------------
-- # SECTION QuickCheck
--------------------------------------
instance Arbitrary TimeSignature where
  arbitrary = do
    (Positive n) <- arbitrary
    d <- elements [2 ^ x | x <- ([0 .. 7] :: [Integer])]
    return (n // d)

instance Arbitrary Duration where
  arbitrary :: Gen Duration
  arbitrary = do
    n <- getPositive <$> arbitrary `suchThat` (>= Positive 1)
    denominatorPower <- elements ([0 .. 7] :: [Integer]) -- for denominators 1, 2, 4, ..., 128
    let d = 2 ^ denominatorPower
    return $ Duration (n % d)

-- | Conversion from TimeSignature to Duration and back should be an identity:
prop_toDur_fromDur_identity :: TimeSignature -> Bool
prop_toDur_fromDur_identity ts =
  let d = toDur ts
      ts' = fromDur d (ts ^. lower)
   in ts == ts'

-- | fromDur' without a preferred denominator, it should be the
-- same as using fromDur with the denominator of the given duration:
prop_fromDur_fromDur' :: Duration -> Bool
prop_fromDur_fromDur' d =
  fromDur' d Nothing == fromDur d (denominator $ unDuration d)

-- When a duration's numerator is 0, fromDur'' should return Nothing:
prop_fromDur''_zero_duration :: Integer -> Bool
prop_fromDur''_zero_duration denom = fromDur'' 0 denom == Nothing

-- prop_applyFunction_identity :: TimeSignature -> Bool
-- prop_applyFunction_identity ts = applyFunctionToTS id ts == ts
-- A valid time signature should have a denominator that's a power of two:
prop_valid_ts_power_of_two :: TimeSignature -> Property
prop_valid_ts_power_of_two ts = isValid ts ==> tsLowerPowerOfTwo ts

main :: IO ()
main = do
  quickCheck prop_toDur_fromDur_identity
  quickCheck prop_fromDur_fromDur'
  quickCheck prop_fromDur''_zero_duration
--   verboseCheck prop_applyFunction_identity
  quickCheck prop_valid_ts_power_of_two
