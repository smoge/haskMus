{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}

module Time.Duration where

import Control.Lens
import Data.Bits ((.&.))
import Data.Ord (comparing)
import Data.List (find, minimumBy, sortOn, unfoldr)
import Data.Data
import Data.Ratio
import Language.Haskell.TH.Syntax

newtype Division = Division {unDivision :: Integer} deriving (Eq, Show, Ord, Data, Lift)

newtype Dots = Dots {unDots :: Integer} deriving (Eq, Show, Ord, Data, Lift)

newtype Multiplier = Multiplier {unMultiplier :: Rational} deriving (Eq, Show, Ord, Data, Lift)

data Duration = Duration
  { _division :: Division,
    _dots :: Dots,
    _multiplier :: Rational
  }
  deriving (Eq, Show)

makeLenses ''Duration

divisionToRational :: Division -> Rational
divisionToRational = (1 %) . unDivision

durationToRational :: Duration -> Rational
durationToRational Duration {_division = d, _dots = dots, _multiplier = m} =
  1 % unDivision d * (1 + (2 ^ unDots dots - 1) % (2 ^ unDots dots)) * m

dotMultiplier :: Dots -> Rational
dotMultiplier (Dots d) = 1 + (2 ^ d - 1) % (2 ^ d)

dotsFromMultiplier :: Rational -> Dots
dotsFromMultiplier r = binarySearch 0 9
  where
    binarySearch :: Integer -> Integer -> Dots
    binarySearch low high
      | low > high = error "Invalid multiplier or too many dots"
      | midMultiplier == r = Dots mid
      | midMultiplier < r = binarySearch (mid + 1) high
      | otherwise = binarySearch low (mid - 1)
      where
        mid = (low + high) `div` 2
        midMultiplier = dotMultiplier (Dots mid)

durationToRat :: Duration -> Rational
durationToRat (Duration (Division divVal) dots m)
    | divVal == 0 = 0 % 1
    | otherwise = (1 % divVal) * dotMultiplier dots * m

-- | Compare Durations
compareDurations :: Duration -> Duration -> Ordering
compareDurations = Data.Ord.comparing durationToRational

areMultiplierEqual :: Duration -> Duration -> Bool
areMultiplierEqual dur1 dur2 = view multiplier dur1 == view multiplier dur2

instance Ord Duration where
    compare :: Duration -> Duration -> Ordering
    compare = compareDurations

addDot' :: Duration -> Integer -> Duration
addDot' dur m = dur & dots %~ (\(Dots n) -> Dots (n + m))


(+.) :: Duration -> Integer -> Duration
d +. i = addDot' d i

(-.) :: Duration -> Integer -> Duration
d -. i = addDot' d (negate i)


-- !MOVE TO ANOTHER MODULE:

orderByMusicalSimplicity :: [Rational] -> [Rational]
orderByMusicalSimplicity = sortOn musicalOrderHelper

musicalOrderHelper :: Rational -> (Int, Int, Int, Int, Int, Int)
musicalOrderHelper r =
    ( complexity
    , denom
    , numerLessThanDenom
    , absDiff
    , numer
    , powerOfTwo
    )
    where
        numer = negate $ fromIntegral (numerator r)
        denom = fromIntegral (denominator r)
        complexity = kolmogorovComplexityRational r
        absDiff = numer - denom
        numerLessThanDenom = if absDiff < 0 then 1 else 0
        powerOfTwo = if isPowerOfTwo denom then 1 else 0

kolmogorovComplexityRational :: Rational -> Int
kolmogorovComplexityRational r = length (primeFactors (numerator r)) + length (primeFactors (denominator r))

isPowerOfTwo :: Int -> Bool
isPowerOfTwo n = n > 0 && (n Data.Bits..&. (n - 1)) == 0

isPrime :: Integer -> Bool
isPrime n = n `elem` takeWhile (<= n) primes

primeFactors :: Integer -> [Integer]
primeFactors num = unfoldr f (testFactors num, num)
    where
        f (_, 1) = Nothing
        f (ps, n) = case find (\p -> (n `rem` p) == 0) ps of
            Nothing -> Just (n, ([], 1)) -- prime
            Just fact -> Just (fact, (dropWhile (< fact) ps, n `div` fact))


primes :: [Integer]
primes = 2 : 3 : filter (\n -> length (primeFactors n) == 1) [5, 7 ..]

testFactors :: Integer -> [Integer]
testFactors n = takeWhile ((<= n) . (square)) primes

square :: Integer -> Integer
square x = x * x
