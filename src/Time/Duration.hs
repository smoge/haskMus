{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}

module Time.Duration where

import Control.Lens
import Data.Data
import Data.List (sortOn)
import Data.Ord (comparing)
import Data.Ratio
import Language.Haskell.TH.Syntax
import Util.MathDuration

-- | Represents a musical division.
newtype Division = Division {unDivision :: Integer} deriving (Eq, Show, Ord, Data, Lift)

-- | Represents the number of dots in a musical notation, affecting the duration.
newtype Dots = Dots {unDots :: Integer} deriving (Eq, Show, Enum, Ord, Data, Lift)

newtype Multiplier = Multiplier {unMultiplier :: Rational} deriving (Eq, Show, Ord, Data, Lift)

data Duration = Duration
  { _division :: Division,
    _dots :: Dots,
    _multiplier :: Rational
  }
  deriving (Eq, Show)

makeLenses ''Duration


-- | Creates a 'Dots' instance ensuring the value is non-negative.
makeDots :: Integer -> Maybe Dots
makeDots n
  | n >= 0    = Just $ Dots n
  | otherwise = Nothing


-- | Convert a 'Division' to a 'Rational'
divisionToRational :: Division -> Rational
divisionToRational = (1 %) . unDivision

-- | Convert a 'Duration' to a 'Rational'
durationToRational :: Duration -> Rational
durationToRational Duration {_division = d, _dots = dots_, _multiplier = m} =
  1 % unDivision d * (1 + (2 ^ unDots dots_ - 1) % (2 ^ unDots dots_)) * m

-- | Calculate the multiplier for a given number of dots
dotMultiplier :: Dots -> Rational
dotMultiplier (Dots d) = 1 + (2 ^ d - 1) % (2 ^ d)

-- dotsFromMultiplier :: Rational -> Dots
-- dotsFromMultiplier r = binarySearch 0 9
--   where
--     binarySearch :: Integer -> Integer -> Dots
--     binarySearch low high
--       | low > high = error "Invalid multiplier or too many dots"
--       | midMultiplier == r = Dots mid
--       | midMultiplier < r = binarySearch (mid + 1) high
--       | otherwise = binarySearch low (mid - 1)
--       where
--         mid = (low + high) `div` 2
--         midMultiplier = dotMultiplier (Dots mid)

-- | Given a rational number, returns the corresponding dots value.
--   If the rational number is negative, returns Nothing.
--   Uses binary search to find the dots value.
--   The search is performed between 0 and 9.
--   The function caches the dotMultiplier values for each integer between 0 and 9.
--   The cache is used to speed up the search.
dotsFromMultiplier :: Rational -> Maybe Dots
dotsFromMultiplier r
  | r < 0 = Nothing  -- check for negative rationals
  | otherwise = binarySearch 0 9
  where
    -- Cache for dotMultiplier, converting each integer to Dots
    cache = map (dotMultiplier . Dots) [0..9]

    binarySearch :: Integer -> Integer -> Maybe Dots
    binarySearch low high
      | low > high = Nothing
      | midMultiplier == r = Just $ Dots mid
      | midMultiplier < r = binarySearch (mid + 1) high
      | otherwise = binarySearch low (mid - 1)
      where
        mid = (low + high) `div` 2
        midMultiplier = cache !! fromIntegral mid


-- | Convert a 'Duration' to a 'Rational'
durationToRat :: Duration -> Rational
durationToRat (Duration (Division divVal) dots_ m)
  | divVal == 0 = 0 % 1
  | otherwise = (1 % divVal) * dotMultiplier dots_ * m

-- | Check if the multipliers of two durations are equal
areMultiplierEqual :: Duration -> Duration -> Bool
areMultiplierEqual dur1 dur2 = view multiplier dur1 == view multiplier dur2

-- | Custom 'Ord' instance for 'Duration'
instance Ord Duration where
  -- | Compare two durations based on their 'Rational' representation
  compare :: Duration -> Duration -> Ordering
  compare = comparing durationToRational

-- | Add a specified number of dots to a 'Duration'
addDotsToDuration :: Duration -> Integer -> Duration
addDotsToDuration dur m = dur & dots %~ (\(Dots n) -> Dots (n + m))

-- | Operator for adding dots to a 'Duration'
(+.) :: Duration -> Integer -> Duration
d +. i = addDotsToDuration d i

-- | Operator for subtracting dots from a 'Duration'
(-.) :: Duration -> Integer -> Duration
d -. i = addDotsToDuration d (negate i)

-- | Order a list of 'Rational' numbers based on their musical simplicity
orderByMusicalSimplicity :: [Rational] -> [Rational]
orderByMusicalSimplicity = sortOn musicalOrderHelper
