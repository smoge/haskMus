{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}

module Time.Duration where

import Control.Lens
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
