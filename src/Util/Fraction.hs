module Util.Fraction
  ( splitFraction,
    unsplitFraction,
    MixedNumber (..),
    toMixedNumber,
    fromMixedNumber,
  )
where

import Data.Ratio

splitFraction :: Rational -> (Integer, Rational)
splitFraction x = (wholePart, fractionalPart)
  where
    (wholePart, fractionalPart) = properFraction x

unsplitFraction :: (Integer, Rational) -> Rational
unsplitFraction (integerPart, fractionalPart) = fromInteger integerPart + fractionalPart

data MixedNumber = MixedNumber
  { integer :: Integer,
    fractional :: Ratio Integer
  }
  deriving (Eq)

toMixedNumber :: Rational -> MixedNumber
toMixedNumber x = MixedNumber wholePart fractionalPart
  where
    (wholePart, fractionalPart) = properFraction x

fromMixedNumber :: MixedNumber -> Ratio Integer
fromMixedNumber (MixedNumber whole frac) = fromInteger whole + frac

instance Show MixedNumber where
  show :: MixedNumber -> String
  show (MixedNumber whole frac)
    | whole == 0 = show (numerator frac) <> ("%" <> show (denominator frac))
    | frac == 0 = show whole
    | otherwise =
        "("
          <> show whole
          <> (" + " <> show (numerator frac) <> "%" <> show (denominator frac))
          <> ")"
