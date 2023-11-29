module Util.Fraction (
    splitFraction,
    unsplitFraction,
) where

--import Data.Ratio 

splitFraction :: Rational -> (Integer, Rational)
splitFraction x = (wholePart, fractionalPart)
    where
        (wholePart, fractionalPart) = properFraction x

unsplitFraction :: (Integer, Rational) -> Rational
unsplitFraction (integerPart, fractionalPart) = fromInteger integerPart + fractionalPart
