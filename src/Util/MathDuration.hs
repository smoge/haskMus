-- Separate module named MathUtils.hs

module Util.MathDuration where

import Data.Bits ((.&.))
import Data.List (find, sortOn, unfoldr)
import Data.Ratio

-- orderByMusicalSimplicity :: [Rational] -> [Rational]
-- orderByMusicalSimplicity = sortOn musicalOrderHelper

musicalOrderHelper :: Rational -> (Int, Int, Int, Int, Int, Int)
musicalOrderHelper r =
  ( complexity,
    denom,
    numerLessThanDenom,
    absDiff,
    numer,
    powerOfTwo
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
isPowerOfTwo n = n > 0 && (n .&. (n - 1)) == 0

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
testFactors n = takeWhile ((<= n) . square) primes

square :: Integer -> Integer
square x = x * x
