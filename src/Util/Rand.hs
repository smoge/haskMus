{-# OPTIONS_GHC -Wno-type-defaults #-}
module Util.Rand where

import qualified Data.Vector as V
import System.Random (RandomGen, randomR, Random, mkStdGen)
import Data.List (foldl')


-- | Fisher-Yates shuffle algorithm for randomly permuting a list.
--
-- This algorithm generates a random permutation of a finite sequence in O(n) time.
--
-- Example:
--
-- >>> let g = mkStdGen 42
-- >>> let originalList = [1..10]
-- >>> let (shuffledList, newGen) = shuffle originalList g
-- >>> shuffledList

shuffle :: forall g a. RandomGen g => [a] -> g -> ([a], g)
shuffle xs g =
    let v = V.fromList xs
        indices = [V.length v - 1, V.length v - 2 .. 1] :: [Int]
        (v', g') = foldl' step (v, g) indices
    in (V.toList v', g')
  where
    step :: (V.Vector a, g) -> Int -> (V.Vector a, g)
    step (v, gg) i =
        let (j, g') = randomR (0, i) gg
            vi = v V.! i   -- Fetch the element at index i
            vj = v V.! j   -- Fetch the element at index j
        in (v V.// [(i, vj), (j, vi)], g')  -- Swap elements at indices i and j

---- | Computes the mean of a list of numbers.
mean :: (Fractional a) => [a] -> a
mean xs = sum xs / fromIntegral (length xs)
--
---- | Computes the variance of a list of numbers.
variance :: (Floating a) => [a] -> a
variance xs =
    let m = mean xs
        n = fromIntegral (length xs)
    in sum [(x - m)^2 | x <- xs] / (n - 1)

---- | Computes the standard deviation of a list of numbers.
stdDev :: (Floating a) => [a] -> a
stdDev = sqrt . variance

--
-- | Computes the covariance of two lists of numbers.
covariance :: (Floating a) => [a] -> [a] -> a
covariance xs ys
    | length xs /= length ys = error "Lists must have the same length"
    | otherwise =
        let mx = mean xs
            my = mean ys
            n = fromIntegral (length xs) - 1
        in sum [(x - mx) * (y - my) | (x, y) <- zip xs ys] / n


-- | Computes the Pearson correlation coefficient of two lists of numbers.
correlation :: (Floating a) => [a] -> [a] -> a
correlation xs ys = covariance xs ys / (stdDev xs * stdDev ys)


-- | Generates a random number from a normal (Gaussian) distribution.
--
-- The normal distribution is characterized by its mean and standard deviation.
-- It's widely used in statistics and natural sciences to represent real-valued
-- random variables whose distributions are not known.
--
-- Example:
--
-- >>> let g = mkStdGen 42
-- >>> let (value, newGen) = normalRandom 0 1 g
-- > -- value is a random number from a standard normal distribution (mean 0, std dev 1)
normalRandom :: (RandomGen g, Floating a, Random a) => a -> a -> g -> (a, g)
normalRandom mean_ stdDev_ g =
    let (u1, g1) = randomR (0, 1) g
        (u2, g2) = randomR (0, 1) g1
        z0 = sqrt (- (2 * log u1)) * cos (2 * pi * u2)
    in (mean_ + stdDev_ * z0, g2)



-- Basic random number generation functions

-- | Generates a random number between 0 and n.
rand :: (RandomGen g, Random n, Num n) => n -> g -> (n, g)
rand n = randomR (0, n)

-- | Generates a list of k random numbers between 0 and n.
nrand :: (RandomGen g, Random n, Num n) => Int -> n -> g -> ([n], g)
nrand k = kvariant k . rand

-- | Generates a random number between -n and n.
rand2 :: (RandomGen g, Random n, Num n) => n -> g -> (n, g)
rand2 n = randomR (-n, n)

-- | Generates a list of k random numbers between -n and n.
nrand2 :: (RandomGen g, Random a, Num a) => Int -> a -> g -> ([a], g)
nrand2 k = kvariant k . rand2

-- | Generates a random number in the range [l, r].
rrand :: (Random n, RandomGen g) => n -> n -> g -> (n, g)
rrand = curry randomR

mk_kvariant :: r -> (t -> r -> r) -> (r -> s) -> Int -> (g -> (t, g)) -> g -> (s, g)
mk_kvariant k_nil k_join un_k k f = go k_nil k
  where
    go x i g
      | i == 0 = (un_k x, g)
      | otherwise = let (y, g') = f g in go (k_join y x) (i - 1) g'

-- | Generates a list of random values.
kvariant :: Int -> (g -> (a, g)) -> g -> ([a], g)
kvariant = mk_kvariant [] (:) id