{-# OPTIONS_GHC -Wno-type-defaults #-}
module Util.Rand where

import qualified Data.Vector as V
import System.Random (RandomGen, randomR)
import Data.List (foldl')

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
