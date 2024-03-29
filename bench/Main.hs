
module Main where

import Criterion.Main

-- Optimized Fibonacci function using iteration.
fib :: Integer -> Integer
fib n | n < 0     = error "negative!"
      | n == 0    = 0
      | n == 1    = 1
      | otherwise = go n 0 1
  where
    go 0 a _ = a
    go 1 _ b = b
    go k a b = go (k-1) b (a+b)



fibOld :: Integer -> Integer
fibOld m | m < 0     = error "negative!"
      | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n-1) + go (n-2)


main :: IO ()
main = defaultMain [
  bgroup "fib" [ bench "1"  $ whnf fib 1
               , bench "5"  $ whnf fib 5
               , bench "9"  $ whnf fib 9
               ],
  bgroup "fibOld" [ bench "fibOld 1"  $ whnf fibOld 1
                  , bench "fibOld 5"  $ whnf fibOld 5
                  , bench "fibOld 9"  $ whnf fibOld 9
                  ]
  ]
