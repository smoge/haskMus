{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Time.Pattern where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random (StdGen, random, randomRIO, mkStdGen, randomR)
import Data.List (scanl')
import Data.Maybe (fromMaybe)

import Debug.Trace (trace)
import Control.DeepSeq (deepseq)
import System.Clock (Clock(Monotonic), getTime, toNanoSecs)

type Time = Double
type Duration = Double

data Event = Event
  { eTime :: !Time
  , eDuration :: !Duration
  , eParameters :: !(Map String Double)
  } deriving (Show)

newtype Pattern a = Pattern { runPattern :: Time -> StdGen -> (a, StdGen) }

instance Functor Pattern where
  fmap f (Pattern p) = Pattern $ \t gen ->
    let (x, gen') = p t gen
    in (f x, gen')

-- Helper function to get a random value
getRandom :: (StdGen -> (a, StdGen)) -> Pattern a
getRandom f = Pattern $ \_ gen -> f gen

-- Pattern combinators
pwhite :: Double -> Double -> Pattern Double
pwhite min_ max_ = getRandom (randomR (min_, max_))

-- Brownian motion pattern.
pbrown :: Double -> Double -> Double -> Pattern Double
pbrown lo hi step = Pattern $ \_ gen ->
  let (r, gen') = randomR (-step, step) gen
      current = (lo + hi) / 2  -- Start in the middle of the range
      next = max lo (min hi (current + r))
  in (next, gen')

-- Scale patterns.
pscale :: [Double] -> Pattern Double
pscale scale = Pattern $ \t _ ->
  let i = floor t `mod` length scale
  in (scale !! i, mkStdGen (floor t))  -- Use time as seed for reproducibility

-- Bind multiple patterns into an event.
pbind :: Map String (Pattern Double) -> Pattern Event
pbind paramPatterns = Pattern $ \t gen ->
  let (paramValues, gen') = Map.foldrWithKey
        (\k v (acc, g) ->
          let (val, g') = runPattern v t g
          in ((k, val):acc, g'))
        ([], gen)
        paramPatterns
      params = Map.fromList paramValues
      dur = Map.findWithDefault 0 "dur" params
      event = Event t dur params
  in (event, gen')

midiToFreq :: Double -> Double
midiToFreq n = 440 * (2 ** ((n - 69) / 12))

normalize :: [Double] -> [Double]
normalize xs = fmap (/ totalSum) xs
  where
    totalSum = sum xs


normalizeSum :: [(a, Double)] -> [(a, Double)]
normalizeSum [] = error "normalizeSum: empty list of items" 
normalizeSum xs = fmap (\(x, w) -> (x, abs w / totalSum)) xs
  where
    totalSum = sum $ fmap (abs . snd) xs


pchoose :: [(Double, Double)] -> Pattern Double
pchoose weightedItemsOrig
    | null weightedItemsOrig = error "pchoose: empty list of items"
    | otherwise =
        let weightedItems = weightedItemsOrig
        in weightedItems `deepseq` Pattern $ \_ gen ->
            let totalWeight = sum $ fmap snd weightedItems
                cumWeights = case scanl' (+) 0 $ fmap snd weightedItems of
                  _ : xs -> xs
                  [] -> error "pchoose: empty list after scanl'"
                (r, gen') = randomR (0, totalWeight) gen
            in trace ("selected item with r: " <> show r) (selectItem r weightedItems cumWeights, gen')
  where
    selectItem _ [] _ = error "pchoose: empty list of items"
    selectItem r ((x, _):xs) (w:ws)
        | r <= w    = x
        | otherwise = selectItem r xs ws
    selectItem _ _ _ = error "pchoose: mismatched weights"

ensureNonZeroWeights :: [(a, Double)] -> [(a, Double)]
ensureNonZeroWeights [] = error "ensureNonZeroWeights: empty list of items"
ensureNonZeroWeights items =
    let minWeight = 1e-6  -- A small, non-zero value
    in fmap (\(x, w) -> (x, max w minWeight)) items


scheduleEvents :: [Event] -> IO ()
scheduleEvents events = do
  startTime <- getTime Monotonic
  mapM_ (playEvent startTime) events
  where
    playEvent startTime event = do
      currentTime <- getTime Monotonic
      let eventTime = fromIntegral (toNanoSecs startTime) / 1e9 + eTime event
          delay = max 0 $ eventTime - fromIntegral (toNanoSecs currentTime) / 1e9
      threadDelay $ floor $ delay * 1000000
      print event

runPatternIO :: Int -> Pattern Event -> IO [Event]
runPatternIO n p = do
  let initialGen = mkStdGen 42
      go _ 0 _ acc = pure (reverse acc)
      go t i gen acc = do
        let (event, gen') = runPattern p t gen
            nextT = t + eDuration event
        go nextT (i - 1) gen' (event : acc)
  go 0 n initialGen []

-- Transform a uniform random variable to an exponentially distributed one.
uniformToExponential :: Double -> Double -> Double
uniformToExponential λ u
  | u > 0 && u < 1 = (- log (1 - u)) / λ
  | otherwise = error "Uniform random variable must be in the range (0, 1)."

pexponential :: Double -> Pattern Double
pexponential lambda = getRandom $ \gen ->
  let (u, gen') = randomR (0.0, 1.0) gen
  in (uniformToExponential lambda u, gen')


main :: IO ()
main = do
    let durPattern = fmap (* 1.5) (pexponential 9)
        freqPattern = fmap midiToFreq (pbrown 60 72 1)
        ampPattern = pwhite 0.1 0.8
        rawScalePattern = normalizeSum [(60, 0.2), (62, -0.2), (64, 0.5), (65, 0.2), (67, 0.2)]
        scalePattern = pchoose $ ensureNonZeroWeights rawScalePattern


    let myPattern = pbind $ Map.fromList
            [ ("dur", durPattern)
            , ("freq", fmap midiToFreq scalePattern)
            , ("amp", ampPattern)
            , ("pan", pwhite (-1) 1)
            ]

    events <- runPatternIO 50 myPattern
    scheduleEvents events
    trace ("rawScalePattern: " <> show rawScalePattern) $ pure ()
