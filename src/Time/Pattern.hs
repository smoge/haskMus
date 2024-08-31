{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Time.Pattern where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM )
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random (StdGen, random, randomRIO, mkStdGen, randomR)
import Data.List (scanl')
import Data.Maybe (fromMaybe)



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

--pbrown :: Double -> Double -> Double -> Pattern Double
--pbrown lo hi step = Pattern $ \t gen ->
--  let (r, gen') = randomR (-step, step) gen
--      next = max lo (min hi (t + r))
--  in (next, gen')

pbrown :: Double -> Double -> Double -> Pattern Double
pbrown lo hi step = Pattern $ \_ gen ->
  let (r, gen') = randomR (-step, step) gen
      current = (lo + hi) / 2  -- Start in the middle of the range
      next = max lo (min hi (current + r))
  in (next, gen')

pscale :: [Double] -> Pattern Double
pscale scale = Pattern $ \t _ ->
  let i = floor t `mod` length scale
  in (scale !! i, mkStdGen (floor t))  -- Use time as seed for reproducibility

pbind :: Map String (Pattern Double) -> Pattern Event
pbind paramPatterns = Pattern $ \t gen ->
  let (paramValues, gen') = foldr
        (\(k, v) (acc, g) ->
          let (val, g') = runPattern v t g
          in ((k, val):acc, g'))
        ([], gen)
        (Map.toList paramPatterns)
      params = Map.fromList paramValues
      dur = Map.findWithDefault 0 "dur" params
      event = Event t dur params
  in (event, gen')

midiToFreq :: Double -> Double
midiToFreq n = 440 * (2 ** ((n - 69) / 12))


pchoose :: [(Double, Double)] -> Pattern Double
pchoose weightedItems = Pattern $ \_ gen ->
    let totalWeight = sum $ fmap snd weightedItems
        cumWeights = scanl' (+) 0 $ fmap snd weightedItems
        (r, gen') = randomR (0, totalWeight) gen
        chosen = selectItem r weightedItems cumWeights
    in (chosen, gen')
  where
    selectItem :: Double -> [(Double, Double)] -> [Double] -> Double
    selectItem _ [] _ = error "pchoose: empty list of items"
    selectItem r ((x, _):xs) (w:ws)
        | r <= w    = x
        | otherwise = selectItem (r - w) xs ws
    selectItem _ _ _ = error "pchoose: mismatched weights"

-- Helper function to ensure non-zero weights
ensureNonZeroWeights :: [(a, Double)] -> [(a, Double)]
ensureNonZeroWeights items =
    let minWeight = 1e-6  -- A small, non-zero value
    in fmap (\(x, w) -> (x, max w minWeight)) items


scheduleEvents :: [Event] -> IO ()
scheduleEvents events = do
  startTime <- getPOSIXTime
  mapM_ (playEvent (realToFrac startTime)) events
  where
    playEvent startTime event = do
      currentTime <- getPOSIXTime
      let eventTime = startTime + eTime event
          delay = max 0 $ eventTime - realToFrac currentTime
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
--

--main :: IO ()
--main = do
--    let durPattern = pwhite 0.11 1.3
--        freqPattern = fmap midiToFreq (pbrown 60 72 1)
--        ampPattern = pwhite 0.1 0.8
--        -- New weighted choice pattern for scale degrees
--        scalePattern = pchoose [(60, 0.4), (62, 0.2), (64, 0.2), (65, 0.1), (67, 0.1)]
--        myPattern = pbind $ Map.fromList
--            [ ("dur", durPattern)
--            , ("freq", fmap midiToFreq scalePattern)  -- Use weighted choice for frequency
--            , ("amp", ampPattern)
--            , ("pan", pwhite (-1) 1)
--            ]
--    events <- runPatternIO 50 myPattern
--    scheduleEvents events


main :: IO ()
main = do
    let durPattern = pwhite 0.11 1.3
        freqPattern = fmap midiToFreq (pbrown 60 72 1)
        ampPattern = pwhite 0.1 0.8
        -- Ensure non-zero weights
        scalePattern = pchoose $ ensureNonZeroWeights
            [(60, 0.2), (62, 0.2), (64, 0.2), (65, 0.2), (67, 0.2)]
        myPattern = pbind $ Map.fromList
            [ ("dur", durPattern)
            , ("freq", fmap midiToFreq scalePattern)
            , ("amp", ampPattern)
            , ("pan", pwhite (-1) 1)
            ]
    events <- runPatternIO 50 myPattern
    scheduleEvents events
