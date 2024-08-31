{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Time.Pattern where

import Control.Concurrent (threadDelay)
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random (randomRIO)

type Time = Double
type Duration = Double

data Event = Event
  { eTime :: Time,
    eDuration :: Duration,
    eParameters :: Map String Double
  }
  deriving (Show)

newtype Stream a = Stream {runStream :: Time -> IO (a, Stream a)}

data Pattern a where
  PSeq :: [Pattern a] -> Bool -> Pattern a
  PRand :: [Stream a] -> Pattern a
  PWhite :: Double -> Double -> Pattern Double
  PGeom :: Double -> Double -> Int -> Int -> Pattern Double
  PMap :: (a -> b) -> Pattern a -> Pattern b
  PBind :: Map String (Stream Double) -> Pattern Event
  PCycle :: [Pattern a] -> Pattern a
  PXRand :: [Stream a] -> Pattern a
  PBrown :: Double -> Double -> Double -> Pattern Double
  PScale :: [Double] -> Pattern Double
  PDegrade :: Double -> Pattern a -> Pattern a

class IsPattern p where
  asStream :: p a -> Stream a

instance Functor Stream where
  fmap f (Stream s) = Stream $ \t -> do
    (x, s') <- s t
    pure (f x, fmap f s')

instance Applicative Stream where
  pure x = Stream $ \_ -> pure (x, pure x)
  (Stream sf) <*> (Stream sx) = Stream $ \t -> do
    (f, sf') <- sf t
    (x, sx') <- sx t
    pure (f x, sf' <*> sx')

instance IsPattern Pattern where
  asStream (PSeq patterns repeat_) = Stream $ \t ->
    let loop [] = if repeat_ then loop patterns else error "PSeq exhausted"
        loop (p : ps) = do
          (x, s) <- runStream (asStream p) t
          pure (x, Stream $ \t' -> if t' == t then runStream s t' else loop ps)
     in loop patterns

  asStream (PRand streams) = Stream $ \t -> do
    i <- randomRIO (0, length streams - 1)
    (x, s') <- runStream (streams !! i) t
    let newStreams = take i streams ++ [s'] ++ drop (i + 1) streams
    pure (x, asStream (PRand newStreams))

  asStream (PWhite min_ max_) = Stream $ \_ -> do
    x <- randomRIO (min_, max_)
    pure (x, asStream (PWhite min_ max_))

  asStream (PGeom start grow len counter) = Stream $ \_ ->
    let x = start * (grow ^ (counter `mod` len))
     in pure (x, asStream (PGeom start grow len (counter + 1)))

  asStream (PMap f p) = fmap f (asStream p)

  asStream (PBind paramStreams) = Stream $ \t -> do
    (params, paramStreams') <- runParamStreams paramStreams t
    let dur = Map.findWithDefault 0 "dur" params
        event = Event { eTime = t, eDuration = dur, eParameters = params }
    pure (event, asStream (PBind paramStreams'))

  asStream (PCycle patterns) = Stream $ \t ->
    let loop [] = loop patterns
        loop (p:ps) = do
          (x, s) <- runStream (asStream p) t
          pure (x, Stream $ \t' -> if t' == t then runStream s t' else loop ps)
     in loop patterns

  asStream (PXRand streams) = Stream $ \t -> do
    i <- randomRIO (0, length streams - 1)
    (x, s') <- runStream (streams !! i) t
    let newStreams = take i streams ++ drop (i + 1) streams ++ [s']
    pure (x, asStream (PXRand newStreams))

  asStream (PBrown min_ max_ step) = Stream $ \_ -> do
    x <- randomRIO (min_, max_)
    let nextMin = max min_ (x - step)
        nextMax = min max_ (x + step)
    pure (x, asStream (PBrown nextMin nextMax step))

  asStream (PScale scale) = Stream $ \t ->
    let index = floor t `mod` length scale
     in pure (scale !! index, asStream (PScale (drop (index + 1) scale ++ take (index + 1) scale)))

  asStream (PDegrade prob p) = Stream $ \t -> do
    r <- randomRIO (0, 1)
    if r < prob
      then runStream (asStream p) t
      else pure (error "Degraded", asStream (PDegrade prob p))

runParamStreams :: Map String (Stream Double) -> Time -> IO (Map String Double, Map String (Stream Double))
runParamStreams streams t = do
  results <- mapM (\(k, s) -> do (v, s') <- runStream s t; pure ((k, v), (k, s'))) (Map.toList streams)
  let (params, streams') = unzip results
  pure (Map.fromList params, Map.fromList streams')

pseq :: [Pattern a] -> Bool -> Pattern a
pseq = PSeq

prand :: [Pattern a] -> Pattern a
prand patterns = PRand (map asStream patterns)

pwhite :: Double -> Double -> Pattern Double
pwhite = PWhite

pgeom :: Double -> Double -> Int -> Pattern Double
pgeom start grow len = PGeom start grow len 0

pmap :: (a -> b) -> Pattern a -> Pattern b
pmap = PMap

pbind :: Map String (Pattern Double) -> Pattern Event
pbind = PBind . Map.map asStream

pcycle :: [Pattern a] -> Pattern a
pcycle = PCycle

pxrand :: [Pattern a] -> Pattern a
pxrand patterns = PXRand (map asStream patterns)

pbrown :: Double -> Double -> Double -> Pattern Double
pbrown = PBrown

pscale :: [Double] -> Pattern Double
pscale = PScale

pdegrade :: Double -> Pattern a -> Pattern a
pdegrade = PDegrade

constant :: a -> Pattern a
constant x = pseq [PMap (const x) (PWhite 0 1)] True

class RunPattern p where
  runPattern :: Int -> p -> IO [Event]

instance RunPattern (Pattern Event) where
  runPattern steps p = go steps (asStream p) 0
    where
      go 0 _ _ = pure []
      go n s t = do
        (event@Event {eDuration = dur}, s') <- runStream s t
        events <- go (n - 1) s' (t + dur)
        pure (event : events)

instance RunPattern (Pattern Double) where
  runPattern steps p = go steps (asStream p) 0
    where
      go 0 _ _ = pure []
      go n s t = do
        (x, s') <- runStream s t
        events <- go (n - 1) s' t
        pure (Event t 0 (Map.singleton "value" x) : events)

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

midiToFreq :: Double -> Double
midiToFreq n = 440 * (2 ** ((n - 69) / 12))

main :: IO ()
main = do
  let durPattern = pwhite 0.1 1.0
      freqPattern = prand [pgeom 100 1.1 15, pgeom 1000 1.1 15, pgeom 5000 1.1 15]
      ampPattern = pwhite 0.1 0.8
      scalePattern = pscale [60, 62, 64, 65, 67, 69, 71, 72]  -- C major scale
      degradedFreqPattern = pdegrade 0.2 freqPattern  -- 20% chance of skipping a frequency
      brownianPattern = pbrown 60 72 1  -- Brownian motion between MIDI notes 60 and 72

      myPattern = pbind $ fromList
        [ ("dur", durPattern)
        , ("freq", pmap midiToFreq brownianPattern)  -- Convert MIDI notes to Hz
        , ("amp", ampPattern)
        , ("pan", pwhite (-1) 1)  -- Add panning
        ]

  events <- runPattern 50 myPattern
  scheduleEvents events