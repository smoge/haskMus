{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module implements a SuperCollider-inspired pattern in Haskell.

module Time.Pattern  where

import Control.Applicative (liftA2)
import Control.Monad (replicateM)
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import System.Random (randomRIO)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Concurrent (threadDelay)

-- | Time is represented as a Double, measuring seconds.
type Time = Double

-- | Duration is also represented as a Double, measuring seconds.
type Duration = Double

-- | An Event represents a single musical event with a start time, duration, and a map of parameters.
-- This is similar to SuperCollider's Event concept.
data Event = Event
    { eTime :: Time        -- ^ The start time of the event
    , eDuration :: Duration -- ^ The duration of the event
    , eParameters :: Map String Double  -- ^ Parameters like frequency, amplitude, etc.
    } deriving (Show)

-- | A Stream is a time-dependent sequence of values.
-- It's implemented as a function that takes a time and returns a value and the next stream.
newtype Stream a = Stream { runStream :: Time -> IO (a, Stream a) }

-- | The Pattern type represents different kinds of musical patterns.
-- It uses GADTs to provide type safety for different pattern types.
data Pattern a where
    PSeq :: [Pattern a] -> Bool -> Pattern a  -- ^ Sequence of patterns with repeat flag
    PRand :: [Pattern a] -> Pattern a  -- ^ Random choice from a list of patterns
    PWhite :: Double -> Double -> Pattern Double  -- ^ Random values within a range
    PGeom :: Double -> Double -> Int -> Pattern Double  -- ^ Geometric series
    PMap :: (a -> b) -> Pattern a -> Pattern b  -- ^ Function application to a pattern
    PBind :: Map String (Stream Double) -> Pattern Event  -- ^ Combines multiple patterns into an event pattern

-- | The IsPattern class defines how to convert a pattern to a stream.
class IsPattern p where
    asStream :: p a -> Stream a

-- Functor instance for Stream allows mapping functions over streams.
instance Functor Stream where
    fmap f (Stream s) = Stream $ \t -> do
        (x, s') <- s t
        pure (f x, fmap f s')

-- Applicative instance for Stream allows combining streams.
instance Applicative Stream where
    pure x = Stream $ \_ -> pure (x, pure x)
    (Stream sf) <*> (Stream sx) = Stream $ \t -> do
        (f, sf') <- sf t
        (x, sx') <- sx t
        pure (f x, sf' <*> sx')

-- IsPattern instance for Pattern defines how each pattern type is converted to a stream.
instance IsPattern Pattern where
    -- PSeq creates a sequence of patterns, optionally repeating
    asStream (PSeq patterns repeat_) = Stream $ \t ->
        let loop [] = if repeat_ then loop patterns else error "PSeq exhausted"
            loop (p:ps) = do
                (x, s) <- runStream (asStream p) t
                pure (x, Stream $ \t' ->
                    if t' == t
                        then runStream s t'
                        else loop ps)
        in loop patterns

    -- PRand randomly selects one of the given patterns
    asStream (PRand patterns) = Stream $ \t -> do
        i <- randomRIO (0, length patterns - 1)
        runStream (asStream (patterns !! i)) t

    -- PWhite generates random values within a given range
    asStream (PWhite min_ max_) = Stream $ \_ -> do
        x <- randomRIO (min_, max_)
        pure (x, asStream (PWhite min_ max_))

    -- PGeom generates a geometric series
    asStream (PGeom start grow len) = Stream $ \t ->
        let x = start * (grow ^ (floor t `mod` len))
        in pure (x, asStream (PGeom start grow len))

    -- PMap applies a function to a pattern
    asStream (PMap f p) = fmap f (asStream p)

    -- PBind combines multiple parameter streams into an event pattern
    asStream (PBind paramStreams) = Stream $ \t -> do
        (params, paramStreams') <- runParamStreams paramStreams t
        let dur = Map.findWithDefault 0 "dur" params
            event = Event
                { eTime = t
                , eDuration = dur
                , eParameters = params
                }
        pure (event, asStream (PBind paramStreams'))

-- | Runs all parameter streams for a given time, returning the current values and updated streams.
runParamStreams :: Map String (Stream Double) -> Time -> IO (Map String Double, Map String (Stream Double))
runParamStreams streams t = do
    let runStream' k s = do
            (v, s') <- runStream s t
            pure (k, (v, s'))
    results <- mapM (uncurry runStream') (Map.toList streams)
    let params = Map.fromList [(k, v) | (k, (v, _)) <- results]
        streams' = Map.fromList [(k, s') | (k, (_, s')) <- results]
    pure (params, streams')



-- | Creates a sequence pattern.
pseq :: [Pattern a] -> Bool -> Pattern a
pseq = PSeq

-- | Creates a random choice pattern.
prand :: [Pattern a] -> Pattern a
prand = PRand

-- | Creates a white noise pattern between two values.
pwhite :: Double -> Double -> Pattern Double
pwhite = PWhite

-- | Creates a geometric series pattern.
pgeom :: Double -> Double -> Int -> Pattern Double
pgeom = PGeom

-- | Applies a function to a pattern.
pmap :: (a -> b) -> Pattern a -> Pattern b
pmap = PMap

-- | Combines multiple patterns into an event pattern.
pbind :: Map String (Pattern Double) -> Pattern Event
pbind = PBind . Map.map asStream

-- | Creates a constant pattern.
constant :: a -> Pattern a
constant x = pseq [PMap (const x) (PWhite 0 1)] True

-- | Type class for patterns that can be run to produce a list of events.
class RunPattern p where
    runPattern :: Int -> p -> IO [Event]

-- RunPattern instance for Pattern Event
instance RunPattern (Pattern Event) where
    runPattern steps p = go steps (asStream p) 0
      where
        go 0 _ _ = pure []
        go n s t = do
          (event@Event{eDuration = dur}, s') <- runStream s t
          events <- go (n-1) s' (t + dur)
          pure (event : events)

-- RunPattern instance for Pattern Double
instance RunPattern (Pattern Double) where
    runPattern steps p = go steps (asStream p) 0
      where
        go 0 _ _ = pure []
        go n s t = do
          (x, s') <- runStream s t
          events <- go (n-1) s' t
          pure (Event t 0 (Map.singleton "value" x) : events)

-- | Schedules and plays events.
scheduleEvents :: [Event] -> IO ()
scheduleEvents events = do
    startTime <- getPOSIXTime
    mapM_ (playEvent (realToFrac startTime)) events
  where
    playEvent startTime event = do
      currentTime <- getPOSIXTime
      let eventTime = startTime + eTime event
          delay = max 0 $ eventTime - realToFrac currentTime
      threadDelay $ floor $ delay * 1000000  -- Convert to microseconds
      print event  -- In a real implementation, this would trigger sound playback


main :: IO ()
main = do

    let durPattern = pwhite 0.00001 0.5  
        freqPattern = pgeom 100 1.05 12 
        ampPattern = pwhite 0.1 10.8    

        -- Combine the patterns into a single event pattern
        myPattern = pbind $ fromList
            [ ("dur", durPattern)
            , ("freq", freqPattern)
            , ("amp", ampPattern)
            ]

    -- Generate 50 events from the pattern
    events <- runPattern 50 myPattern
    -- Schedule and play the events
    scheduleEvents events