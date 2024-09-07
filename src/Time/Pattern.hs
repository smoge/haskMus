{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Time.Pattern where


import Data.Map (Map)
import qualified Data.Map as Map
--import System.Random (StdGen, mkStdGen, randomR)
--import Data.List (scanl')
--import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Control.DeepSeq (deepseq)
import System.Clock (Clock(Monotonic), getTime, toNanoSecs)
import Control.Monad.State
import System.Random
import Sound.Sc3
import Sound.Sc3.Server.Command
import Sound.Osc.Fd
--import qualified Sound.Sc3 as Sc3
--import Sound.Sc3.Server.Command
--import qualified Sound.Osc.Fd as Osc
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad 

type TimeP = Double
type Duration = Double

data Event = Event
  { eTime :: !TimeP
  , eDuration :: !Duration
  , eParameters :: !(Map String Double)
  } deriving (Show)

newtype Pattern a = Pattern { runPattern :: TimeP -> StdGen -> (a, StdGen) }

instance Functor Pattern where
  fmap :: (a -> b) -> Pattern a -> Pattern b
  fmap f (Pattern p) = Pattern $ \t gen ->
    let (x, gen') = p t gen
    in (f x, gen')

instance Applicative Pattern where
  pure x = Pattern $ \_ gen -> (x, gen)
  Pattern pf <*> Pattern px = Pattern $ \t gen ->
    let (f, gen')  = pf t gen
        (x, gen'') = px t gen'
    in (f x, gen'')

-- Using Applicative 
sumPattern :: Pattern Double -> Pattern Double -> Pattern Double
sumPattern = liftA2 (+)

productPattern :: Pattern Double -> Pattern Double -> Pattern Double
productPattern = liftA2 (*)

instance Monad Pattern where
  return = pure
  Pattern px >>= f = Pattern $ \t gen ->
    let (x, gen') = px t gen
        Pattern py = f x
    in py t gen'

-- Using Monad 
dependentPattern :: Pattern Double -> Pattern Double
dependentPattern p = do
  x <- p
  if x > 0.5
    then pure (x * 2)
    else pure (x / 2)


getRandom :: (StdGen -> (a, StdGen)) -> Pattern a
getRandom f = Pattern $ \_ gen -> f gen

pwhite :: Double -> Double -> Pattern Double
pwhite min_ max_ = getRandom (randomR (min_, max_))



type BrownianState = (Double, StdGen)


brownianStep :: Double -> Double -> Double -> State BrownianState Double
brownianStep lo hi sqrtDeltaT = do
  (pos, g) <- get
  let (r, g') = randomR (-1.0, 1.0) g
      dW = r * sqrtDeltaT
      newPos = max lo (min hi (pos + dW))
  put (newPos, g')
  pure newPos



pbrown :: Double -> Double -> Double -> Pattern Double
pbrown lo hi volatility = Pattern $ \_ gen ->
  let initialPos = (lo + hi) / 2
      initialState = (initialPos, gen)
      (result, (_, newGen)) = runState (brownianStep lo hi volatility) initialState
  in (result, newGen)

pscale :: [Double] -> Pattern Double
pscale scale = Pattern $ \t _ ->
  let i = floor t `mod` length scale
      scale' = (case drop i scale of
         x : _ -> x
         [] -> error "pscale: empty scale")
  in (scale', mkStdGen (floor t))

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

normalizeSum' :: [(a, Double)] -> [(a, Double)]
normalizeSum' [] = error "normalizeSum: empty list of items"
normalizeSum' xs = fmap (\(x, w) -> (x, abs w / totalSum)) xs
  where
    totalSum = sum $ fmap (abs . snd) xs

pchoose :: [(Double, Double)] -> Pattern Double
pchoose weightedItemsOrig
    | null weightedItemsOrig = error "pchoose: empty list of items"
    | otherwise =
        let weightedItems = weightedItemsOrig
        in weightedItems `deepseq` Pattern $ \_ gen ->
            let totalWeight = sum $ fmap snd weightedItems
                cumWeights = scanl1 (+) $ fmap snd weightedItems
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
    let minWeight = 1e-6
    in fmap (\(x, w) -> (x, max w minWeight)) items

scheduleEvents :: [Event] -> IO ()
scheduleEvents events = do
  startTime <- getTime Monotonic
  mapM_ (playEvent startTime) events
  where
    playEvent startTime event = do
      currentTime <- getTime Monotonic
      let eventTime = fromIntegral (toNanoSecs startTime) / 1e9 + eTime event
          delay = max 0 (eventTime - fromIntegral (toNanoSecs currentTime) / 1e9)
      threadDelay $ floor (delay * 1000000)
      print event


runPatternIO :: Int -> Int -> Pattern Event -> IO [Event]
runPatternIO seed n p = do
  let initialGen = mkStdGen seed
      go _ 0 _ acc = pure (reverse acc)
      go t i gen acc = do
        let (event, gen') = runPattern p t gen
            nextT = t + eDuration event
        go nextT (i - 1) gen' (event : acc)
  go 0 n initialGen []

uniformToExponential :: Double -> Double -> Double
uniformToExponential λ u
  | u > 0 && u < 1 = - (log (1 - u) / λ)
  | otherwise = error "Uniform random variable must be in the range (0, 1)."

pexponential :: Double -> Pattern Double
pexponential lambda = getRandom $ \gen ->
  let (u, gen') = randomR (0.0, 1.0) gen
  in (uniformToExponential lambda u, gen')

main :: IO ()
main = do
    let durPattern = fmap (* 0.5) (pexponential 9)
        freqPattern = fmap (* 100) (pbrown 0 1.0 0.2)  -- fmap midiToFreq (pbrown 60 72 1)
        ampPattern = pwhite 0.1 0.8
        rawScalePattern = normalizeSum' [(60, 0.2), (62, 0.2), (64, 0.5), (65, 0.2), (67, 0.2)]
        scalePattern = pchoose  rawScalePattern

    let myPattern = pbind $ Map.fromList
            [ ("dur", durPattern)
            , ("freq", fmap midiToFreq scalePattern)
            , ("BROWN", freqPattern)
            , ("amp", ampPattern)
            , ("pan", pwhite (-1) 1)
            ]

    events <- runPatternIO 42 150 myPattern
    scheduleEvents events
    --trace ("rawScalePattern: " <> show rawScalePattern) $ pure ()


simpleSynth :: Ugen
simpleSynth = out 0 $ pan2 (sinOsc ar freq 0 * amp) pan 1
  where
    freq = control kr "freq" 440
    amp = control kr "amp" 0.5
    pan = control kr "pan" 0
--
--createSynthForEvent :: Event -> IO ()
--createSynthForEvent event = withSc3 $ do
--    let freq = Map.findWithDefault 440 "freq" (eParameters event)
--        amp = Map.findWithDefault 0.5 "amp" (eParameters event)
--        pan = Map.findWithDefault 0 "pan" (eParameters event)
--        dur = eDuration event
--
--    -- Send synth definition to the server (if not already sent)
--    async $ d_recv (synthdef "simple" simpleSynth)
--
--    -- Start the synth
--    nodeId <- async $ s_new "simple" (-1) AddToTail 1
--                        [("freq", freq), ("amp", amp), ("pan", pan)]
--
--    -- Schedule the synth to stop after its duration
--    liftIO $ forkIO $ do
--        threadDelay (floor (dur * 1000000))  -- Convert to microseconds
--        withSc3 $ n_free [nodeId]

-- New GrainEvent type for granular synthesis
data GrainEvent = GrainEvent
  { geTime :: !TimeP
  , geDuration :: !Duration
  , geFreq :: !Double
  , geAmp :: !Double
  , gePan :: !Double
  } deriving (Show)

-- Generate a cloud of grain events
generateGrainCloud :: Int -> TimeP -> Duration -> IO [GrainEvent]
generateGrainCloud grainCount startTime totalDuration = replicateM grainCount $ do
  time <- randomRIO (startTime, startTime + totalDuration)
  duration <- randomRIO (0.01, 0.1)  -- Typical grain durations
  freq <- randomRIO (200, 2000)  -- Frequency range
  amp <- randomRIO (0.01, 0.1)  -- Amplitude range
  pan <- randomRIO (-1, 1)  -- Panning
  pure $ GrainEvent time duration freq amp pan

