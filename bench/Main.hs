{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Pitch.Accidental
import Pitch.Parser
import Pitch.Pitch
import qualified Data.Text as T

-- Benchmark the pitchToFloat function
benchmarkPitchToFloat :: Benchmark
benchmarkPitchToFloat =
  bgroup
    "pitchToFloat"
    [ bench "C Natural Octave 4" $ whnf pitchToFloat (Pitch C Natural (Octave 4)),
      bench "D QuarterSharp Octave 8" $ whnf pitchToFloat (Pitch D QuarterSharp (Octave 8)),
      bench "E Flat Octave 0" $ whnf pitchToFloat (Pitch E Flat (Octave 0)),
      bench "F Natural Octave 0" $ whnf pitchToFloat (Pitch F Natural (Octave 10))
    ]

benchmarkPcToRational :: Benchmark
benchmarkPcToRational =
  bgroup
    "pcToRational"
    [ bench "C Natural" $ whnf pcToRational (PitchClass C Natural),
      bench "F QuarterFlat" $ whnf pcToRational (PitchClass F QuarterFlat),
      bench "G Sharp" $ whnf pcToRational (PitchClass G Sharp),
      bench "B Flat" $ whnf pcToRational (PitchClass B Flat)
    ]

{-
parsePitches "cqs' cqf,  gqs''" `shouldBe` Right [Pitch C QuarterSharp (Octave 5), Pitch C QuarterFlat (Octave 3), Pitch G QuarterSharp (Octave 6)] -}

benchmarkParsePitches :: Benchmark
benchmarkParsePitches =
  bgroup
    "parsePitches"
    [ bench "c d e f d" $ whnf parsePitches (T.pack "c d e f d"),
      bench "cqs' cqf,  gqs''" $ whnf parsePitches (T.pack "cqs' cqf,  gqs''")
    ]

main :: IO ()
main =
  defaultMain
    [
      {- benchmarkPitchToFloat,
      benchmarkPcToRational, -}
      benchmarkParsePitches
    ]
