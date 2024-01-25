{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Pitch.LilyPitch where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import Pitch.Pitch

instance IsString Pitch where
  fromString :: String -> Pitch
  fromString str = fromMaybe (error $ "Invalid pitch: " <> str) (Map.lookup str pitchMap)

$(generatePitchVars (Map.keys pitchMap))

allPitches :: [Pitch]
allPitches =
  [ "c",
    "cis",
    "ces",
    "cisis",
    "ceses",
    "d",
    "dis",
    "des",
    "dih___",
    "deh'''",
    "d__",
    "e",
    "ees",
    "eis",
    "eeh",
    "feseh",
    "eih",
    "f"
  ]

allPitches2 :: [Pitch]
allPitches2 =
  [ c,
    cis,
    ces,
    cisis,
    ceses,
    d,
    dis,
    des,
    dih___,
    deh''',
    d__,
    e,
    ees,
    eis,
    eeh,
    feseh,
    eih,
    f
  ]