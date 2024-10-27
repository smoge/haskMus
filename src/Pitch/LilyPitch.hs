{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Pitch.LilyPitch where

import Data.Map qualified as Map
import Pitch.Pitch (Pitch, generatePitchVars, pitchMap)

$(generatePitchVars (Map.keys pitchMap))

testAllPitches :: [Pitch]
testAllPitches =
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

testAllPitches2 :: [Pitch]
testAllPitches2 =
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

{-

a :: Pitch
a = fromString "a"
a' :: Pitch
a' = fromString "a'"
a'' :: Pitch
a'' = fromString "a''"
a''' :: Pitch
a''' = fromString "a'''"
a'''' :: Pitch
a'''' = fromString "a''''"
a''''' :: Pitch
a''''' = fromString "a'''''"
a_ :: Pitch
a_ = fromString "a_"
a__ :: Pitch
a__ = fromString "a__"
a___ :: Pitch
a___ = fromString "a___"
a____ :: Pitch
a____ = fromString "a____"
a_____ :: Pitch
a_____ = fromString "a_____"
aeh :: Pitch
aeh = fromString "aeh"
aeh' :: Pitch
-}
