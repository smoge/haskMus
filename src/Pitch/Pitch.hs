{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Pitch.Pitch
  ( Pitch (..),
    Octave (..),
    pitchToRational,
    pitchToFloat,
    pitchClass,
    applyRules,
    Rule (..),
    OctaveChange (..),
    enharmonicRules,
    applyRulesWithMap,
    RuleMap,
    buildRuleMap,
    createPitchMap,
    pitchMap,
    generatePitchVars,
    makePitch,
    getPitchClass,
    getOctave,
    setOctave,
    transposeOctave,
    enharmonicPitchEquivs,
    normalizePitch,
    IsString (..),
    applyOctaveChange,
  )
where

import Control.Lens (Lens', lens, makeFields)
import Control.Monad.Extra (concatForM)
import Data.Char (toLower)
import Data.Data (Data)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Language.Haskell.TH
  ( Body (NormalB),
    Dec (SigD, ValD),
    Exp (AppE, LitE, VarE),
    Lit (StringL),
    Pat (VarP),
    Q,
    Type (ConT),
    mkName,
  )
import Language.Haskell.TH.Syntax (Lift)
import Pitch.Accidental
  ( Accidental
      ( DoubleFlat,
        DoubleSharp,
        Flat,
        Natural,
        QuarterFlat,
        QuarterSharp,
        Sharp,
        ThreeQuartersFlat,
        ThreeQuartersSharp
      ),
  )
import Pitch.PitchClass
  ( NoteName (B, C, D),
    PitchClass (..),
    enharmonics,
    pcToRational,
  )

data Pitch = Pitch
  { noteName :: NoteName,
    accidental :: Accidental,
    octave :: Octave
  }
  deriving (Eq, Lift, Data)

newtype Octave = Octave {unOctave :: Int}
  deriving (Eq, Ord, Lift, Data, Enum)

instance Show Octave where
  show :: Octave -> String
  show (Octave o) = "Octave " <> show o

instance Show Pitch where
  show :: Pitch -> String
  show (Pitch name acc oct) = show name <> " " <> show acc <> " " <> show oct

makeFields ''Pitch

pitchClass :: Lens' Pitch PitchClass
pitchClass = lens getter setter
  where
    getter pitch = PitchClass pitch.noteName pitch.accidental
    setter pitch (PitchClass n a) = Pitch n a pitch.octave

pitchToRational :: Pitch -> Rational
pitchToRational (Pitch nm ac oct) = pcToRational (PitchClass nm ac) + fromIntegral (unOctave oct + 1) * 12

pitchToFloat :: Pitch -> Float
pitchToFloat = fromRational . pitchToRational

-- Normalization and preferred accidentals
data Rule = Rule
  { fromPitchClass :: !PitchClass,
    toPitchClass :: !PitchClass,
    octaveChange :: !OctaveChange
  }
  deriving (Show, Eq)

data OctaveChange = NoChange | OctaveUp | OctaveDown
  deriving (Show, Eq)

applyRules :: [Rule] -> Pitch -> Pitch
applyRules rules pitch@(Pitch _ _ oct) =
  case find (`matchesRule` pitch) rules of
    Just (Rule _ (PitchClass toName toAcc) octChange) ->
      Pitch toName toAcc (applyOctaveChange octChange oct)
    Nothing -> pitch

matchesRule :: Rule -> Pitch -> Bool
matchesRule (Rule (PitchClass fromName fromAcc) _ _) (Pitch name acc _) =
  fromName == name && fromAcc == acc

applyOctaveChange :: OctaveChange -> Octave -> Octave
applyOctaveChange NoChange oct = oct
applyOctaveChange OctaveUp (Octave o) = Octave (o + 1)
applyOctaveChange OctaveDown (Octave o) = Octave (o - 1)

enharmonicRules :: [Rule]
enharmonicRules =
  [ Rule (PitchClass C Flat) (PitchClass B Natural) OctaveDown,
    Rule (PitchClass C DoubleFlat) (PitchClass B Flat) OctaveDown,
    Rule (PitchClass C Sharp) (PitchClass C Sharp) NoChange,
    Rule (PitchClass C DoubleSharp) (PitchClass D Natural) NoChange
  ]

type RuleMap = Map.Map PitchClass Rule

applyRulesWithMap :: RuleMap -> Pitch -> Pitch
applyRulesWithMap ruleMap (Pitch nn acc oct) =
  case Map.lookup (PitchClass nn acc) ruleMap of
    Just (Rule _ toPC octChange) ->
      Pitch toPC.noteName toPC.accidental (applyOctaveChange octChange oct)
    Nothing -> Pitch nn acc oct

buildRuleMap :: [Rule] -> RuleMap
buildRuleMap = Map.fromList . fmap (\r -> (fromPitchClass r, r))

-- Pitch map for string representation
createPitchMap :: [NoteName] -> Map.Map String Pitch
createPitchMap = foldr (Map.union . createPitchesForNote) Map.empty

createPitchesForNote :: NoteName -> Map.Map String Pitch
createPitchesForNote note = Map.fromList $ do
  acc <-
    [ Natural,
      Sharp,
      Flat,
      QuarterSharp,
      QuarterFlat,
      ThreeQuartersFlat,
      ThreeQuartersSharp,
      DoubleFlat,
      DoubleSharp
      ]
  let modifier = case acc of
        Sharp -> "is"
        Flat -> "es"
        QuarterSharp -> "ih"
        QuarterFlat -> "eh"
        Natural -> ""
        ThreeQuartersFlat -> "eseh"
        ThreeQuartersSharp -> "isih"
        DoubleFlat -> "eses"
        DoubleSharp -> "isis"
        _ -> error "Invalid accidental"
  (octaveSuffix, oct) <-
    [ ("", 4),
      ("'", 5),
      ("''", 6),
      ("'''", 7),
      ("''''", 8),
      ("'''''", 9),
      ("_", 3),
      ("__", 2),
      ("___", 1),
      ("____", 0),
      ("_____", -1)
      ]
  pure (fmap toLower (show note) <> modifier <> octaveSuffix, Pitch note acc (Octave oct))

pitchMap :: Map.Map String Pitch
pitchMap = createPitchMap [C .. B]

-- Template Haskell for generating pitch variables
-- generatePitchVars :: [String] -> Language.Haskell.TH.Q [Language.Haskell.TH.Dec]
-- generatePitchVars pitchNames =
--  concat <$> forM pitchNames \name -> do
--    let varName = Language.Haskell.TH.mkName name
--        pitchVal = Language.Haskell.TH.AppE (Language.Haskell.TH.VarE 'fromString) (Language.Haskell.TH.LitE (Language.Haskell.TH.StringL name))
--    pure [Language.Haskell.TH.SigD varName (Language.Haskell.TH.ConT ''Pitch), Language.Haskell.TH.ValD (Language.Haskell.TH.VarP varName) (Language.Haskell.TH.NormalB pitchVal) []]
--

-----------------------------------------------------------------------------------

-- Template Haskell for generating pitch variables
generatePitchVars :: [String] -> Q [Dec]
generatePitchVars pitchNames = concatForM pitchNames \name -> do
  let varName = mkName name
      pitchVal = AppE (VarE 'fromString) (LitE (StringL name))
  pure
    [ SigD varName (ConT ''Pitch),
      ValD (VarP varName) (NormalB pitchVal) []
    ]

--------------------------------------------------------------------------------
-- Utility functions

makePitch :: NoteName -> Accidental -> Int -> Pitch
makePitch name acc o = Pitch name acc (Octave o)

getPitchClass :: Pitch -> PitchClass
getPitchClass (Pitch name acc _) = PitchClass name acc

getOctave :: Pitch -> Int
getOctave (Pitch _ _ (Octave o)) = o

setOctave :: Int -> Pitch -> Pitch
setOctave o (Pitch name acc _) = Pitch name acc (Octave o)

transposeOctave :: Int -> Pitch -> Pitch
transposeOctave n (Pitch name acc (Octave o)) = Pitch name acc (Octave (o + n))

enharmonicPitchEquivs :: Pitch -> [Pitch]
enharmonicPitchEquivs pitch =
  [ Pitch name acc (Octave $ getOctave pitch)
    | PitchClass name acc <- enharmonics (getPitchClass pitch)
  ]

normalizePitch :: Pitch -> Pitch
normalizePitch = applyRules enharmonicRules

instance IsString Pitch where
  fromString :: String -> Pitch
  fromString str = fromMaybe (error $ "Invalid pitch: " <> str) (Map.lookup str pitchMap)
