{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveLift             #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedRecordDot    #-}
-- Needed for lens operations
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}


module Pitch.Pitch where

import           Control.Lens               hiding (elements)
import           Control.Monad              (forM)
import           Data.Char                  (toLower)
import           Data.Data                  (Data)
import           Data.Fixed                 (mod')
import           Data.List                  (find)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe)
import           Data.Ratio
import           Data.String                (IsString (..))
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Pitch.Accidental


data NoteName = C | D | E | F | G | A | B
  deriving (Eq, Ord, Show, Enum, Bounded, Lift, Data)

data IntervalBasis = Chromatic | Diatonic
  deriving (Eq, Ord, Show, Enum)

data PitchClass = PitchClass
  { noteName   :: NoteName,
    accidental :: Accidental
  }
  deriving (Eq, Lift, Data, Ord)

data Pitch = Pitch
  { noteName   :: NoteName,
    accidental :: Accidental,
    octave     :: Octave
  }
  deriving (Eq, Lift, Data)

newtype Octave = Octave {unOctave :: Int}
  deriving (Eq, Ord, Lift, Data, Enum)


-- deriving instance Data Pitch

mkPitch :: NoteName -> Accidental -> Octave -> Pitch
mkPitch = Pitch

{-# INLINE mkPitch' #-}
mkPitch' :: PitchClass -> Octave -> Pitch
mkPitch' pc o = Pitch {noteName = pc.noteName, accidental = pc.accidental, octave = o}

data SomeNote = forall notename. (IsNoteName notename) => SomeNote notename


class NoteClass (noteName :: NoteName) where
  sayNote :: String

class IsNoteName a where
  toNoteName :: a -> NoteName

instance IsNoteName SomeNote where
  -- \| Converts a SomeNote to a NoteName.
  toNoteName :: SomeNote -> NoteName
  toNoteName (SomeNote nn) = toNoteName nn

instance Show SomeNote where
  show = show . toNoteName

instance IsString NoteName where
  fromString :: String -> NoteName
  fromString "c" = C
  fromString "d" = D
  fromString "e" = E
  fromString "f" = F
  fromString "g" = G
  fromString "a" = A
  fromString "b" = B
  fromString s   = error $ "Invalid NoteName string: " <> s

instance Show PitchClass where
  show :: PitchClass -> String
  show (PitchClass name acc) = show name <> " " <> show acc

instance Show Octave where
  show :: Octave -> String
  show (Octave o) = "Octave " <> show o

instance Show Pitch where
  show :: Pitch -> String
  show (Pitch name acc oct) = show name <> " " <> show acc <> " " <> show oct


makeFields ''PitchClass
makeFields ''Pitch


pitchClass :: Lens' Pitch PitchClass
pitchClass = lens getter setter
  where
    getter :: Pitch -> PitchClass
    getter pitch = PitchClass pitch.noteName pitch.accidental

    setter :: Pitch -> PitchClass -> Pitch
    setter pitch (PitchClass n a) = Pitch n a pitch.octave



{-# INLINE pcToRational #-}
pcToRational :: PitchClass -> Rational
pcToRational pitchclass = base + acVal
  where
    base = case Prelude.lookup nm noteNameToRational' of
      Just val -> val
      Nothing  -> error "NoteName not found"
    acVal = accidentalToSemitones ac :: Rational
    nm = pitchclass.noteName
    ac = pitchclass.accidental

(=~) :: PitchClass -> PitchClass -> Bool
pc1 =~ pc2 = (pcToRational pc1 `mod'` 12) == (pcToRational pc2 `mod'` 12)

noteNameToRationalMap :: Map.Map NoteName Rational
noteNameToRationalMap = Map.fromList [(C, 0), (D, 2), (E, 4), (F, 5), (G, 7), (A, 9), (B, 11)]

noteNameToRational :: NoteName -> Rational
noteNameToRational name = fromMaybe (error ("NoteName " <> show name <> " not found")) (Map.lookup name noteNameToRationalMap)

noteNameToRational' :: [(NoteName, Rational)]
noteNameToRational' = [(C, 0), (D, 2), (E, 4), (F, 5), (G, 7), (A, 9), (B, 11)]

pitchToRational :: Pitch -> Rational
pitchToRational (Pitch nm ac oct) = pcToRational (PitchClass nm ac) + fromIntegral (unOctave oct + 1) * 12

rationalToFloat :: Rational -> Float
rationalToFloat = fromRational

pitchToFloat :: Pitch -> Float
pitchToFloat (Pitch nm ac oct) = fromRational $ pcToRational (PitchClass nm ac) + fromIntegral (unOctave oct + 1) * 12

allPitchClasses :: [PitchClass]
allPitchClasses = liftA2 PitchClass [C, D, E, F, G, A, B] allAccidentals

allPCRationals :: [Rational]
allPCRationals = fmap pcToRational allPitchClasses

enharmonicPCEquivs :: Rational -> [(Rational, PitchClass)]
enharmonicPCEquivs val =
  [(v, pitchclass) | pitchclass <- allPitchClasses, let v = pcToRational pitchclass, v `mod'` 12 == val `mod'` 12]

enharmonicPCEquivs' :: PitchClass -> [(Rational, PitchClass)]
enharmonicPCEquivs' pitchclass =
  [(v, pc') | pc' <- allPitchClasses, let v = pcToRational pc', v `mod'` 12 == pcToRational pitchclass `mod'` 12]



type EnharmonicMapping = [(Rational, [PitchClass])]

-- type EnharmonicMapping = Map.Map Rational [PitchClass]

enharmonicMapping :: [Rational] -> EnharmonicMapping
enharmonicMapping = fmap (\r -> (r, fmap snd (enharmonicPCEquivs r)))

enharmonics :: PitchClass -> [PitchClass]
enharmonics pitchclass = fromMaybe [pitchclass] (lookup (pcToRational pitchclass) enharmonicMap)
  where
    enharmonicMap = enharmonicMapping allPCRationals

allEnharmonics :: [[PitchClass]]
allEnharmonics = fmap enharmonics allPitchClasses

allEnharmonicsMapping :: [(PitchClass, [PitchClass])]
allEnharmonicsMapping = zip allPitchClasses allEnharmonics

c4 :: Pitch
c4 = Pitch C Natural (Octave 4)

preferedAccidentalPC :: PitchClass -> PitchClass
preferedAccidentalPC pitchclass
  | pitchclass == PitchClass C Flat        = PitchClass B Natural
  | pitchclass == PitchClass D Sharp       = PitchClass E Flat
  | pitchclass == PitchClass E Sharp       = PitchClass F Natural
  | pitchclass == PitchClass F Flat        = PitchClass E Natural
  | pitchclass == PitchClass A Sharp       = PitchClass B Flat
  | pitchclass == PitchClass B Sharp       = PitchClass C Natural
  | pitchclass == PitchClass B DoubleSharp = PitchClass C Sharp
  | otherwise                              = pitchclass

preferredAccidentalP :: Pitch -> Pitch
preferredAccidentalP pitch@(Pitch _ _ oct@(Octave o))
  | pitch == Pitch C Flat oct   = Pitch B Natural (Octave ( o - 1))
  | pitch == Pitch D Flat oct   = Pitch C Sharp oct
  | pitch == Pitch D Sharp oct  = Pitch E Flat oct
  | pitch == Pitch F Flat oct   = Pitch E Natural oct
  | pitch == Pitch G Flat oct   = Pitch F Sharp oct
  | pitch == Pitch A Sharp oct  = Pitch B Flat oct
  | pitch == Pitch B Sharp oct  = Pitch C Natural (Octave ( o + 1))
  | otherwise                   = pitch

piPitch :: Pitch -> Pitch
piPitch pitch@(Pitch _ _ oct@(Octave o))
  | pitch == Pitch C Flat oct   = Pitch B Natural (Octave ( o - 1))
  | pitch == Pitch D Flat oct   = Pitch C Sharp oct
  | pitch == Pitch D Sharp oct  = Pitch E Flat oct
  | pitch == Pitch F Flat oct   = Pitch E Natural oct
  | pitch == Pitch G Flat oct   = Pitch F Sharp oct
  | pitch == Pitch A Sharp oct  = Pitch B Flat oct
  | pitch == Pitch B Sharp oct  = Pitch C Natural (Octave ( o + 1))
  | otherwise                      = pitch


preferredAccidentalList :: [Pitch] -> [Pitch]
preferredAccidentalList = fmap preferredAccidentalP



normalizeEnharmonicPC :: PitchClass -> PitchClass
normalizeEnharmonicPC pc_ = case pc_ of
  PitchClass C Flat  -> PitchClass B Natural
  PitchClass C Sharp -> pc_
  PitchClass D Flat  -> PitchClass C Sharp
  PitchClass D Sharp -> PitchClass E Flat
  PitchClass E Sharp -> PitchClass F Natural
  PitchClass F Flat  -> PitchClass E Natural
  PitchClass F Sharp -> pc_
  PitchClass G Flat  -> PitchClass F Sharp
  PitchClass G Sharp -> pc_
  PitchClass A Flat  -> pc_
  PitchClass A Sharp -> PitchClass B Flat
  PitchClass B Sharp -> PitchClass C Natural
  _                  -> pc_

normalizeEnharmonicPitch :: Pitch -> Pitch
normalizeEnharmonicPitch pitch = case pitch of
  Pitch C Flat oct               -> Pitch B Natural (octaveDown oct)
  Pitch C DoubleFlat oct         -> Pitch B Flat (octaveDown oct)
  Pitch D Flat oct               -> Pitch C Sharp oct
  Pitch D Sharp oct              -> Pitch E Flat oct
  Pitch E Sharp oct              -> Pitch F Natural oct
  Pitch F Flat oct               -> Pitch E Natural oct
  Pitch G Flat oct               -> Pitch F Sharp oct
  Pitch A Sharp oct              -> Pitch B Flat oct
  Pitch B Sharp oct              -> Pitch C Natural (octaveUp oct)
  Pitch B ThreeQuartersSharp oct -> Pitch C QuarterSharp (octaveUp oct)
  _                              -> pitch
  where
    octaveDown (Octave o) = Octave (o - 1)
    octaveUp (Octave o) = Octave (o + 1)

normalizeEnharmonicPCs :: [PitchClass] -> [PitchClass]
normalizeEnharmonicPCs = fmap normalizeEnharmonicPC

normalizeEnharmonicPitches :: [Pitch] -> [Pitch]
normalizeEnharmonicPitches = fmap normalizeEnharmonicPitch

data Rule = Rule
  { fromPitchClass :: !PitchClass
  , toPitchClass   :: !PitchClass
  , octaveChange   :: !OctaveChange
  } deriving (Show, Eq)



data OctaveChange = NoChange | OctaveUp | OctaveDown
  deriving (Show, Eq)


applyRules :: [Rule] -> Pitch -> Pitch
applyRules rules pitch@(Pitch _ _ oct) =
  case find (`matchesRule` pitch) rules of
    Just (Rule _ (PitchClass toName toAcc) octChange) ->
      Pitch toName toAcc (applyOctaveChange octChange oct)
    Nothing -> pitch

-- {-# INLINE matchesRule #-}
matchesRule :: Rule -> Pitch -> Bool
matchesRule (Rule (PitchClass fromName fromAcc) _ _) (Pitch name acc _) =
  fromName == name && fromAcc == acc

-- {-# INLINE applyRulesWithMap #-}
applyOctaveChange :: OctaveChange -> Octave -> Octave
applyOctaveChange NoChange oct          = oct
applyOctaveChange OctaveUp (Octave o)   = Octave (o + 1)
applyOctaveChange OctaveDown (Octave o) = Octave (o - 1)


enharmonicRules :: [Rule]
enharmonicRules =
  [ Rule (PitchClass C Flat)           (PitchClass B Natural)        OctaveDown
  , Rule (PitchClass C DoubleFlat)     (PitchClass B Flat)           OctaveDown
  , Rule (PitchClass C Sharp)          (PitchClass C Sharp)          NoChange
  , Rule (PitchClass C DoubleSharp)    (PitchClass D Natural)        NoChange
  , Rule (PitchClass D Flat)           (PitchClass C Sharp)          NoChange
  , Rule (PitchClass D Sharp)          (PitchClass E Flat)           NoChange
  , Rule (PitchClass E Sharp)          (PitchClass F Natural)        NoChange
  , Rule (PitchClass E DoubleSharp)    (PitchClass F Sharp)          NoChange
  , Rule (PitchClass F Flat)           (PitchClass E Natural)        NoChange
  , Rule (PitchClass F Sharp)          (PitchClass F Sharp)          NoChange
  , Rule (PitchClass G Flat)           (PitchClass F Sharp)          NoChange
  , Rule (PitchClass G Sharp)          (PitchClass G Sharp)          NoChange
  , Rule (PitchClass A Flat)           (PitchClass G Sharp)          NoChange
  , Rule (PitchClass A Sharp)          (PitchClass B Flat)           NoChange
  , Rule (PitchClass B Sharp)          (PitchClass C Natural)        OctaveUp
  , Rule (PitchClass B DoubleSharp)    (PitchClass C Sharp)          OctaveUp

  , Rule (PitchClass C QuarterSharp)   (PitchClass C QuarterSharp)   NoChange
  , Rule (PitchClass C ThreeQuartersSharp) (PitchClass D QuarterFlat) NoChange
  , Rule (PitchClass D QuarterFlat)    (PitchClass D QuarterFlat)    NoChange
  , Rule (PitchClass D ThreeQuartersFlat)  (PitchClass C QuarterSharp) NoChange
  , Rule (PitchClass E QuarterSharp)   (PitchClass F QuarterFlat)    NoChange
  , Rule (PitchClass F QuarterSharp)   (PitchClass F QuarterSharp)   NoChange
  , Rule (PitchClass G QuarterSharp)   (PitchClass G QuarterSharp)   NoChange
  , Rule (PitchClass A QuarterSharp)   (PitchClass B ThreeQuartersFlat) NoChange
  , Rule (PitchClass B QuarterSharp)   (PitchClass C QuarterFlat)    OctaveUp
  ]


exampleNormalization :: IO ()
exampleNormalization = do
  let pitches = [ Pitch C Flat (Octave 4)
                , Pitch D Sharp (Octave 4)
                , Pitch F Natural (Octave 4)
                , Pitch B Sharp (Octave 3)
                , Pitch C ThreeQuartersSharp (Octave 4)
                , Pitch E QuarterSharp (Octave 5)
                , Pitch G DoubleSharp (Octave 3)
                ] :: [Pitch]
  mapM_ (\p -> do
    putStrLn $ "Original: " <> show p
    putStrLn $ "Normalized: " <> show (applyRules enharmonicRules p)
    putStrLn "") pitches


pc :: NoteName -> Accidental -> PitchClass
pc = PitchClass

(-:>) :: PitchClass -> PitchClass -> Rule
(-:>) from_ to_ = Rule from_ to_ OctaveDown

(+:>) :: PitchClass -> PitchClass -> Rule
(+:>) from_ to_ = Rule from_ to_ OctaveUp

(=:>) :: PitchClass -> PitchClass -> Rule
(=:>) from_ to_ = Rule from_ to_ NoChange


keep :: PitchClass -> Rule
keep p = Rule p p NoChange

enharmonicRules2 :: [Rule]
enharmonicRules2 =
  [ pc C Flat           -:> pc B Natural
  , pc C DoubleFlat     -:> pc B Flat
  , pc C DoubleSharp    =:> pc D Natural
  , pc D Flat           =:> pc C Sharp
  , pc D Sharp          =:> pc E Flat
  , pc E Sharp          =:> pc F Natural
  , pc E DoubleSharp    =:> pc F Sharp
  , pc F Flat           =:> pc E Natural
  , pc G Flat           =:> pc F Sharp
  , pc A Flat           =:> pc G Sharp
  , pc A Sharp          =:> pc B Flat
  , pc B Sharp          +:> pc C Natural
  , pc B DoubleSharp    +:> pc C Sharp

  , pc C ThreeQuartersSharp =:> pc D QuarterFlat
  , pc D ThreeQuartersFlat  =:> pc C QuarterSharp
  , pc E QuarterSharp       =:> pc F QuarterFlat
  ]

prettyPrintRules :: [Rule] -> IO ()
prettyPrintRules = mapM_ (\(Rule from_ to_ oct) ->
  putStrLn (show from_ <> (" -> " <> show to_ <>
             case oct of
               NoChange   -> ""
               OctaveUp   -> " (Octave Up)"
               OctaveDown -> " (Octave Down)")))


-- >>> prettyPrintRules enharmonicRules



applyRuleMap :: Functor f => RuleMap -> f Pitch -> f Pitch
applyRuleMap ruleMap = fmap (applyRulesWithMap ruleMap)

{-# INLINE applyRuleMapToPitches #-}
applyRuleMapToPitch :: RuleMap -> Pitch -> Pitch
applyRuleMapToPitch   = applyRulesWithMap


applyRuleMapToPitches :: RuleMap -> [Pitch] -> [Pitch]
applyRuleMapToPitches   = applyRuleMap


applyRulesToPitch :: [Rule] -> Pitch -> Pitch
applyRulesToPitch rules pitch@(Pitch name acc oct) =
  case findMatchingRule (PitchClass name acc) rules of
    Just (Rule _ (PitchClass toName toAcc) octChange) ->
      Pitch toName toAcc (applyOctaveChange octChange oct)
    Nothing -> pitch

findMatchingRule :: PitchClass -> [Rule] -> Maybe Rule
findMatchingRule pc_ = find (\(Rule fromPC _ _) -> fromPC == pc_)


-- ! test
-- >applyRulesToPitches enharmonicRules2 ( mkPitchesFromIntervals c4 minorScaleInterval)
-- [C Natural Octave 4,D Natural Octave 4,E Flat Octave 4,F Natural Octave 4,G Natural Octave 4,G Sharp Octave 4,B Flat Octave 4]


-- demonstrateEnharmonicNormalization

-- > preferredAccidentalList $  mkPitchesFromIntervals c4 minorScaleInterval
--[C Natural Octave 4,D Natural Octave 4,E Flat Octave 4,F Natural Octave 4,G Natural Octave 4,G Sharp Octave 4,B Flat Octave 4]



type RuleMap = Map.Map PitchClass Rule


{-# INLINE applyRulesWithMap #-}
applyRulesWithMap :: RuleMap -> Pitch -> Pitch
applyRulesWithMap ruleMap (Pitch nn acc oct) =
    case Map.lookup (PitchClass nn acc) ruleMap of
        Just (Rule _ toPC octChange) ->
            Pitch toPC.noteName toPC.accidental (applyOctaveChange octChange oct)
        Nothing -> Pitch nn acc oct


--{-# INLINE buildRuleMap #-}
buildRuleMap :: [Rule] -> RuleMap
buildRuleMap = Map.fromList . fmap (\r -> (fromPitchClass r, r))

--{-# INLINE chromaticPosition #-}
chromaticPosition :: PitchClass -> Rational
chromaticPosition pc = pcToRational pc `mod'` 12


--{-# INLINE inferOctaveChange #-}
inferOctaveChange :: PitchClass -> PitchClass -> OctaveChange
inferOctaveChange (PitchClass fromName _) (PitchClass toName _)
  | fromName == C && toName == B = OctaveDown
  | fromName == D && toName == B = OctaveDown
  | fromName == B && toName == C = OctaveUp
  | fromName == A && toName == C = OctaveUp
  | otherwise = NoChange




applyRule :: Pitch -> Rule -> Pitch
applyRule (Pitch _ _ oct) (Rule _ (PitchClass toName toAcc) octChange) =
  Pitch toName toAcc (applyOctaveChange octChange oct)



{- ---------------------------- playground -----------------------------------

c = PitchClass C Natural
c ^. noteName
c ^. accidental

c4 = Pitch C Natural (Octave 4)

pitchToRational c4

splitFraction $ pitchToRational $  Pitch G QuarterSharp (Octave 5)

fromRational $ pitchToRational $  Pitch E QuarterSharp (Octave 3)

-- Changes the accidental of 'c' to Sharp
c & accidental .~ Sharp
--C Sharp

c & accidental %~ (\x -> addAccidental x (1%2))
-- C QuarterSharp

pitchClasses = map (\x -> PitchClass x Natural) [C .. B]

-- Changes the accidental of every PitchClass in the list to Flat

pitchClasses & each . accidental .~ Flat
--[C Flat,D Flat,E Flat,F Flat,G Flat,A Flat,B Flat]

-- Checks if 'c' has an accidental ojustf Natural
has (accidental . only Natural) c
--True

-- If the accidental is Natural, change it to Flat.
c & accidental . filtered (== Natural) .~ Flat
C Flat

p = Pitch C Natural (Octave 4)

p ^. noteName
-- C

p ^. accidental
-- Natural

p ^. octave
-- Octave 4

p & accidental .~ Sharp  -- Changes the accidental of 'p' to Sharp
-- C Sharp Octave 4

p & accidental %~ (\x -> addAccidental x (1%2))
-- C QuarterSharp Octave 4

pitches = map (\x -> Pitch x Natural (Octave 4)) [C .. B]
pitches & each . accidental .~ Flat  -- Changes the accidental of every Pitch in the list to Flat

-- [C Flat Octave 4,D Flat Octave 4,E Flat Octave 4,F Flat Octave 4,G Flat Octave 4,A Flat Octave 4,B Flat Octave 4]

has (accidental . only Natural) p  -- Checks if 'p' has an accidental of Natural

-- True

p & accidental . filtered (== Natural) .~ Flat  -- If the accidental is Natural, change it to Flat.

-- C Flat Octave 4

p & octave .~ Octave 5  -- Change the octave of 'p' to 5

-- C Natural Octave 5

p & octave %~ (\(Octave o) -> Octave (o + 1))  -- Increment the octave by 1

-- C Natural Octave 5

-------------------------------------------------------------------------------- -}



notes :: [NoteName]
notes = [C, D, E, F, G, A, B]

createPitchMap :: [NoteName] -> Map.Map String Pitch
createPitchMap = foldr (Map.union . createPitchesForNote) Map.empty

createPitchesForNote :: NoteName -> Map.Map String Pitch
createPitchesForNote note = Map.fromList $ do
  acc <- [Natural, Sharp, Flat, QuarterSharp, QuarterFlat, ThreeQuartersFlat, ThreeQuartersSharp, DoubleFlat, DoubleSharp]
  let modifier = case acc of
        Sharp              -> "is"
        Flat               -> "es"
        QuarterSharp       -> "ih"
        QuarterFlat        -> "eh"
        Natural            -> ""
        ThreeQuartersFlat  -> "eseh"
        ThreeQuartersSharp -> "isih"
        DoubleFlat         -> "eses"
        DoubleSharp        -> "isis"
        _                  -> ""
  (octaveSuffix, oct) <- [("", 4), ("'", 5), ("''", 6), ("'''", 7), ("''''", 8), ("'''''", 9), ("_", 3), ("__", 2), ("___", 1), ("____", 0), ("_____", -1)]
  pure (fmap toLower (show note) <> modifier <> octaveSuffix, Pitch note acc (Octave oct))

pitchMap :: Map.Map String Pitch
pitchMap = createPitchMap notes

concatForM :: (Monad m) => [a] -> (a -> m [b]) -> m [b]
concatForM xs action = concat <$> forM xs action

generatePitchVars :: [String] -> Q [Dec]
generatePitchVars pitchNames = concatForM pitchNames $ \name -> do
  let varName = mkName name
      pitchVal = AppE (VarE 'fromString) (LitE (StringL name))
  pure [SigD varName (ConT ''Pitch), ValD (VarP varName) (NormalB pitchVal) []]

