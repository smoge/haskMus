{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Pitch.Pitch where

import Control.Lens hiding (elements)
-- import Test.QuickCheck (Arbitrary (arbitrary), Gen, elements)

import Control.Monad (forM)
import Data.Char (toLower)
import Data.Data
import Data.Fixed (mod')
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.String
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Pitch.Accidental
import Data.Ratio
import Data.List (find)
import Data.List.Extra (enumerate)

data NoteName = C | D | E | F | G | A | B
  deriving (Eq, Ord, Show, Enum, Bounded, Lift, Data)

data IntervalBasis = Chromatic | Diatonic
  deriving (Eq, Ord, Show, Enum)

data PitchClass = PitchClass
  { _noteName :: !NoteName,
    _accidental :: !Accidental
  }
  deriving (Eq, Lift, Data, Ord)

data Pitch = Pitch
  { _noteName :: !NoteName,
    _accidental :: !Accidental,
    _octave :: !Octave
  }
  deriving (Eq, Lift, Data)

newtype Octave = Octave {unOctave :: Int}
  deriving (Eq, Ord, Lift, Data, Enum)

-- deriving instance Data Pitch

mkPitch :: NoteName -> Accidental -> Octave -> Pitch
mkPitch = Pitch

{-# INLINE mkPitch' #-}
mkPitch' :: PitchClass -> Octave -> Pitch
mkPitch' pc o = Pitch {_noteName = pc ^. noteName, _accidental = pc ^. accidental, _octave = o}

data SomeNote = forall notename. (IsNoteName notename) => SomeNote notename

-------------------------------------------------------------------------------------
--  Type classes
-------------------------------------------------------------------------------------

class NoteClass (noteName :: NoteName) where
  sayNote :: String

class IsNoteName a where
  toNoteName :: a -> NoteName

class HasNoteName a where
  noteName :: Lens' a NoteName

class HasAccidental a where
  accidental :: Lens' a Accidental

class HasPitchClass a where
  pitchClass :: Lens' a PitchClass

class HasOctave a where
  octave :: Lens' a Octave

-------------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------------

-- | Typeclass instance for retrieving the note name of a Pitch.
instance HasNoteName Pitch where
  -- \| Extracts the note name from a Pitch and applies a function to it.
  noteName f (Pitch nn acc o) = (\nn' -> Pitch nn' acc o) <$> f nn

instance HasOctave Pitch where
  octave f (Pitch nn acc o) = Pitch nn acc <$> f o

-- | Typeclass that represents a type with an accidental.
instance HasAccidental Pitch where
  -- \| Modifies the accidental of a Pitch using the provided function.
  accidental f (Pitch nn acc o) = (\acc' -> Pitch nn acc' o) <$> f acc

-- | Typeclass that represents a type with a pitch class.
instance HasPitchClass Pitch where
  -- \| Lens that focuses on the pitch class of a Pitch.
  pitchClass :: Lens' Pitch PitchClass
  pitchClass f (Pitch nn acc o) = (\(PitchClass nn' acc') -> Pitch nn' acc' o) <$> f (PitchClass nn acc)

-- | Typeclass that represents a type with a note name.
instance HasNoteName PitchClass where
  -- \| Modifies the note name of a PitchClass using the provided function.
  noteName f (PitchClass nn acc) = (`PitchClass` acc) <$> f nn

-- | Typeclass that represents a type with an accidental.
instance HasAccidental PitchClass where
  -- \| Modifies the accidental of a PitchClass using the provided function.
  accidental f (PitchClass nn acc) = PitchClass nn <$> f acc

-- | Typeclass that represents a type that can be converted to a NoteName.
instance IsNoteName SomeNote where
  -- \| Converts a SomeNote to a NoteName.
  toNoteName :: SomeNote -> NoteName
  toNoteName (SomeNote nn) = toNoteName nn

instance Show SomeNote where
  show = show . toNoteName

instance NoteClass C where
  sayNote = "c"

instance NoteClass D where
  sayNote = "d"

instance NoteClass E where
  sayNote = "e"

instance NoteClass F where
  sayNote = "f"

instance NoteClass G where
  sayNote = "g"

instance NoteClass A where
  sayNote = "a"

instance NoteClass B where
  sayNote = "b"

instance IsString NoteName where
  fromString :: String -> NoteName
  fromString "c" = C
  fromString "d" = D
  fromString "e" = E
  fromString "f" = F
  fromString "g" = G
  fromString "a" = A
  fromString "b" = B
  fromString s = error $ "Invalid NoteName string: " <> s

instance Show PitchClass where
  show (PitchClass name acc) = show name <> " " <> show acc

instance Show Octave where
  show (Octave o) = "Octave " <> show o

instance Show Pitch where
  show :: Pitch -> String
  show (Pitch name acc oct) = show name <> " " <> show acc <> " " <> show oct

-- Functions
makeLensesFor
  [ ("PitchClass", "_noteName"),
    ("PitchClass", "_accidental"),
    ("Pitch", "_noteName"),
    ("Pitch", "_accidental"),
    ("Pitch", "_octave")
  ]
  ''PitchClass

{-# INLINE pcToRational #-}
pcToRational :: PitchClass -> Rational
pcToRational pc = base + acVal
  where
    base = case Prelude.lookup nm noteNameToRational' of
      Just val -> val
      Nothing -> error "NoteName not found"
    acVal = accidentalToSemitones ac :: Rational
    nm = pc ^. noteName
    ac = pc ^. accidental

(=~) :: PitchClass -> PitchClass -> Bool
pc1 =~ pc2 = (pcToRational pc1 `mod'` 12) == (pcToRational pc2 `mod'` 12)

noteNameToRationalMap :: Map.Map NoteName Rational
noteNameToRationalMap = Map.fromList [(C, 0), (D, 2), (E, 4), (F, 5), (G, 7), (A, 9), (B, 11)]

noteNameToRational :: NoteName -> Rational
noteNameToRational name = fromMaybe (error ("NoteName " <> show name <> " not found")) (Map.lookup name noteNameToRationalMap)

noteNameToRational' :: [(NoteName, Rational)]
noteNameToRational' = [(C, 0), (D, 2), (E, 4), (F, 5), (G, 7), (A, 9), (B, 11)]

{-
noteNameToRational :: NoteName -> Rational
noteNameToRational name = case Prelude.lookup name noteNameToRational' of
  Just val -> val
  Nothing -> error ("NoteName " <> show name <> " not found")
 -}

{-# INLINE pitchToRational #-}
pitchToRational :: Pitch -> Rational
pitchToRational (Pitch nm ac oct) = pcToRational (PitchClass nm ac) + fromIntegral (unOctave oct + 1) * 12

{-# INLINE rationalToFloat #-}
rationalToFloat :: Rational -> Float
rationalToFloat = fromRational

{- pitchToFloat :: Pitch -> Float
pitchToFloat = rationalToFloat . pitchToRational
-}

{-# INLINE pitchToFloat #-}
pitchToFloat :: Pitch -> Float
pitchToFloat (Pitch nm ac oct) = fromRational $ pcToRational (PitchClass nm ac) + fromIntegral (unOctave oct + 1) * 12

allPitchClasses :: [PitchClass]
allPitchClasses = liftA2 PitchClass [C, D, E, F, G, A, B] allAccidentals

allPCRationals :: [Rational]
allPCRationals = fmap pcToRational allPitchClasses

enharmonicPCEquivs :: Rational -> [(Rational, PitchClass)]
enharmonicPCEquivs val =
  [(v, pc) | pc <- allPitchClasses, let v = pcToRational pc, v `mod'` 12 == val `mod'` 12]

enharmonicPCEquivs' :: PitchClass -> [(Rational, PitchClass)]
enharmonicPCEquivs' pc =
  [(v, pc') | pc' <- allPitchClasses, let v = pcToRational pc', v `mod'` 12 == pcToRational pc `mod'` 12]

type EnharmonicMapping = [(Rational, [PitchClass])]

enharmonicMapping :: [Rational] -> EnharmonicMapping
enharmonicMapping = fmap (\r -> (r, fmap snd (enharmonicPCEquivs r)))

enharmonics :: PitchClass -> [PitchClass]
enharmonics pc = fromMaybe [pc] (lookup (pcToRational pc) enharmonicMap)
  where
    enharmonicMap = enharmonicMapping allPCRationals

allEnharmonics :: [[PitchClass]]
allEnharmonics = fmap enharmonics allPitchClasses

allEnharmonicsMapping :: [(PitchClass, [PitchClass])]
allEnharmonicsMapping = zip allPitchClasses allEnharmonics

c4 :: Pitch
c4 = Pitch C Natural (Octave 4)

preferedAccidentalPC :: PitchClass -> PitchClass
preferedAccidentalPC pc
  | pc == PitchClass C Flat   = PitchClass B Natural
  | pc == PitchClass D Sharp  = PitchClass E Flat
  | pc == PitchClass A Sharp  = PitchClass B Flat
  | pc == PitchClass B Sharp  = PitchClass C Natural
  | otherwise                 = pc

preferredAccidentalP :: Pitch -> Pitch
preferredAccidentalP pitch@(Pitch _ _ oct@(Octave o))
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
  PitchClass C Flat -> PitchClass B Natural
  PitchClass C Sharp -> pc_
  PitchClass D Flat -> PitchClass C Sharp
  PitchClass D Sharp -> PitchClass E Flat
  PitchClass E Sharp -> PitchClass F Natural
  PitchClass F Flat -> PitchClass E Natural
  PitchClass F Sharp -> pc_
  PitchClass G Flat -> PitchClass F Sharp
  PitchClass G Sharp -> pc_
  PitchClass A Flat -> pc_
  PitchClass A Sharp -> PitchClass B Flat
  PitchClass B Sharp -> PitchClass C Natural
  _ -> pc_

normalizeEnharmonicPitch :: Pitch -> Pitch
normalizeEnharmonicPitch pitch = case pitch of
  Pitch C Flat oct -> Pitch B Natural (octaveDown oct)
  Pitch C DoubleFlat oct -> Pitch B Flat (octaveDown oct)
  Pitch D Flat oct -> Pitch C Sharp oct
  Pitch D Sharp oct -> Pitch E Flat oct
  Pitch E Sharp oct -> Pitch F Natural oct
  Pitch F Flat oct -> Pitch E Natural oct
  Pitch G Flat oct -> Pitch F Sharp oct
  Pitch A Sharp oct -> Pitch B Flat oct
  Pitch B Sharp oct -> Pitch C Natural (octaveUp oct)
  Pitch B ThreeQuartersSharp oct -> Pitch C QuarterSharp (octaveUp oct)
  _ -> pitch 
  where
    octaveDown (Octave o) = Octave (o - 1)
    octaveUp (Octave o) = Octave (o + 1)

normalizeEnharmonicPCs :: [PitchClass] -> [PitchClass]
normalizeEnharmonicPCs = fmap normalizeEnharmonicPC

normalizeEnharmonicPitches :: [Pitch] -> [Pitch]
normalizeEnharmonicPitches = fmap normalizeEnharmonicPitch

data Rule = Rule
  { fromPitch :: PitchClass
  , toPitch :: PitchClass
  , octaveChange :: OctaveChange
  } deriving (Show, Eq)


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

-- Example usage of the rules
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
  , keep (pc C Sharp)
  , pc C DoubleSharp    =:> pc D Natural
  , pc D Flat           =:> pc C Sharp
  , pc D Sharp          =:> pc E Flat
  , pc E Sharp          =:> pc F Natural
  , pc E DoubleSharp    =:> pc F Sharp
  , pc F Flat           =:> pc E Natural
  , keep (pc F Sharp)
  , pc G Flat           =:> pc F Sharp
  , keep (pc G Sharp)
  , pc A Flat           =:> pc G Sharp
  , pc A Sharp          =:> pc B Flat
  , pc B Sharp          +:> pc C Natural
  , pc B DoubleSharp    +:> pc C Sharp

  , keep (pc C QuarterSharp)
  , pc C ThreeQuartersSharp =:> pc D QuarterFlat
  , keep (pc D QuarterFlat)
  , pc D ThreeQuartersFlat  =:> pc C QuarterSharp
  , pc E QuarterSharp       =:> pc F QuarterFlat
  , keep (pc F QuarterSharp)
  , keep (pc G QuarterSharp)
  ]

prettyPrintRules :: [Rule] -> IO ()
prettyPrintRules = mapM_ (\(Rule from_ to_ oct) ->
  putStrLn (show from_ <> (" -> " <> show to_ <>
             case oct of
               NoChange -> ""
               OctaveUp -> " (Octave Up)"
               OctaveDown -> " (Octave Down)")))


-- >>> prettyPrintRules enharmonicRules

applyRulesToPitches :: [Rule] -> [Pitch] -> [Pitch]
applyRulesToPitches rules = fmap (applyRulesToPitch rules)

applyRulesToPitch :: [Rule] -> Pitch -> Pitch
applyRulesToPitch rules pitch@(Pitch name acc oct) =
  case findMatchingRule (PitchClass name acc) rules of
    Just (Rule _ (PitchClass toName toAcc) octChange) ->
      Pitch toName toAcc (applyOctaveChange octChange oct)
    Nothing -> pitch

findMatchingRule :: PitchClass -> [Rule] -> Maybe Rule
findMatchingRule pc_ = find (\(Rule fromPC _ _) -> fromPC == pc_)


-- Better ?


applyRulesWithMap :: RuleMap -> Pitch -> Pitch
applyRulesWithMap ruleMap (Pitch nn acc oct) =
    case Map.lookup (PitchClass nn acc) ruleMap of
        Just (Rule _ toPC octChange) -> 
            Pitch (toPC ^. noteName) (toPC ^. accidental) (applyOctaveChange octChange oct)
        Nothing -> Pitch nn acc oct

applyRuleMapToPitches :: RuleMap -> [Pitch] -> [Pitch]
applyRuleMapToPitches = fmap . applyRulesWithMap


-- ! test
-- >applyRulesToPitches enharmonicRules2 ( mkPitchesFromIntervals c4 minorScaleInterval)
-- [C Natural Octave 4,D Natural Octave 4,E Flat Octave 4,F Natural Octave 4,G Natural Octave 4,G Sharp Octave 4,B Flat Octave 4]


-- demonstrateEnharmonicNormalization

-- > preferredAccidentalList $  mkPitchesFromIntervals c4 minorScaleInterval
--[C Natural Octave 4,D Natural Octave 4,E Flat Octave 4,F Natural Octave 4,G Natural Octave 4,G Sharp Octave 4,B Flat Octave 4]



type RuleMap = Map.Map PitchClass Rule

buildRuleMap :: [Rule] -> RuleMap
buildRuleMap rules = Map.fromList [(fromPitch rule, rule) | rule <- rules]

--applyRulesWithMap :: RuleMap -> Pitch -> Pitch
--applyRulesWithMap ruleMap pitch@(Pitch nn acc _) =
--  let pc_ = PitchClass nn acc
--  in case Map.lookup pc_ ruleMap of
--       Just rule -> applyRule pitch rule
--       Nothing   -> pitch

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

-- -- !FIXME: MOVE TO TESTS

-- instance Arbitrary NoteName where
--   arbitrary = elements [C, D, E, F, G, A, B]

-- instance Arbitrary PitchClass where
--   arbitrary = PitchClass <$> arbitrary <*> arbitrary

-- instance Arbitrary Octave where
--   arbitrary :: Gen Octave
--   arbitrary = Octave <$> arbitrary

-- instance Arbitrary Pitch where
--   arbitrary = Pitch <$> arbitrary <*> arbitrary <*> arbitrary

-- -- PitchClass C Flat

notes :: [NoteName]
notes = [C, D, E, F, G, A, B]

-- Function to create a map of pitch names to pitch values
createPitchMap :: [NoteName] -> Map.Map String Pitch
createPitchMap = foldr (Map.union . createPitchesForNote) Map.empty

createPitchesForNote :: NoteName -> Map.Map String Pitch
createPitchesForNote note = Map.fromList $ do
  acc <- [Natural, Sharp, Flat, QuarterSharp, QuarterFlat, ThreeQuartersFlat, ThreeQuartersSharp, DoubleFlat, DoubleSharp]
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
        _ -> ""
  (octaveSuffix, oct) <- [("", 4), ("'", 5), ("''", 6), ("'''", 7), ("''''", 8), ("'''''", 9), ("_", 3), ("__", 2), ("___", 1), ("____", 0), ("_____", -1)]
  pure (fmap toLower (show note) <> modifier <> octaveSuffix, Pitch note acc (Octave oct))

-- Create pitch map
pitchMap :: Map.Map String Pitch
pitchMap = createPitchMap notes

concatForM :: (Monad m) => [a] -> (a -> m [b]) -> m [b]
concatForM xs action = concat <$> forM xs action

{- generatePitchVars :: [String] -> Q [Dec]
generatePitchVars pitchNames =
    concatForM pitchNames $ \name -> do
        let varName = mkName name
        let pitchVal = AppE (VarE 'fromString) (LitE (StringL name))
        pure [SigD varName (ConT ''Pitch), ValD (VarP varName) (NormalB pitchVal) []]
 -}

generatePitchVars :: [String] -> Q [Dec]
generatePitchVars pitchNames = concatForM pitchNames $ \name -> do
  let varName = mkName name
      pitchVal = AppE (VarE 'fromString) (LitE (StringL name))
  pure [SigD varName (ConT ''Pitch), ValD (VarP varName) (NormalB pitchVal) []]



newtype Interval = Interval { semitones :: Rational }
  deriving (Eq, Ord, Show, Lift, Data)

instance Num Interval where
  (+) (Interval a) (Interval b) = Interval (a + b)
  (-) (Interval a) (Interval b) = Interval (a - b)
  (*) (Interval a) (Interval b) = Interval (a * b)
  negate (Interval a) = Interval (negate a)
  abs (Interval a) = Interval (abs a)
  signum (Interval a) = Interval (signum a)
  fromInteger n = Interval (fromInteger n)

instance Semigroup Interval where
  (<>) :: Interval -> Interval -> Interval
  (<>) = (+)

instance Monoid Interval where
  mempty :: Interval
  mempty = Interval 0


rationalToPitch :: Rational -> Pitch
rationalToPitch semitones =
  let

    octave_ = Octave (floor (semitones / 12) - 1)

    semitoneInOctave = semitones `mod'` 12

    (noteName_, naturalSemitone) = (case reverse (takeWhile (\(_, s) -> s <= semitoneInOctave) noteNameToRational') of
       x : _ -> x
       [] -> error "Pitch rationalToPitch Invalid semitone value")

    accidentalValue = semitoneInOctave - naturalSemitone
    accidental_ = case accidentalValue of
      0 -> Natural
      x | x > 0 && x < 0.25 -> Natural
        | x >= 0.25 && x < 0.75 -> QuarterSharp
        | x >= 0.75 && x < 1.25 -> Sharp
        | x >= 1.25 && x < 1.75 -> ThreeQuartersSharp
        | x >= 1.75 -> DoubleSharp
        | x < 0 && x > -0.25 -> Natural
        | x <= -0.25 && x > -0.75 -> QuarterFlat
        | x <= -0.75 && x > -1.25 -> Flat
        | x <= -1.25 && x > -1.75 -> ThreeQuartersFlat
        | x <= -1.75 -> DoubleFlat
        | otherwise -> error "Invalid accidental value"
  in
    Pitch noteName_ accidental_ octave_

rationalToPitchClass :: Rational -> PitchClass
rationalToPitchClass semitones =
  let pitch = rationalToPitch semitones
  in PitchClass (pitch ^. noteName) (pitch ^. accidental)


(-:) :: Pitch -> Pitch -> Interval
p1 -: p2 = Interval (pitchToRational p2 - pitchToRational p1)

(+.) :: Pitch -> Interval -> Pitch
p +. i = rationalToPitch (pitchToRational p + semitones i)

(-.) :: Pitch -> Interval -> Pitch
p -. i = rationalToPitch (pitchToRational p - semitones i)

fromSemitones :: Rational -> Interval
fromSemitones = Interval


pitchToInterval :: Pitch -> Pitch -> Interval
pitchToInterval p1 p2 = Interval (pitchToRational p2 - pitchToRational p1)

invertInterval :: Interval -> Interval
invertInterval (Interval s) = Interval (12 - (s `mod'` 12))

isCompoundInterval :: Interval -> Bool
isCompoundInterval (Interval s) = s >= 12

simplifyInterval :: Interval -> Interval
simplifyInterval (Interval s) = Interval (s `mod'` 12)

(=~=) :: Pitch -> Pitch -> Bool
p1 =~= p2 = pitchToRational p1 `mod'` 12 == pitchToRational p2 `mod'` 12

getInterval :: Pitch -> Pitch -> Interval
getInterval = (-:)

transposeUp :: Pitch -> Interval -> Pitch
transposeUp = (+.)

transposeDown :: Pitch -> Interval -> Pitch
transposeDown = (-.)

createScale :: Pitch -> [Interval] -> [Pitch]
createScale = scanl transposeUp
-- createScale start intervals = scanl transposeUp start intervals


data IntervalName =
    Unison
  | AugmentedUnison
  | MinorSecond | NeutralSecond | MajorSecond
  | AugmentedSecond
  | MinorThird | NeutralThird | MajorThird
  | AugmentedThird
  | DiminishedFourth | PerfectFourth | AugmentedFourth
  | DiminishedFifth | Tritone | AugmentedFourth'
  | DiminishedFifth' | PerfectFifth | AugmentedFifth
  | MinorSixth | NeutralSixth | MajorSixth
  | AugmentedSixth
  | DiminishedSeventh | MinorSeventh | NeutralSeventh | MajorSeventh
  | PerfectOctave
  | AugmentedOctave
  | MinorNinth | NeutralNinth | MajorNinth
  | AugmentedNinth
  | MinorTenth | NeutralTenth | MajorTenth
  | AugmentedTenth
  | PerfectEleventh | AugmentedEleventh
  | DiminishedTwelfth | PerfectTwelfth | AugmentedTwelfth
  | MinorThirteenth | NeutralThirteenth | MajorThirteenth
  | AugmentedThirteenth
  | MinorFourteenth | NeutralFourteenth | MajorFourteenth
  | DoubleOctave
  deriving (Eq, Ord, Show, Enum, Bounded)

intervalMap :: Map.Map IntervalName Rational
intervalMap = Map.fromList
  [ (Unison, 0)
  , (AugmentedUnison, 1/2)
  , (MinorSecond, 1)
  , (NeutralSecond, 3/2)
  , (MajorSecond, 2)
  , (AugmentedSecond, 5/2)
  , (MinorThird, 3)
  , (NeutralThird, 7/2)
  , (MajorThird, 4)
  , (AugmentedThird, 9/2)
  , (DiminishedFourth, 4)
  , (PerfectFourth, 5)
  , (AugmentedFourth, 11/2)
  , (DiminishedFifth, 6)
  , (Tritone, 6)
  , (AugmentedFourth', 13/2)
  , (DiminishedFifth', 13/2)
  , (PerfectFifth, 7)
  , (AugmentedFifth, 15/2)
  , (MinorSixth, 8)
  , (NeutralSixth, 17/2)
  , (MajorSixth, 9)
  , (AugmentedSixth, 19/2)
  , (DiminishedSeventh, 9)
  , (MinorSeventh, 10)
  , (NeutralSeventh, 21/2)
  , (MajorSeventh, 11)
  , (PerfectOctave, 12)
  , (AugmentedOctave, 25/2)
  , (MinorNinth, 13)
  , (NeutralNinth, 27/2)
  , (MajorNinth, 14)
  , (AugmentedNinth, 29/2)
  , (MinorTenth, 15)
  , (NeutralTenth, 31/2)
  , (MajorTenth, 16)
  , (AugmentedTenth, 33/2)
  , (PerfectEleventh, 17)
  , (AugmentedEleventh, 35/2)
  , (DiminishedTwelfth, 18)
  , (PerfectTwelfth, 19)
  , (AugmentedTwelfth, 39/2)
  , (MinorThirteenth, 20)
  , (NeutralThirteenth, 41/2)
  , (MajorThirteenth, 21)
  , (AugmentedThirteenth, 43/2)
  , (MinorFourteenth, 22)
  , (NeutralFourteenth, 45/2)
  , (MajorFourteenth, 23)
  , (DoubleOctave, 24)
  ]

intervalFromName :: IntervalName -> Interval
intervalFromName name = case Map.lookup name intervalMap of
  Just semitones -> Interval semitones
  Nothing -> error $ "Klarenz ERROR: in Pitch.Pitch function intervalFromName: Unknown interval: " <> show name


nameFromInterval :: Interval -> Maybe IntervalName
nameFromInterval (Interval semitones) =
  fmap fst $ find (\(_, s) -> s == semitones) $ Map.toList intervalMap


unison, augmentedUnison, minorSecond, neutralSecond, majorSecond, augmentedSecond, minorThird :: Interval
neutralThird, majorThird, augmentedThird, diminishedFourth, perfectFourth :: Interval
augmentedFourth, diminishedFifth, tritone, augmentedFourth', diminishedFifth' :: Interval
perfectFifth, augmentedFifth, minorSixth, neutralSixth, majorSixth, augmentedSixth :: Interval
diminishedSeventh, minorSeventh, neutralSeventh, majorSeventh, perfectOctave :: Interval
augmentedOctave, minorNinth, neutralNinth, majorNinth, augmentedNinth, minorTenth :: Interval
neutralTenth, majorTenth, augmentedTenth, perfectEleventh, augmentedEleventh :: Interval
diminishedTwelfth, perfectTwelfth, augmentedTwelfth, minorThirteenth, neutralThirteenth :: Interval
majorThirteenth, augmentedThirteenth, minorFourteenth, neutralFourteenth :: Interval
majorFourteenth, doubleOctave :: Interval

unison = intervalFromName Unison
augmentedUnison = intervalFromName AugmentedUnison
minorSecond = intervalFromName MinorSecond
neutralSecond = intervalFromName NeutralSecond
majorSecond = intervalFromName MajorSecond
augmentedSecond = intervalFromName AugmentedSecond
minorThird = intervalFromName MinorThird
neutralThird = intervalFromName NeutralThird
majorThird = intervalFromName MajorThird
augmentedThird = intervalFromName AugmentedThird
diminishedFourth = intervalFromName DiminishedFourth
perfectFourth = intervalFromName PerfectFourth
augmentedFourth = intervalFromName AugmentedFourth
diminishedFifth = intervalFromName DiminishedFifth
tritone = intervalFromName Tritone
augmentedFourth' = intervalFromName AugmentedFourth'
diminishedFifth' = intervalFromName DiminishedFifth'
perfectFifth = intervalFromName PerfectFifth
augmentedFifth = intervalFromName AugmentedFifth
minorSixth = intervalFromName MinorSixth
neutralSixth = intervalFromName NeutralSixth
majorSixth = intervalFromName MajorSixth
augmentedSixth = intervalFromName AugmentedSixth
diminishedSeventh = intervalFromName DiminishedSeventh
minorSeventh = intervalFromName MinorSeventh
neutralSeventh = intervalFromName NeutralSeventh
majorSeventh = intervalFromName MajorSeventh
perfectOctave = intervalFromName PerfectOctave
augmentedOctave = intervalFromName AugmentedOctave
minorNinth = intervalFromName MinorNinth
neutralNinth = intervalFromName NeutralNinth
majorNinth = intervalFromName MajorNinth
augmentedNinth = intervalFromName AugmentedNinth
minorTenth = intervalFromName MinorTenth
neutralTenth = intervalFromName NeutralTenth
majorTenth = intervalFromName MajorTenth
augmentedTenth = intervalFromName AugmentedTenth
perfectEleventh = intervalFromName PerfectEleventh
augmentedEleventh = intervalFromName AugmentedEleventh
diminishedTwelfth = intervalFromName DiminishedTwelfth
perfectTwelfth = intervalFromName PerfectTwelfth
augmentedTwelfth = intervalFromName AugmentedTwelfth
minorThirteenth = intervalFromName MinorThirteenth
neutralThirteenth = intervalFromName NeutralThirteenth
majorThirteenth = intervalFromName MajorThirteenth
augmentedThirteenth = intervalFromName AugmentedThirteenth
minorFourteenth = intervalFromName MinorFourteenth
neutralFourteenth = intervalFromName NeutralFourteenth
majorFourteenth = intervalFromName MajorFourteenth
doubleOctave = intervalFromName DoubleOctave

--nameFromInterval :: Interval -> Maybe IntervalName
--nameFromInterval (Interval semitones) =
--  fmap fst $ find (\(_, s) -> s == semitones) $ Map.toList intervalMap

allIntervalNames :: [IntervalName]
allIntervalNames = enumerate

allIntervals :: [Interval]
allIntervals = fmap intervalFromName allIntervalNames

instance Fractional Interval where
  (/) :: Interval -> Interval -> Interval
  (/) (Interval a) (Interval b) = Interval (a / b)
  fromRational :: Rational -> Interval
  fromRational = Interval

instance Real Interval where
  toRational :: Interval -> Rational
  toRational (Interval r) = r

instance RealFrac Interval where
  properFraction :: Integral b => Interval -> (b, Interval)
  properFraction (Interval r) = (fromInteger i, Interval f)
    where (i, f) = properFraction r

complementInterval :: Interval -> Interval
complementInterval (Interval semitones) = Interval (12 - (semitones `mod'` 12))

createNamedScale :: Pitch -> [IntervalName] -> [Pitch]
createNamedScale start intervalNames =
  scanl (+.) start (fmap intervalFromName intervalNames)

majorScaleIntervalNames :: [IntervalName]
majorScaleIntervalNames = [Unison, MajorSecond, MajorThird, PerfectFourth, PerfectFifth, MajorSixth, MajorSeventh]

minorScaleIntervals :: [IntervalName]
minorScaleIntervals = [Unison, MajorSecond, MinorThird, PerfectFourth, PerfectFifth, MinorSixth, MinorSeventh]

minorScale :: Pitch -> [Pitch]
minorScale start = createNamedScale start minorScaleIntervals


majorScale :: Pitch -> [Pitch]
majorScale start = createNamedScale start majorScaleIntervalNames

majorScaleInterval :: [Interval]
majorScaleInterval = fmap intervalFromName majorScaleIntervalNames

minorScaleInterval :: [Interval]
minorScaleInterval = fmap intervalFromName minorScaleIntervals


--
-- >majorScaleInterval
--[Interval {semitones = 0 % 1},Interval {semitones = 2 % 1},Interval {semitones = 4 % 1},Interval {semitones = 5 % 1},Interval {semitones = 7 % 1},Interval {semitones = 9 % 1},Interval {semitones = 11 % 1}]


intervalsToNames :: [Interval] -> [IntervalName]
intervalsToNames intervals = fromMaybe (error "Invalid interval") . nameFromInterval <$> intervals


mkPitchesFromIntervals :: Pitch -> [Interval] -> [Pitch]
mkPitchesFromIntervals start intervals =
  addInterval start <$> intervals
  where
    addInterval pitch (Interval semitones) =
      rationalToPitch (pitchToRational pitch + semitones)


cMinorScale :: [PitchClass]
cMinorScale =
  [ PitchClass C Natural
  , PitchClass D Natural
  , PitchClass E Flat
  , PitchClass F Natural
  , PitchClass G Natural
  , PitchClass A Flat
  , PitchClass B Flat
  ]

cMinorEnharmonicMapping :: EnharmonicMapping
cMinorEnharmonicMapping = enharmonicMapping $ pcToRational <$> cMinorScale

prettyPrintEnharmonicMapping :: EnharmonicMapping -> IO ()
prettyPrintEnharmonicMapping  = mapM_ printEntry
  where
    printEntry (ratio, pcs) = do
      putStrLn $ "Semitones from C: " <> show (fromRational ratio :: Double)
      putStrLn "Enharmonic equivalents:"
      mapM_ (\pc -> putStrLn $ "  " <> show pc) pcs
      putStrLn ""
