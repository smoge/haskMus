{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Pitch.Accidental (
    Accidental (..),
    accidentalToSemitones,
    semitonesToAccidental,
    modifyAccidental,
    accToLily,
    checkAccidental,
    addAccidental,
    allAccidentals,
    allSemitones,
    sharpenQ,
    sharpenS,
    flattenQ,
    flattenS,
    accidentalCompare,
    invertAccidental,
) where

import Data.Data
import Data.List (isPrefixOf)
import Data.Ord (comparing)
import Data.Ratio
import Data.String
import qualified Data.Text as T
import Language.Haskell.TH.Syntax (Lift)


data Accidental
    = DoubleFlat
    | ThreeQuartersFlat
    | Flat
    | QuarterFlat
    | Natural
    | QuarterSharp
    | Sharp
    | ThreeQuartersSharp
    | DoubleSharp
    | Custom Rational
    deriving (Eq, Ord, Show, Lift, Data)


-- instance Lift Accidental

{- |
 >>> map sharpenQ allAccidentals
 >>> map sharpenS allAccidentals
 >>> map flattenQ allAccidentals
 >>> map flattenS allAccidentals
 [ThreeQuartersFlat,Flat,QuarterFlat,Natural,QuarterSharp,Sharp,ThreeQuartersSharp,DoubleSharp,Custom (5 % 2)]
 [Flat,QuarterFlat,Natural,QuarterSharp,Sharp,ThreeQuartersSharp,DoubleSharp,Custom (5 % 2),Custom (3 % 1)]
 [Custom ((-5) % 2),DoubleFlat,ThreeQuartersFlat,Flat,QuarterFlat,Natural,QuarterSharp,Sharp,ThreeQuartersSharp]
 [Custom ((-3) % 1),Custom ((-5) % 2),DoubleFlat,ThreeQuartersFlat,Flat,QuarterFlat,Natural,QuarterSharp,Sharp]
-}
sharpenQ :: Accidental -> Accidental
sharpenQ acc = modifyAccidental acc (+ (1 / 2))


sharpenS :: Accidental -> Accidental
sharpenS acc = modifyAccidental acc (+ 1)


flattenQ :: Accidental -> Accidental
flattenQ acc = modifyAccidental acc (+ ((-1) % 2))


flattenS :: Accidental -> Accidental
flattenS acc = modifyAccidental acc (+ (-1))


accidentalCompare :: Accidental -> Accidental -> Ordering
accidentalCompare = comparing accidentalToSemitones


{- -----------------------------------------------------------------------------

scrambled1 :: [Accidental]
scrambled1 = [QuarterFlat, DoubleSharp, ThreeQuartersSharp, Sharp, Natural, QuarterSharp, DoubleFlat, Flat, ThreeQuartersFlat, Custom (2 % 3)]

sortBy accidentalCompare scrambled1

----------------------------------------------------------------------------- -}

class IsAccidental a where
    toAccidental :: a -> Accidental


data SomeAccidental
    = forall accidental.
        (IsAccidental accidental) =>
      SomeAccidental accidental


instance IsAccidental SomeAccidental where
    toAccidental :: SomeAccidental -> Accidental
    toAccidental (SomeAccidental acc) = toAccidental acc


instance IsAccidental Accidental where
    toAccidental :: Accidental -> Accidental
    toAccidental = id


instance IsAccidental Rational where
    toAccidental :: Rational -> Accidental
    toAccidental = semitonesToAccidental


class AccClass (accNme :: Accidental) where
    sayAccidental :: String


instance Show SomeAccidental where
    show = show . toAccidental


instance AccClass DoubleFlat where
    sayAccidental = "ff"


instance AccClass Flat where
    sayAccidental = "f"


instance AccClass Natural where
    sayAccidental = "n"


instance AccClass QuarterFlat where
    sayAccidental = "qf"


instance AccClass QuarterSharp where
    sayAccidental = "qs"


instance AccClass Sharp where
    sayAccidental = "s"


instance AccClass ThreeQuartersSharp where
    sayAccidental = "qss"


instance AccClass ThreeQuartersFlat where
    sayAccidental = "qsf"


instance AccClass DoubleSharp where
    sayAccidental = "ss"


-- acidente1 :: Accidental
-- acidente1 = "ff"

-- ex1 :: (String, String, String)
-- ex1 = (sayAccidental @Flat, sayAccidental @QuarterSharp, sayAccidental  @DoubleSharp)

-- >>> read @Accidental "f"

{- | Converts a string to an accidental
 >>>  "ff" ::  Accidental
 >>>  "n" ::  Accidental
 >>>  "quartersharp" ::  Accidental
 >>>  "semiflat" ::  Accidental
 DoubleFlat
 Natural
 QuarterSharp
 QuarterFlat
-}

{- |
 >>> Sharp + Sharp
 >>> Flat + Sharp
 DoubleSharp
 Natural
-}
instance Num Accidental where
    (+) a b = toAccidental $ accidentalToSemitones a + accidentalToSemitones b
    (-) a b = toAccidental $ accidentalToSemitones a - accidentalToSemitones b
    (*) a b = toAccidental $ accidentalToSemitones a * accidentalToSemitones b
    abs a = toAccidental $ abs (accidentalToSemitones a)
    signum a =
        case compare (accidentalToSemitones a) 0 of
            EQ -> Natural
            GT -> Sharp
            LT -> Flat
    fromInteger n = toAccidental (n % 1)
    negate (Custom r) = (Custom (-r))
    negate a = toAccidental $ negate (accidentalToSemitones a)


instance Enum Accidental where
    toEnum n = case n of
        0 -> DoubleFlat
        1 -> ThreeQuartersFlat
        2 -> Flat
        3 -> QuarterFlat
        4 -> Natural
        5 -> QuarterSharp
        6 -> Sharp
        7 -> ThreeQuartersSharp
        8 -> DoubleSharp
        _ -> error "toEnum: Out of bounds for Accidental"


    fromEnum acc = case acc of
        DoubleFlat -> 0
        ThreeQuartersFlat -> 1
        Flat -> 2
        QuarterFlat -> 3
        Natural -> 4
        QuarterSharp -> 5
        Sharp -> 6
        ThreeQuartersSharp -> 7
        DoubleSharp -> 8
        Custom _ -> error "fromEnum: Custom accidental doesn't have a fixed enumeration"


    succ :: Accidental -> Accidental
    succ acc
        | acc == maxBound = error "succ: tried to take succ of maxBound"
        | otherwise = sharpenQ acc


    pred :: Accidental -> Accidental
    pred acc
        | acc == minBound = error "pred: tried to take pred of minBound"
        | otherwise = flattenQ acc


instance Bounded Accidental where
    minBound = DoubleFlat
    maxBound = DoubleSharp


-- | Converts an accidental to its corresponding semitone offset as a rational number.
accidentalToSemitones :: Accidental -> Rational
accidentalToSemitones DoubleFlat = -2
accidentalToSemitones ThreeQuartersFlat = (-3) % 2
accidentalToSemitones Flat = -1
accidentalToSemitones QuarterFlat = (-1) % 2
accidentalToSemitones Natural = 0
accidentalToSemitones QuarterSharp = 1 % 2
accidentalToSemitones Sharp = 1
accidentalToSemitones ThreeQuartersSharp = 3 % 2
accidentalToSemitones DoubleSharp = 2
accidentalToSemitones (Custom r) = r


-- | Converts a semitone offset to the corresponding accidental.
semitonesToAccidental :: Rational -> Accidental
semitonesToAccidental r
    | r == (-2) % 1 = DoubleFlat
    | r == (-3) % 2 = ThreeQuartersFlat
    | r == (-1) % 1 = Flat
    | r == (-1) % 2 = QuarterFlat
    | r == 0 % 1 = Natural
    | r == 1 % 2 = QuarterSharp
    | r == 1 % 1 = Sharp
    | r == 3 % 2 = ThreeQuartersSharp
    | r == 2 % 1 = DoubleSharp
    | otherwise = (Custom r)


{- | Converts an accidental to its corresponding LilyPond representation.
 >>>  map accToLily allAccidentals == map  T.pack ["ff","tqf","f","qf","","qs","s","tqs","ss"]
 True
-}
accToLily :: Accidental -> T.Text
accToLily DoubleFlat = T.pack "ff"
accToLily ThreeQuartersFlat = T.pack "tqf"
accToLily Flat = T.pack "f"
accToLily QuarterFlat = T.pack "qf"
accToLily Natural = T.pack ""
accToLily QuarterSharp = T.pack "qs"
accToLily Sharp = T.pack "s"
accToLily ThreeQuartersSharp = T.pack "tqs"
accToLily DoubleSharp = T.pack "ss"
accToLily (Custom r) = T.pack $ show r


instance Read Accidental where
    readsPrec _ value =
        case value of
            "ff" -> [(DoubleFlat, "")]
            "tqf" -> [(ThreeQuartersFlat, "")]
            "f" -> [(Flat, "")]
            "qf" -> [(QuarterFlat, "")]
            "" -> [(Natural, "")]
            "n" -> [(Natural, "")]
            "qs" -> [(QuarterSharp, "")]
            "s" -> [(Sharp, "")]
            "tqs" -> [(ThreeQuartersSharp, "")]
            "ss" -> [(DoubleSharp, "")]
            "sharp" -> [(Sharp, "")]
            "flat" -> [(Flat, "")]
            "natural" -> [(Natural, "")]
            "quartersharp" -> [(QuarterSharp, "")]
            "semisharp" -> [(QuarterSharp, "")]
            "quarterflat" -> [(QuarterFlat, "")]
            "semiflat" -> [(QuarterFlat, "")]
            _ -> error $ "Invalid Accidental string: " <> value


-- >>> map (fromString @Accidental) ["ff","tqf","f","qf","","qs","s","tqs","ss"]
-- [DoubleFlat,ThreeQuartersFlat,Flat,QuarterFlat,Natural,QuarterSharp,Sharp,ThreeQuartersSharp,DoubleSharp]

instance IsString Accidental where
    fromString "ff" = DoubleFlat
    fromString "tqf" = ThreeQuartersFlat
    fromString "f" = Flat
    fromString "qf" = QuarterFlat
    fromString "" = Natural
    fromString "n" = Natural
    fromString "qs" = QuarterSharp
    fromString "s" = Sharp
    fromString "tqs" = ThreeQuartersSharp
    fromString "ss" = DoubleSharp
    fromString "sharp" = Sharp
    fromString "flat" = Flat
    fromString "natural" = Natural
    fromString "quartersharp" = QuarterSharp
    fromString "semisharp" = QuarterSharp
    fromString "quarterflat" = QuarterFlat
    fromString "semiflat" = QuarterFlat
    fromString "♭" = Flat
    fromString "♯" = Sharp
    fromString "♮" = Natural
    fromString "𝄫" = DoubleFlat
    fromString "𝄪" = DoubleSharp
    fromString "𝄳" = QuarterFlat
    fromString "𝄲" = QuarterSharp
    fromString str
        | "custom " `isPrefixOf` str = Custom (read (drop 7 str) :: Rational)
        | otherwise = error $ "Invalid Accidental string: " <> str


{- | Modify the accidental by applying a function to its semitone value. Returns the modified accidental.
 >>> modifyAccidental Sharp (*2)
 DoubleSharp
-}
modifyAccidental :: Accidental -> (Rational -> Rational) -> Accidental
modifyAccidental acc f =
    let newSemitone = f (accidentalToSemitones acc)
     in case newSemitone of
            r
                | r == -2 -> DoubleFlat
                | r == -3 % 2 -> ThreeQuartersFlat
                | r == -1 -> Flat
                | r == -1 % 2 -> QuarterFlat
                | r == 0 -> Natural
                | r == 1 % 2 -> QuarterSharp
                | r == 1 -> Sharp
                | r == 3 % 2 -> ThreeQuartersSharp
                | r == 2 -> DoubleSharp
                | otherwise -> (Custom r)


-- | Checks whether an accidental is equal to its corresponding semitone offset.
checkAccidental :: Accidental -> Bool
checkAccidental acc = semitonesToAccidental (accidentalToSemitones acc) == acc


{- | Modifies an accidental by a given delta (Rational).

 >>> addAccidental DoubleFlat (1 % 2)
 >>> addAccidental DoubleSharp (1 % 2)
 ThreeQuartersFlat
 Custom (5 % 2)
-}
addAccidental :: Accidental -> Rational -> Accidental
addAccidental acc delta
    | newSemitone == -2 = DoubleFlat
    | newSemitone == (-3) % 2 = ThreeQuartersFlat
    | newSemitone == -1 = Flat
    | newSemitone == (-1) % 2 = QuarterFlat
    | newSemitone == 0 = Natural
    | newSemitone == 1 % 2 = QuarterSharp
    | newSemitone == 1 = Sharp
    | newSemitone == 3 % 2 = ThreeQuartersSharp
    | newSemitone == 2 = DoubleSharp
    | otherwise = (Custom newSemitone)
    where
        currentSemitone = accidentalToSemitones acc
        newSemitone = currentSemitone + delta


invertAccidental :: Accidental -> Accidental
invertAccidental = negate


allAccidentals :: [Accidental]
allAccidentals =
    [ DoubleFlat,
      ThreeQuartersFlat,
      Flat,
      QuarterFlat,
      Natural,
      QuarterSharp,
      Sharp,
      ThreeQuartersSharp,
      DoubleSharp
    ]


allSemitones :: [Rational]
allSemitones = fmap accidentalToSemitones allAccidentals

{- -----------------------------------------------------------------------------

shuffled1 :: [Accidental]
shuffled1 = [QuarterFlat, DoubleSharp, ThreeQuartersSharp, Sharp, Natural, QuarterSharp, DoubleFlat, Flat, ThreeQuartersFlat]

shuffled2 :: [Accidental]
shuffled2 = [Natural, ThreeQuartersSharp, Flat, Sharp, DoubleSharp, DoubleFlat, QuarterFlat, ThreeQuartersFlat, QuarterSharp]

pc1 = PitchClass C Flat

pc1 ^. noteName
pc1 ^. accidental

succ pc1

--------------------------------------------------------------------------------------- -}
