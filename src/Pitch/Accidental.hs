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
    accidental_XML_alter,
    accidental_XML_accidental_value,
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
 [Custom ((-5) % 2),DoubleFlat,ThreeQuartersFlat,Flat,QuarterFlat,Natural,QuarterSharp,Sharp,ThreeQuartersSharp]l
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

--XML--------------------------------------------------------------------------

accidental_XML_alter :: Accidental -> T.Text
accidental_XML_alter DoubleFlat = T.pack "-2"
accidental_XML_alter ThreeQuartersFlat = T.pack "-1.5"
accidental_XML_alter Flat = T.pack "-1"
accidental_XML_alter QuarterFlat = T.pack "-0.5"
accidental_XML_alter Natural = T.pack "0"
accidental_XML_alter QuarterSharp = T.pack "0.5"
accidental_XML_alter Sharp = T.pack "1"
accidental_XML_alter ThreeQuartersSharp = T.pack "1.5"
accidental_XML_alter DoubleSharp = T.pack "2"
accidental_XML_alter (Custom _) = T.pack "0"

accidental_XML_accidental_value :: Accidental -> T.Text
accidental_XML_accidental_value DoubleFlat = T.pack "sharp-sharp"
accidental_XML_accidental_value ThreeQuartersFlat = T.pack "three-quarters-flat"
accidental_XML_accidental_value Flat = T.pack "flat"
accidental_XML_accidental_value QuarterFlat = T.pack "quarter-flat"
accidental_XML_accidental_value Natural = T.pack "natural"
accidental_XML_accidental_value QuarterSharp = T.pack "quarter-sharp"
accidental_XML_accidental_value Sharp = T.pack "sharp"
accidental_XML_accidental_value ThreeQuartersSharp = T.pack "three-quarters-sharp"
accidental_XML_accidental_value DoubleSharp = T.pack "sharp-sharp"
accidental_XML_accidental_value (Custom _) = T.pack "other"

-- <pitch>
--    <step>B</step>
--    <alter>-0.5</alter>
--    <octave>4</octave>
-- </pitch>

-- instance ToXML Pitch where
--   toXML (Pitch pn acc o) = element "pitch" $ do
--     element "step" $ content (T.pack $ show pn)
--     element "alter" $ content (T.pack $ accidentalXMLString acc)
--     element "octave" $ content (T.pack $ show o)

-- See: https://github.com/w3c/musicxml/issues/263
-- <accidental smufl="accidentalFlatThreeArrowsUp">other</accidental>

-- https://www.w3.org/2021/06/musicxml40/musicxml-reference/data-types/accidental-value/

-- musicxmlxsd line 1521

-- <xs:simpleType name="accidental-value">
-- 	<xs:annotation>
-- 		<xs:documentation>The accidental-value type represents notated accidentals supported by MusicXML. In the MusicXML 2.0 DTD this was a string with values that could be included. The XSD strengthens the data typing to an enumerated list. The quarter- and three-quarters- accidentals are Tartini-style quarter-tone accidentals. The -down and -up accidentals are quarter-tone accidentals that include arrows pointing down or up. The slash- accidentals are used in Turkish classical music. The numbered sharp and flat accidentals are superscripted versions of the accidental signs, used in Turkish folk music. The sori and koron accidentals are microtonal sharp and flat accidentals used in Iranian and Persian music. The other accidental covers accidentals other than those listed here. It is usually used in combination with the smufl attribute to specify a particular SMuFL accidental. The smufl attribute may be used with any accidental value to help specify the appearance of symbols that share the same MusicXML semantics.</xs:documentation>
-- 	</xs:annotation>
-- 	<xs:restriction base="xs:string">
-- 		<xs:enumeration value="sharp"/>
-- 		<xs:enumeration value="natural"/>
-- 		<xs:enumeration value="flat"/>
-- 		<xs:enumeration value="double-sharp"/>
-- 		<xs:enumeration value="sharp-sharp"/>
-- 		<xs:enumeration value="flat-flat"/>
-- 		<xs:enumeration value="natural-sharp"/>
-- 		<xs:enumeration value="natural-flat"/>
-- 		<xs:enumeration value="quarter-flat"/>
-- 		<xs:enumeration value="quarter-sharp"/>
-- 		<xs:enumeration value="three-quarters-flat"/>
-- 		<xs:enumeration value="three-quarters-sharp"/>
-- 		<xs:enumeration value="sharp-down"/>
-- 		<xs:enumeration value="sharp-up"/>
-- 		<xs:enumeration value="natural-down"/>
-- 		<xs:enumeration value="natural-up"/>
-- 		<xs:enumeration value="flat-down"/>
-- 		<xs:enumeration value="flat-up"/>
-- 		<xs:enumeration value="double-sharp-down"/>
-- 		<xs:enumeration value="double-sharp-up"/>
-- 		<xs:enumeration value="flat-flat-down"/>
-- 		<xs:enumeration value="flat-flat-up"/>
-- 		<xs:enumeration value="arrow-down"/>
-- 		<xs:enumeration value="arrow-up"/>
-- 		<xs:enumeration value="triple-sharp"/>
-- 		<xs:enumeration value="triple-flat"/>
-- 		<xs:enumeration value="slash-quarter-sharp"/>
-- 		<xs:enumeration value="slash-sharp"/>
-- 		<xs:enumeration value="slash-flat"/>
-- 		<xs:enumeration value="double-slash-flat"/>
-- 		<xs:enumeration value="sharp-1"/>
-- 		<xs:enumeration value="sharp-2"/>
-- 		<xs:enumeration value="sharp-3"/>
-- 		<xs:enumeration value="sharp-5"/>
-- 		<xs:enumeration value="flat-1"/>
-- 		<xs:enumeration value="flat-2"/>
-- 		<xs:enumeration value="flat-3"/>
-- 		<xs:enumeration value="flat-4"/>
-- 		<xs:enumeration value="sori"/>
-- 		<xs:enumeration value="koron"/>
-- 		<xs:enumeration value="other"/>
-- 	</xs:restriction>
-- </xs:simpleType>

-- e--------------------------------------------------------------

-- | ConrtsStringe offset to the corresponding accidental.
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
