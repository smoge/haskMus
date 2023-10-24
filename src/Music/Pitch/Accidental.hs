{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE UnicodeSyntax             #-}

module Music.Pitch.Accidental
  ( Accidental(..)
  , accidentalToSemitones
  , semitonesToAccidental
  , modifyAccidental
  , accToLily
  , checkAccidental
  , addAccidental
  , allAccidentals
  , allSemitones
  , AccidentalString(..)
  ) where

import           Data.Kind
import           Data.List       (isPrefixOf)
import Data.Proxy
import qualified Data.Map.Strict as Map
import           Data.Ratio
import           Data.String
import qualified Data.Text       as T
import           GHC.TypeLits
import           Test.QuickCheck
import           Text.Printf

-- // SECTION   ACCIDENTAL
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
  deriving (Eq, Ord, Show)



class IsAccidental a where
  toAccidental :: a -> Accidental

data SomeAccidental =
  forall accidental. IsAccidental accidental =>
                     SomeAccidental accidental

instance IsAccidental SomeAccidental where
  toAccidental :: SomeAccidental -> Accidental
  toAccidental (SomeAccidental acc) = toAccidental acc

newtype AccidentalSelection selection = AccidentalSelection
  { getAccidentalSelection :: Map.Map String SomeAccidental
  }

class AccClass (notename :: Accidental) where
  sayAccidental :: String
  unicodeAcc :: String


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
  sayAccidental = "n"
  unicodeAcc = ""

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

acidente1 :: Accidental
acidente1 = "ff"

-- ex1 :: (String, String, String)
-- ex1 = (sayAccidental @Flat, sayAccidental @QuarterSharp, sayAccidental  @DoubleSharp)
type AccSelection = [String]

-- >>> read @Accidental "f"
-- | Converts a string to an accidental
-- >>>  "ff" ::  Accidental
-- >>>  "n" ::  Accidental
-- >>>  "quartersharp" ::  Accidental
-- >>>  "semiflat" ::  Accidental
-- DoubleFlat
-- Natural
-- QuarterSharp
-- QuarterFlat
-- instance Num Accidental where
--   (+) a b = toAccidental $ accidentalToSemitones a + accidentalToSemitones b
--   (-) a b = toAccidental $ accidentalToSemitones a - accidentalToSemitones b
--   (*) a b = toAccidental $ accidentalToSemitones a * accidentalToSemitones b
--   abs a = toAccidental $ abs (accidentalToSemitones a)
--   signum a =
--     case compare (accidentalToSemitones a) 0 of
--       EQ -> Natural
--       GT -> Sharp
--       LT -> Flat
--   fromInteger n = toAccidental (fromInteger n)
--   negate (Custom r) = (Custom (-r))
--   negate a = toAccidental $ negate (accidentalToSemitones a)
instance Enum Accidental where
  toEnum n =
    case n of
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
  fromEnum acc =
    case acc of
      DoubleFlat -> 0
      ThreeQuartersFlat -> 1
      Flat -> 2
      QuarterFlat -> 3
      Natural -> 4
      QuarterSharp -> 5
      Sharp -> 6
      ThreeQuartersSharp -> 7
      DoubleSharp -> 8
      Custom _ ->
        error "fromEnum: Custom accidental doesn't have a fixed enumeration"

instance Bounded Accidental where
  minBound = DoubleFlat
  maxBound = DoubleSharp

-- //ANCHOR toAccidental
-- toAccidental :: Rational -> Accidental
-- toAccidental r
--   | r == (-2) = DoubleFlat
--   | r == (-3) % 2 = ThreeQuartersFlat
--   | r == (-1) = Flat
--   | r == (-1) % 2 = QuarterFlat
--   | r == 0 = Natural
--   | r == 1 % 2 = QuarterSharp
--   | r == 1 = Sharp
--   | r == 3 % 2 = ThreeQuartersSharp
--   | r == 2 = DoubleSharp
--   | otherwise = (Custom r)
-- //ANCHOR accidentalToSemitones
-- | Converts an accidental to its corresponding semitone offset as a rational number.
accidentalToSemitones :: Accidental -> Rational
accidentalToSemitones DoubleFlat         = -2
accidentalToSemitones ThreeQuartersFlat  = (-3) % 2
accidentalToSemitones Flat               = -1
accidentalToSemitones QuarterFlat        = (-1) % 2
accidentalToSemitones Natural            = 0
accidentalToSemitones QuarterSharp       = 1 % 2
accidentalToSemitones Sharp              = 1
accidentalToSemitones ThreeQuartersSharp = 3 % 2
accidentalToSemitones DoubleSharp        = 2
accidentalToSemitones (Custom r)         = r

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

-- error "Invalid semitone offset."
-- | Converts an accidental to its corresponding LilyPond representation.
-- >>>  map accToLily allAccidentals == map  T.pack ["ff","tqf","f","qf","","qs","s","tqs","ss"]
-- True
accToLily :: Accidental -> T.Text
accToLily DoubleFlat         = T.pack "ff"
accToLily ThreeQuartersFlat  = T.pack "tqf"
accToLily Flat               = T.pack "f"
accToLily QuarterFlat        = T.pack "qf"
accToLily Natural            = T.pack ""
accToLily QuarterSharp       = T.pack "qs"
accToLily Sharp              = T.pack "s"
accToLily ThreeQuartersSharp = T.pack "tqs"
accToLily DoubleSharp        = T.pack "ss"
accToLily (Custom r)         = T.pack $ show r

-- //ANCHOR OverloadedStrings
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
    | otherwise = error $ "Invalid Accidental string: " ++ str

{-
>>> accStr1 :: Accidental
>>> accStr1 = "♭"
>>> accStr2 :: Accidental
>>> accStr2 = "♯"
>>> accStr3 :: Accidental
>>> accStr3 = "♮"
>>> accStr4 :: Accidental
>>> accStr4 = "𝄫"
>>> accStr5 :: Accidental
>>> accStr5 = "𝄪"
>>> accStr6 :: Accidental
>>> accStr6 = "𝄳"
>>> accStr7 :: Accidental
>>> accStr7 = "𝄲"

>>> accStr1 == Flat
True
>>> accStr2 == Sharp
True
>>> accStr3 == Natural
True
>>> accStr4 == DoubleFlat
True
>>> accStr5 == DoubleSharp
True
>>> accStr6 == QuarterFlat
True
>>> accStr7 == QuarterSharp
True
-}
-- "𝄱" :: Accidental
-- "𝄰" :: Accidental
-- "𝄭" :: Accidental
-- "𝄬" :: Accidental
-- //ANCHOR - modifyAccidental
-- | Modify the accidental by applying a function to its semitone value. Returns the modified accidental.
-- >>> modifyAccidental Sharp (*2) == DoubleSharp
-- True
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

-- | Modifies an accidental by a given delta (Rational).
--
-- >>> addAccidental DoubleFlat (1 % 2) == ThreeQuartersFlat
-- >>> addAccidental Natural 1 == Sharp
-- >>> addAccidental Sharp (-1 % 2) == QuarterSharp
-- >>> addAccidental QuarterSharp (-1) == QuarterFlat
-- >>> addAccidental DoubleSharp (-4) == DoubleFlat
-- True
-- True
-- True
-- True
-- True
-- //ANCHOR - addAccidental
-- >>> addAccidental DoubleSharp (1 % 2)
-- Custom (5 % 2)
addAccidental ::
     Accidental
  -- | The delta to modify the accidental
  -> Rational
  -- | The modified accidental
  -> Accidental
  -- | The initial accidental
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

-- invertAccidental'' :: Accidental -> Accidental
-- invertAccidental'' = negate
allAccidentals :: [Accidental]
allAccidentals =
  [ DoubleFlat
  , ThreeQuartersFlat
  , Flat
  , QuarterFlat
  , Natural
  , QuarterSharp
  , Sharp
  , ThreeQuartersSharp
  , DoubleSharp
  ]

allSemitones :: [Rational]
allSemitones = map accidentalToSemitones allAccidentals

-- //ANCHOR  - QUICKCHECK
instance Arbitrary Accidental where
  arbitrary =
    frequency
      [ (10, elements allAccidentals) -- Picking from the predefined list
      , (1, Custom <$> arbitrary) -- Picking a custom accidental
      ]

-- Newtype wrapper for specific accidental strings
newtype AccidentalString =
  AccidentalString String
  deriving (Show)

-- Arbitrary instance for AccidentalString (QuickCheck)
instance Arbitrary AccidentalString where
  arbitrary =
    AccidentalString
      <$> elements
            [ "ff"
            , "tqf"
            , "f"
            , "qf"
            , ""
            , "n"
            , "qs"
            , "s"
            , "tqs"
            , "ss"
            , "sharp"
            , "flat"
            , "natural"
            , "quartersharp"
            , "semisharp"
            , "quarterflat"
            , "semiflat"
            , "♭"
            , "♯"
            , "♮"
            , "𝄫"
            , "𝄪"
            , "𝄳"
            , "𝄲"
            ]
