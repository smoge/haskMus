{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Time.SimpleDuration where

import Control.Lens
import Data.Data
import Data.List (sortOn)
import Data.Ord (comparing)
import Data.Ratio
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Parsec
import Text.Parsec.String
import Util.MathDuration (isPowerOfTwo')

-- | Represents a musical division.
newtype Division = Division {unDivision :: Integer} deriving (Eq, Show, Ord, Data, Lift)

-- | Represents the number of dots in a musical notation, affecting the duration.
newtype Dots = Dots {unDots :: Integer} deriving (Eq, Show, Enum, Ord, Data, Lift)

-- | Represents a duration with division, dots, and multiplier.
data SimpleDuration = SimpleDuration
  { -- | The division value of the duration.
    _division :: Division,
    -- | The dots value of the duration.
    _dots :: Dots
  }
  deriving (Eq, Show, Data, Lift)

makeLenses ''SimpleDuration

mkDivision :: Integer -> Maybe Division
mkDivision x = if x > 0 && isPowerOfTwo' x then Just (Division x) else Nothing

mkDots :: Integer -> Maybe Dots
mkDots x = if x >= 0 then Just (Dots x) else Nothing

mkSimpleDuration :: Integer -> Integer -> Maybe SimpleDuration
mkSimpleDuration divValue dotsValue =
  SimpleDuration <$> mkDivision divValue <*> mkDots dotsValue

divisionParser :: Parser Integer
divisionParser = read <$> many1 digit

dotsParser :: Parser Integer
dotsParser = fromIntegral . length <$> many (char '.')

lilyPondDurationParser :: Parser SimpleDuration
lilyPondDurationParser = do
  divValue <- divisionParser
  dotsValue <- dotsParser
  case mkSimpleDuration divValue dotsValue of
    Just duration -> pure duration
    Nothing -> parserFail "Invalid duration format"

parseLilyPondDuration :: String -> Either ParseError SimpleDuration
parseLilyPondDuration = parse lilyPondDurationParser ""

example1 :: Either ParseError SimpleDuration
example1 = parseLilyPondDuration "4"

example2 :: Either ParseError SimpleDuration
example2 = parseLilyPondDuration "4."

example3 :: Either ParseError SimpleDuration
example3 = parseLilyPondDuration "8.."

example4 :: Either ParseError SimpleDuration
example4 = parseLilyPondDuration "5.."
-- Invalid

example5 :: Either ParseError SimpleDuration
example5 = parseLilyPondDuration "16"
