{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitch.Parser (
  pitchClassParser,
  parsePitches,
  pitchParser,
  pitchesParser,
  mkPitch'',
  octaveParser  

) where

import Data.Functor.Identity (Identity)
import Data.Text qualified as T
import Data.Void (Void)
import Pitch.Accidental
    ( Accidental(Natural, QuarterFlat, QuarterSharp, Flat, Sharp) )
import Pitch.Pitch ( Octave(Octave), Pitch(Pitch) )
import Pitch.PitchClass ( PitchClass(PitchClass), NoteName(..) )

import Text.Megaparsec
    ( empty,
      runParser,
      choice,
      many,
      sepEndBy,
      Parsec,
      MonadParsec(try),
      ParseErrorBundle,
      ParsecT )
import Text.Megaparsec.Char ( char, space1, string )
import Text.Megaparsec.Char.Lexer qualified as L

-- {-# INLINE spaced #-}
-- spaced :: Parsec Void T.Text a -> Parsec Void T.Text a
-- spaced p = spaceConsumer *> p <* spaceConsumer

spaceConsumer :: ParsecT Void T.Text Identity ()
spaceConsumer = L.space space1 empty empty

pitchClassParser :: Parsec Void T.Text PitchClass
pitchClassParser = choice . fmap (uncurry3 parsePitchClass) $ pitchClasses
  where
    uncurry3 f (a, b, c) = f a b c
    parsePitchClass str n a = try $ string str >> pure (PitchClass n a)

pitchClasses :: [(T.Text, NoteName, Accidental)]
pitchClasses =
  [ ("cqs", C, QuarterSharp),
    ("cqf", C, QuarterFlat),
    ("dqf", D, QuarterFlat),
    ("dqs", D, QuarterSharp),
    ("eqf", E, QuarterFlat),
    ("eqs", E, QuarterSharp),
    ("fqs", F, QuarterSharp),
    ("fqf", F, QuarterFlat),
    ("gqf", G, QuarterFlat),
    ("gqs", G, QuarterSharp),
    ("aqf", A, QuarterFlat),
    ("aqs", A, QuarterSharp),
    ("bqf", B, QuarterFlat),
    ("bqs", B, QuarterSharp),
    ("cf", C, Flat),
    ("df", D, Flat),
    ("ef", E, Flat),
    ("gf", G, Flat),
    ("af", A, Flat),
    ("bf", B, Flat),
    ("cs", C, Sharp),
    ("ds", D, Sharp),
    ("es", E, Sharp),
    ("fs", F, Sharp),
    ("gs", G, Sharp),
    ("as", A, Sharp),
    ("bs", B, Sharp),
    ("c", C, Natural),
    ("d", D, Natural),
    ("e", E, Natural),
    ("f", F, Natural),
    ("g", G, Natural),
    ("a", A, Natural),
    ("b", B, Natural)
  ]

octaveParser :: Parsec Void T.Text Octave
octaveParser = do
  upOctaves <- length <$> many (char '\'')
  downOctaves <- length <$> many (char ',')
  let octs = upOctaves - downOctaves
  pure (Octave (octs + 4))

parsePitches :: T.Text -> Either (ParseErrorBundle T.Text Void) [Pitch]
parsePitches = runParser pitchesParser ""

{-# INLINE mkPitch'' #-}
mkPitch'' :: PitchClass -> Octave -> Pitch
mkPitch'' (PitchClass n a) = Pitch n a

pitchParser :: ParsecT Void T.Text Identity Pitch
pitchParser = do
  pc <- pitchClassParser
  mkPitch'' pc <$> octaveParser

pitchesParser :: ParsecT Void T.Text Identity [Pitch]
pitchesParser = sepEndBy pitchParser spaceConsumer
