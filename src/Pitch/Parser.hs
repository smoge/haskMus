{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Pitch.Parser where

import Pitch.Accidental
import Pitch.Pitch
import Text.Parsec
import qualified Text.Parsec as P

import Text.Parsec.String

-- Consume spaces before and after the parser.
{-# INLINE spaced #-}
spaced :: Parser a -> Parser a
spaced p = spaces *> p <* spaces

pitchClassParser :: Parser PitchClass
pitchClassParser =
  try (string "cqs" >> pure (PitchClass C QuarterSharp))
    <|> try (string "cqf" >> pure (PitchClass C QuarterFlat))
    <|> try (string "dqf" >> pure (PitchClass D QuarterFlat))
    <|> try (string "dqs" >> pure (PitchClass D QuarterSharp))
    <|> try (string "eqf" >> pure (PitchClass E QuarterFlat))
    <|> try (string "eqs" >> pure (PitchClass E QuarterSharp))
    <|> try (string "fqs" >> pure (PitchClass F QuarterSharp))
    <|> try (string "fqf" >> pure (PitchClass F QuarterFlat))
    <|> try (string "gqf" >> pure (PitchClass G QuarterFlat))
    <|> try (string "gqs" >> pure (PitchClass G QuarterSharp))
    <|> try (string "aqf" >> pure (PitchClass A QuarterFlat))
    <|> try (string "aqs" >> pure (PitchClass A QuarterSharp))
    <|> try (string "bqf" >> pure (PitchClass B QuarterFlat))
    <|> try (string "bqs" >> pure (PitchClass B QuarterSharp))
    <|> try (string "cf" >> pure (PitchClass C Flat))
    <|> try (string "df" >> pure (PitchClass D Flat))
    <|> try (string "ef" >> pure (PitchClass E Flat))
    <|> try (string "gf" >> pure (PitchClass G Flat))
    <|> try (string "af" >> pure (PitchClass A Flat))
    <|> try (string "bf" >> pure (PitchClass B Flat))
    <|> try (string "cs" >> pure (PitchClass C Sharp))
    <|> try (string "ds" >> pure (PitchClass D Sharp))
    <|> try (string "es" >> pure (PitchClass E Sharp))
    <|> try (string "fs" >> pure (PitchClass F Sharp))
    <|> try (string "gs" >> pure (PitchClass G Sharp))
    <|> try (string "as" >> pure (PitchClass A Sharp))
    <|> try (string "bs" >> pure (PitchClass B Sharp))
    <|> try (string "c" >> pure (PitchClass C Natural))
    <|> try (string "d" >> pure (PitchClass D Natural))
    <|> try (string "e" >> pure (PitchClass E Natural))
    <|> try (string "f" >> pure (PitchClass F Natural))
    <|> try (string "g" >> pure (PitchClass G Natural))
    <|> try (string "a" >> pure (PitchClass A Natural))
    <|> try (string "b" >> pure (PitchClass B Natural))

pitchClassParser' :: Parser PitchClass
pitchClassParser' =
  P.choice
    [ P.string "cqs" *> pure (PitchClass C QuarterSharp),
      P.string "cqf" *> pure (PitchClass C QuarterFlat),
      P.string "dqf" *> pure (PitchClass D QuarterFlat),
      P.string "dqs" *> pure (PitchClass D QuarterSharp),
      P.string "eqf" *> pure (PitchClass E QuarterFlat),
      P.string "eqs" *> pure (PitchClass E QuarterSharp),
      P.string "fqs" *> pure (PitchClass F QuarterSharp),
      P.string "fqf" *> pure (PitchClass F QuarterFlat),
      P.string "gqf" *> pure (PitchClass G QuarterFlat),
      P.string "gqs" *> pure (PitchClass G QuarterSharp),
      P.string "aqf" *> pure (PitchClass A QuarterFlat),
      P.string "aqs" *> pure (PitchClass A QuarterSharp),
      P.string "bqf" *> pure (PitchClass B QuarterFlat),
      P.string "bqs" *> pure (PitchClass B QuarterSharp),
      P.string "cf" *> pure (PitchClass C Flat),
      P.string "df" *> pure (PitchClass D Flat),
      P.string "ef" *> pure (PitchClass E Flat),
      P.string "ff" *> pure (PitchClass F Flat),
      P.string "gf" *> pure (PitchClass G Flat),
      P.string "af" *> pure (PitchClass A Flat),
      P.string "bf" *> pure (PitchClass B Flat),
      P.string "cs" *> pure (PitchClass C Sharp),
      P.string "ds" *> pure (PitchClass D Sharp),
      P.string "es" *> pure (PitchClass E Sharp),
      P.string "fs" *> pure (PitchClass F Sharp),
      P.string "gs" *> pure (PitchClass G Sharp),
      P.string "as" *> pure (PitchClass A Sharp),
      P.string "bs" *> pure (PitchClass B Sharp),
      P.string "c" *> pure (PitchClass C Natural),
      P.string "d" *> pure (PitchClass D Natural),
      P.string "e" *> pure (PitchClass E Natural),
      P.string "f" *> pure (PitchClass F Natural),
      P.string "g" *> pure (PitchClass G Natural),
      P.string "a" *> pure (PitchClass A Natural),
      P.string "b" *> pure (PitchClass B Natural)
    ]

octaveParser :: Parser Octave
octaveParser = do
  upOctaves <- many (char '\'')
  downOctaves <- many (char ',')
  let octs = length upOctaves - length downOctaves
  pure (Octave (octs + 4))

-- | Parse a string into a list of pitches.
-- >>> parsePitches "cqs' cqf,  gqs''"
-- Right [C QuarterSharp Octave 5,C QuarterFlat Octave 3,G QuarterSharp Octave 6]
parsePitches :: String -> Either ParseError [Pitch]
parsePitches = parse pitchesParser ""

pitchParser :: Parser Pitch
pitchParser = do
  pc <- pitchClassParser
  mkPitch' pc <$> octaveParser

pitchesParser :: Parser [Pitch]
pitchesParser = spaced $ sepEndBy pitchParser spaces

-- octaveParser :: Parser Octave
-- octaveParser = Octave . (+4) <$> (length <$> many (char '\'') <*> (negate . length <$> many (char ',')))
