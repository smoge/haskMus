{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Pitch.Parser (
  parsePitches,
) where

-- import Control.Lens
-- import Language.Haskell.TH
-- import Language.Haskell.TH.Quote
-- import Language.Haskell.TH.Syntax
import Pitch.Accidental (
  Accidental (Flat, Natural, QuarterFlat, QuarterSharp, Sharp),
 )
import Pitch.Pitch
import Text.Parsec
import Text.Parsec.String

-- Consume spaces before and after the parser.
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

{-

import qualified Data.Map as Map

pitchClassMap :: Map String PitchClass
pitchClassMap = Map.fromList
    [ ("cqs", PitchClass C QuarterSharp)
    , ("cqf", PitchClass C QuarterFlat)
    , ("dqf", PitchClass D QuarterFlat)
    , ("dqs", PitchClass D QuarterSharp)
    , ("eqf", PitchClass E QuarterFlat)
    , ("eqs", PitchClass E QuarterSharp)
    , ("fqs", PitchClass F QuarterSharp)
    , ("fqf", PitchClass F QuarterFlat)
    , ("gqf", PitchClass G QuarterFlat)
    , ("gqs", PitchClass G QuarterSharp)
    , ("aqf", PitchClass A QuarterFlat)
    , ("aqs", PitchClass A QuarterSharp)
    , ("bqf", PitchClass B QuarterFlat)
    , ("bqs", PitchClass B QuarterSharp)
    , ("cf", PitchClass C Flat)
    , ("df", PitchClass D Flat)
    , ("ef", PitchClass E Flat)
    , ("gf", PitchClass G Flat)
    , ("af", PitchClass A Flat)
    , ("bf", PitchClass B Flat)
    , ("cs", PitchClass C Sharp)
    , ("ds", PitchClass D Sharp)
    , ("es", PitchClass E Sharp)
    , ("fs", PitchClass F Sharp)
    , ("gs", PitchClass G Sharp)
    , ("as", PitchClass A Sharp)
    , ("bs", PitchClass B Sharp)
    , ("c", PitchClass C Natural)
    , ("d", PitchClass D Natural)
    , ("e", PitchClass E Natural)
    , ("f", PitchClass F Natural)
    , ("g", PitchClass G Natural)
    , ("a", PitchClass A Natural)
    , ("b", PitchClass B Natural)
    ]

pitchClassParser :: Parser PitchClass
pitchClassParser = choice
    [ string key >> pure value
    | (key, value) <- Map.toList pitchClassMap
    ]

 -}
octaveParser :: Parser Octave
octaveParser = do
  upOctaves <- many (char '\'')
  downOctaves <- many (char ',')
  let octs = length upOctaves - length downOctaves
  pure (Octave (octs + 4))

-- octaveParser :: Parser Octave
-- octaveParser = Octave . (+4) <$> (length <$> many (char '\'') <*> (negate . length <$> many (char ',')))

pitchParser :: Parser Pitch
pitchParser = spaced $ do
  pc <- pitchClassParser
  mkPitch' pc <$> octaveParser

-- parsePitch :: String -> Either ParseError Pitch
-- parsePitch = parse pitchParser ""

pitchesParser :: Parser [Pitch]
pitchesParser = spaced $ sepEndBy pitchParser spaces

parsePitches :: String -> Either ParseError [Pitch]
parsePitches = parse pitchesParser ""
