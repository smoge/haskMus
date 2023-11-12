{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}


module Pitch.Parser where

import Language.Haskell.TH.Quote (QuasiQuoter(..))


import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Pitch.Accidental
import Pitch.Pitch
import Text.Parsec
import Text.Parsec.String
import Control.Lens


-- Consume spaces before and after the parser.
spaced :: Parser a -> Parser a
spaced p = spaces *> p <* spaces


pitchClassParser :: Parser PitchClass
pitchClassParser =
  try (string "cqs" >> pure (PitchClass C QuarterSharp))
    <|> try (string "dqf" >> pure (PitchClass D QuarterFlat))
    <|> try (string "dqs" >> pure (PitchClass D QuarterSharp))
    <|> try (string "eqf" >> pure (PitchClass E QuarterFlat))
    <|> try (string "eqs" >> pure (PitchClass E QuarterSharp))
    <|> try (string "fqs" >> pure (PitchClass F QuarterSharp))
    <|> try (string "gqf" >> pure (PitchClass G QuarterFlat))
    <|> try (string "gqs" >> pure (PitchClass G QuarterSharp))
    <|> try (string "aqf" >> pure (PitchClass A QuarterFlat))
    <|> try (string "aqs" >> pure (PitchClass A QuarterSharp))
    <|> try (string "bqf" >> pure (PitchClass B QuarterFlat))
    <|> try (string "bqs" >> pure (PitchClass B QuarterSharp))
    <|> try (string "df" >> pure (PitchClass D Flat))
    <|> try (string "ef" >> pure (PitchClass E Flat))
    <|> try (string "gf" >> pure (PitchClass G Flat))
    <|> try (string "af" >> pure (PitchClass A Flat))
    <|> try (string "cs" >> pure (PitchClass C Sharp))
    <|> try (string "ds" >> pure (PitchClass D Sharp))
    <|> try (string "fs" >> pure (PitchClass F Sharp))
    <|> try (string "gs" >> pure (PitchClass G Sharp))
    <|> try (string "as" >> pure (PitchClass A Sharp))
    <|> try (string "bf" >> pure (PitchClass B Flat))
    <|> try (string "c" >> pure (PitchClass C Natural))
    <|> try (string "d" >> pure (PitchClass D Natural))
    <|> try (string "e" >> pure (PitchClass E Natural))
    <|> try (string "f" >> pure (PitchClass F Natural))
    <|> try (string "g" >> pure (PitchClass G Natural))
    <|> try (string "a" >> pure (PitchClass A Natural))
    <|> try (string "b" >> pure (PitchClass B Natural))



octaveParser :: Parser Octave
octaveParser = do
  _ <- spaces
  upOctaves <- many (char '\'')
  downOctaves <- many (char ',')
  _ <- spaces
  let octs = length upOctaves - length downOctaves
  pure (Octave (octs + 4))



mkPitch' :: PitchClass -> Octave -> Pitch
mkPitch' pc o = Pitch (pc ^. noteName) (pc ^. accidental) o


-- Parser for pitches
pitchParser :: Parser Pitch
pitchParser = do
  _ <- spaces
  pc <- pitchClassParser
  _ <- spaces
  oct <- octaveParser
  _ <- spaces
  pure $ mkPitch' pc oct




-- Define the pitch QuasiQuoter
pitch :: QuasiQuoter
pitch = QuasiQuoter
  { quoteExp = \str -> case parse pitchParser "" str of
      Left err -> error (show err)
      Right pit -> [| pit |]
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }


-- pitch :: QuasiQuoter
-- pitch = QuasiQuoter
--   { quoteExp = \str -> case parse pitchParser "" str of
--       Left err -> error (show err)
--       Right pit -> [| pit |]
--   , quotePat = undefined
--   , quoteType = undefined
--   , quoteDec = undefined
--   }



-- pitch :: QuasiQuoter
-- pitch =
--   QuasiQuoter
--     { quoteExp = \s -> case parse pitchParser "" s of
--         Left err -> [| Left ("Parsing error: " <> show err) |]
--         Right pit -> [| Right $(lift pit) |],
--       quotePat = undefined,
--       quoteType = undefined,
--       quoteDec = undefined
--     }



-- main :: IO ()
-- main = do
--   let pitch1 = [pitch|c|]
--   -- Middle C
--   let pitch2 = [pitch|c'|]
--   -- C one octave above
--   let pitch3 = [pitch|c,|]
--   -- C one octave below
--   print pitch1
--   print pitch2
--   print pitch3
