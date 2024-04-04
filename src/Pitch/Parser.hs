{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Pitch.Parser where

import qualified Data.Text as T
import Pitch.Accidental
import Pitch.Pitch
import Text.Parsec
import Text.Parsec.Error (ParseError)
import Text.Parsec.Text

-- Consume spaces before and after the parser.
{-# INLINE spaced #-}
spaced :: Parser a -> Parser a
spaced p = spaces *> p <* spaces

pitchClassParser :: Parser PitchClass
pitchClassParser =
  choice $
    map
      parsePitchClass
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
  where
    parsePitchClass (str, pitch, accidental) = try (string' str >> pure (PitchClass pitch accidental))

octaveParser :: Parser Octave
octaveParser = do
  upOctaves <- length <$> many (char '\'')
  downOctaves <- length <$> many (char ',')
  let octs = upOctaves - downOctaves
  pure (Octave (octs + 4))

parsePitches :: T.Text -> Either ParseError [Pitch]
parsePitches input = parse pitchesParser "" input

{-# INLINE mkPitch'' #-}
mkPitch'' :: PitchClass -> Octave -> Pitch
mkPitch'' (PitchClass pitch accidental) o = Pitch {_noteName = pitch, _accidental = accidental, _octave = o}

pitchParser :: Parser Pitch
pitchParser = do
  pc <- pitchClassParser
  mkPitch'' pc <$> octaveParser

pitchesParser :: Parser [Pitch]
pitchesParser = sepEndBy pitchParser spaces

{-
User

benchmarking parsePitches/c d e f d
time                 17.69 μs   (17.17 μs .. 18.37 μs)
                     0.992 R²   (0.985 R² .. 0.997 R²)
mean                 17.92 μs   (17.58 μs .. 18.42 μs)
std dev              1.373 μs   (1.090 μs .. 1.820 μs)
variance introduced by outliers: 77% (severely inflated)

benchmarking parsePitches/cqs' cqf,  gqs''
time                 8.847 μs   (8.383 μs .. 9.340 μs)
                     0.972 R²   (0.954 R² .. 0.984 R²)
mean                 8.048 μs   (7.585 μs .. 8.471 μs)
std dev              1.503 μs   (1.226 μs .. 1.958 μs)
variance introduced by outliers: 96% (severely inflated)

benchmarking parsePitches/c d e f d
time                 23.47 μs   (22.57 μs .. 24.64 μs)
                     0.992 R²   (0.987 R² .. 0.997 R²)
mean                 23.24 μs   (22.73 μs .. 24.27 μs)
std dev              2.484 μs   (1.675 μs .. 4.347 μs)
variance introduced by outliers: 86% (severely inflated)

benchmarking parsePitches/cqs' cqf,  gqs''
time                 9.005 μs   (8.631 μs .. 9.265 μs)
                     0.990 R²   (0.985 R² .. 0.994 R²)
mean                 8.672 μs   (8.370 μs .. 8.931 μs)
std dev              885.2 ns   (780.2 ns .. 1.013 μs)
variance introduced by outliers: 87% (severely inflated)

benchmarking parsePitches/c d e f d
time                 21.76 μs   (20.22 μs .. 23.11 μs)
                     0.980 R²   (0.970 R² .. 0.995 R²)
mean                 21.11 μs   (20.54 μs .. 21.74 μs)
std dev              2.076 μs   (1.657 μs .. 2.747 μs)
variance introduced by outliers: 84% (severely inflated)

benchmarking parsePitches/cqs' cqf,  gqs''
time                 7.549 μs   (7.338 μs .. 7.786 μs)
                     0.992 R²   (0.986 R² .. 0.996 R²)
mean                 7.729 μs   (7.556 μs .. 7.924 μs)
std dev              665.6 ns   (544.0 ns .. 834.3 ns)
variance introduced by outliers: 83% (severely inflated)

benchmarking parsePitches/c d e f d
time                 20.25 μs   (19.80 μs .. 20.73 μs)
                     0.994 R²   (0.990 R² .. 0.997 R²)
mean                 20.73 μs   (20.04 μs .. 21.89 μs)
std dev              2.855 μs   (1.501 μs .. 4.454 μs)
variance introduced by outliers: 91% (severely inflated)

benchmarking parsePitches/cqs' cqf,  gqs''
time                 7.265 μs   (7.021 μs .. 7.520 μs)
                     0.990 R²   (0.985 R² .. 0.995 R²)
mean                 7.658 μs   (7.392 μs .. 7.930 μs)
std dev              903.3 ns   (735.6 ns .. 1.260 μs)
variance introduced by outliers: 90% (severely inflated)

benchmarking parsePitches/c d e f d
time                 19.31 μs   (19.00 μs .. 19.74 μs)
                     0.994 R²   (0.987 R² .. 0.998 R²)
mean                 20.28 μs   (19.54 μs .. 21.73 μs)
std dev              3.635 μs   (1.308 μs .. 6.217 μs)
variance introduced by outliers: 95% (severely inflated)

benchmarking parsePitches/cqs' cqf,  gqs''
time                 7.522 μs   (7.381 μs .. 7.634 μs)
                     0.997 R²   (0.996 R² .. 0.998 R²)
mean                 7.588 μs   (7.452 μs .. 7.740 μs)
std dev              506.6 ns   (414.4 ns .. 680.1 ns)
variance introduced by outliers: 74% (severely inflated)


benchmarking parsePitches/c d e f d
time                 21.02 μs   (20.00 μs .. 22.66 μs)
                     0.983 R²   (0.972 R² .. 0.998 R²)
mean                 20.59 μs   (20.14 μs .. 21.21 μs)
std dev              1.883 μs   (1.083 μs .. 2.630 μs)
variance introduced by outliers: 82% (severely inflated)

benchmarking parsePitches/cqs' cqf,  gqs''
time                 7.414 μs   (7.260 μs .. 7.610 μs)
                     0.996 R²   (0.994 R² .. 0.998 R²)
mean                 7.385 μs   (7.284 μs .. 7.503 μs)
std dev              400.5 ns   (325.7 ns .. 507.4 ns)
variance introduced by outliers: 65% (severely inflated)


-}
