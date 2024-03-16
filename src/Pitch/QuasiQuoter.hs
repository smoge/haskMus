module Pitch.QuasiQuoter (pitch) where

import Language.Haskell.TH (Exp, Quote)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (dataToExpQ)
import Pitch.Parser (parsePitches)

-- Define the parsePitches QuasiQuoter
pitch :: QuasiQuoter
pitch =
  QuasiQuoter
    { quoteExp = parsePitchesToExp
    , quotePat = \_ -> fail "Pattern quoting not supported for parsePitches QQ."
    , quoteType = \_ -> fail "Type quoting not supported for parsePitches QQ."
    , quoteDec = \_ -> fail "Declaration quoting not supported for parsePitches QQ."
    }

-- Convert a string to a [Pitch] Exp

parsePitchesToExp :: (MonadFail m, Quote m) => String -> m Exp
parsePitchesToExp s = case parsePitches s of
  Left err -> fail $ "Parse error: " <> show err
  Right pitches -> dataToExpQ (const Nothing) pitches

{- -----------------------------------------------------------------------------

p = [pitch|cs' gf,|]

p

-- [C Sharp Octave 5,G Flat Octave 3]
----------------------------------------------------------------------------- -}
