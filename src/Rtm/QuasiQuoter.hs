-- | QuasiQuoter for Rtm
module Rtm.QuasiQuoter (rtm) where

import Language.Haskell.TH (Exp, Quote)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (dataToExpQ)
import Rtm.Parser (parseRtm)

{- | QuasiQuoter for Rtm.
 example :: Rtm
 example = [rtm| (1 -1 (1 (1 -1 1))) |]
 printRtm example
-}
rtm :: QuasiQuoter
rtm =
    QuasiQuoter
        { quoteExp = rtmToExp,
          quotePat = \_ -> fail "Pattern quoting not supported for rtm QQ.",
          quoteType = \_ -> fail "Type quoting not supported for rtm QQ.",
          quoteDec = \_ -> fail "Declaration quoting not supported for rtm QQ."
        }

rtmToExp :: (MonadFail m, Quote m) => String -> m Exp
rtmToExp s = case Rtm.Parser.parseRtm s of
    Left err -> fail $ "Parse error: " <> show err
    Right tree -> dataToExpQ (const Nothing) tree
