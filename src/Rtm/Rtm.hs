{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Rtm.Rtm where

import Rtm.Common
import Rtm.Parser (parseRtm, rtmParser)
import Rtm.QuasiQuoter (rtm)

example :: Rtm
example = [rtm| (1 -1 (1 (1 -1 1))) |]
