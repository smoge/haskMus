{-# LANGUAGE QuasiQuotes #-}


module OM where

import NeatInterpolation
import Data.Text (Text)
import qualified Data.Text.IO as T



-- teste


f :: Text -> Text -> Text
f a b =
  [trimming|
    function(){
      function(){
        $a
      }
      return $b
    }
  |]



main :: IO ()
main = T.putStrLn $ f "1" "2"

