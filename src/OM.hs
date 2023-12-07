{-# LANGUAGE QuasiQuotes #-}


module OM where

import NeatInterpolation ( trimming )
import Data.Text (Text)
import qualified Data.Text.IO as T



header :: Text
header = [trimming|
;fileheader
; (7.0 :inst 0 0 0 \"doc\" 183)
;endfileheader

(in-package :om)
|]


poly :: Text -> Text
poly a = [trimming|
(setf *instance-to-load*
(omng-make-new-instance
(make-instance 'poly
  :voices
  (list
    $a 
  ))

"instance"))
|]




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

