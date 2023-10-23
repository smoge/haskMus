{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module RtmProperties where


import Music.Time.Rtm
import Test.QuickCheck
-- import Text.Pretty.Simple 

prop_toFromRtmArray :: RtmProportions -> Bool
prop_toFromRtmArray rtm = rtm == (fromRtmArray . toRtmArray) rtm


-- rtm =  RtmProportions [RtmLeaf 11 (RtmProportions [RtmNote 4,RtmRest 4,RtmNote 1,RtmNote 4,RtmNote 2,RtmRest 2,RtmRest 2,RtmRest 3,RtmRest 4,RtmNote 4,RtmNote 2]),RtmNote 14,RtmLeaf 14 (RtmProportions [RtmNote 2,RtmNote 4]),RtmLeaf 11 (RtmProportions [RtmRest 4,RtmNote 3]),RtmNote 11,RtmNote 11,RtmLeaf 13 (RtmProportions [RtmNote 3,RtmNote 2]),RtmNote 14,RtmLeaf 10 (RtmProportions [RtmRest 3,RtmNote 1,RtmRest 1,RtmRest 1,RtmNote 3,RtmRest 3,RtmNote 3,RtmRest 1,RtmRest 3,RtmRest 3,RtmRest 4]),RtmNote 13,RtmRest 12]
-- pPrint rtm
-- pPrint $ toRtmArray rtm
-- pPrint $ (fromRtmArray . toRtmArray) rtm


main :: IO ()
main = do
  verboseCheck prop_toFromRtmArray

-- return []

-- runTests :: IO Bool
-- runTests = $verboseCheckAll
--runTests = $quickCheckAll

