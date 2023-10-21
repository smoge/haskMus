module RtmProperties where


import Music.Time.Rtm
import Test.QuickCheck

prop_toFromRtmArray :: RtmProportions -> Bool
prop_toFromRtmArray rtm = rtm == (fromRtmArray . toRtmArray) rtm

main :: IO ()
main = do
  quickCheck prop_toFromRtmArray

{- 

>>> testRtm = RtmProportions [RtmLeaf 2 (RtmProportions [RtmLeaf 2 (RtmProportions [RtmNote 2,RtmNote 3,RtmRest 4])])]
>>> array = toRtmArray testRtm
>>> fromRtmArray array
RtmProportions [RtmLeaf 2 (RtmProportions [RtmNote 2,RtmNote 2])]


 -}
