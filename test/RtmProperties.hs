{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module RtmProperties where


import Music.Time.Rtm
import Test.QuickCheck
-- import Test.Framework (defaultMain, testGroup)
-- import Test.Framework.Providers.QuickCheck2 (testProperty)


prop_toFromRtmArray :: RtmProportions -> Bool
prop_toFromRtmArray rtm = rtm == (fromRtmArray . toRtmArray) rtm


main :: IO ()
main = do
--   quickCheck $ \x ->
--     let y = toRtmArray x
--         z = fromRtmArray y
--     in ((x == z) || traceShow (x, y, z) False)
  -- Generate random RtmProportions values and test the property
  quickCheck (forAll arbitrary prop_toFromRtmArray)

return []

runTests :: IO Bool
runTests = $quickCheckAll

{- 

>>> testRtm = RtmProportions [RtmLeaf    2 (RtmProportions [RtmLeaf 2 (RtmProportions [RtmNote 2,RtmNote 3,RtmRest 4])])]
>>> array = toRtmArray testRtm
>>> fromRtmArray array
RtmProportions [RtmLeaf 2 (RtmProportions [RtmNote 2,RtmNote 2])]


>>> testRtm = RtmProportions [RtmLeaf 2 (RtmProportions [RtmLeaf 2 (RtmProportions [RtmNote 2,RtmNote 3,RtmRest 4])])]
>>> array = toRtmArray testRtm
>>> fromRtmArray array
RtmProportions [RtmLeaf 2 (RtmProportions [RtmNote 2,RtmNote 2])]

>>> testRtm4 = RtmProportions [RtmLeaf 1 (RtmProportions [RtmLeaf 2 (RtmProportions [RtmNote 2,RtmNote 2]),RtmRest 3])]
>>> array4 = toRtmArray testRtm4
>>> array4
>>> fromRtmArray array4
>>> testRtm4 == ( fromRtmArray . toRtmArray) testRtm4
RtmArray [1,2,2,2,-3] (Vector [Vector [Scalar,Vector [Vector [Scalar,Vector [Scalar,Scalar]],Scalar]]])
RtmProportions [RtmLeaf 1 (RtmProportions [RtmNote 2])]
False


>>> testRtm3 = RtmProportions [RtmNote 2,RtmRest 1,RtmNote 1,RtmRest 1,RtmLeaf 2 (RtmProportions [RtmRest 1,RtmNote 1,RtmNote 4,RtmNote 2])]
>>> array3 = toRtmArray testRtm3
>>> show array3
>>> fromRtmArray array3
>>> testRtm3 == ( fromRtmArray . toRtmArray) testRtm3
"RtmArray [2,-1,1,-1,2,-1,1,4,2] (Vector [Scalar,Scalar,Scalar,Scalar,Vector [Scalar,Vector [Scalar,Scalar,Scalar,Scalar]]])"
RtmProportions [RtmNote 2,RtmRest 1,RtmNote 1,RtmRest 1,RtmLeaf 2 (RtmProportions [RtmRest 1,RtmNote 1])]
False

-}
