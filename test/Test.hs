module Main
  ( main,
  )
where

import AccidentalProperties (runTests)
import PitchProperties (runTests)
import PitchSpec (spec)
import Test.Hspec (hspec)
-- import RtmProperties (runTests, prop_toFromRtmArray)

reportResults :: String -> IO Bool -> IO ()
reportResults testName action = do
  passed <- action
  if passed
    then putStrLn $ testName ++ ": All QuickCheck properties passed."
    else putStrLn $ testName ++ ": Some QuickCheck properties failed."

main :: IO ()
main = do
  putStrLn "Run the Hspec tests from PitchSpec"
  hspec PitchSpec.spec
  putStrLn "+++ Run the QuickCheck properties from AccidentalTests.hs"
  reportResults "AccidentalTests" AccidentalProperties.runTests
  putStrLn "+++ Run the QuickCheck properties from PitchProperties.hs"
  reportResults "PitchProperties" PitchProperties.runTests

  -- putStrLn "+++ Run the QuickCheck properties from RtmProperties.hs"
  -- reportResults "RtmProperties" RtmProperties.runTests