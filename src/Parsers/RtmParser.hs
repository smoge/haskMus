{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Parsers.RtmParser where

import           Control.Applicative
import           Data.Either         (fromRight)
import qualified Data.Tree           as T
import           Test.HUnit
import           Text.Parsec         hiding ((<|>))
import           Text.Parsec.String

data ComponentLabel
  = Scalar Int
  | Gap Int
  | Vector Int
  deriving (Eq, Show, Ord)

type Component = T.Tree ComponentLabel

scalar :: Int -> Component
scalar n = T.Node (Scalar n) []

gap :: Int -> Component
gap n = T.Node (Gap n) []

vector :: Int -> [Component] -> Component
vector n cs = T.Node (Vector n) cs

printTree :: Component -> IO ()
printTree = putStrLn . T.drawTree . fmap show

printTree' :: Show a => T.Tree a -> IO ()
printTree' = putStrLn . T.drawTree . fmap show

--test
createComponent :: Component
createComponent = vector 2 [scalar 3, gap 1, vector 4 [scalar 5, gap 2]]

-- Function to convert Component to a String representation
componentToString :: Component -> String
componentToString (T.Node (Scalar n) []) = show n
componentToString (T.Node (Gap n) []) = "-" ++ show n
componentToString (T.Node (Vector n) cs) =
  "(" ++ show n ++ " (" ++ unwords (map componentToString cs) ++ "))"
componentToString (T.Node (Scalar _) _) =
  error "Scalar should not have children"
componentToString (T.Node (Gap _) _) = error "Gap should not have children"

isValidComponent :: Component -> Bool
isValidComponent (T.Node (Scalar n) []) = n > 0
isValidComponent (T.Node (Gap n) [])    = n > 0
isValidComponent (T.Node (Vector n) cs) = n > 0 && all isValidComponent cs
isValidComponent _                      = False

componentParser :: Parser Component
componentParser = try vectorParser <|> try scalarParser <|> gapParser
  where
    scalarParser :: Parser Component
    scalarParser = scalar . read <$> many1 digit
    gapParser :: Parser Component
    gapParser = gap . read <$> (char '-' *> many1 digit)
    vectorParser :: Parser Component
    vectorParser = do
      _ <- char '('
      n <- read <$> many1 digit
      _ <- spaces >> char '('
      comps <- sepBy componentParser spaces
      _ <- char ')' >> char ')'
      return $ vector n comps

parseComponentString :: String -> Either ParseError Component
parseComponentString s = parse componentParser "" s

main2 :: IO ()
main2 = do
  let example = "(2 (1 1 1 3 -1 (4 (5 1 1 -2))))"
  let parsed = parseComponentString example
  case parsed of
    Right comp -> do
      putStrLn "Parsed successfully:"
      printTree comp
    Left err -> do
      putStrLn "Failed to parse:"
      print err
 -----
 ---------

{-
>>> componentToString createComponent
"(2 (3 -1 (4 (5 -2))))"
 -}
-- TEST
-- Testing the componentToString function
testComponentToString :: Test
testComponentToString =
  TestCase $ do
    assertEqual
      "Test for createComponent"
      "(2 (3 -1 (4 (5 -2))))"
      (componentToString createComponent)

-- Testing the parser
testParseComponentString :: Test
testParseComponentString =
  TestCase $ do
    let exampleStr = "(2 (3 -1 (4 (5 -2))))"
    let parsed = parseComponentString exampleStr
    assertEqual
      "Test for parsing a component string"
      (Right createComponent)
      parsed

-- Testing the validator
testIsValidComponent :: Test
testIsValidComponent =
  TestCase $ do
    let validComp = createComponent
    let invalidComp = vector (-1) [scalar 3]
    assertBool "Test for valid component" (isValidComponent validComp)
    assertBool "Test for invalid component" (not (isValidComponent invalidComp))

-- Grouping tests
tests :: Test
tests =
  TestList
    [testComponentToString, testParseComponentString, testIsValidComponent]

-- Running tests in main
main :: IO Counts
main = do
  putStrLn "Running tests..."
  runTestTT tests
