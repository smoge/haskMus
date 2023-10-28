{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Parsers.RtmParser where

import           Control.Applicative
import           Data.Either         (fromRight)
import           Data.Tree
import qualified Data.Tree           as T
import           Debug.Trace
import           Test.HUnit
import           Test.QuickCheck     hiding (vector)

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

-- isValidComponent :: Component -> Bool
-- isValidComponent (T.Node (Scalar n) []) = n > 0
-- isValidComponent (T.Node (Gap n) [])    = n > 0
-- isValidComponent (T.Node (Vector n) cs) = n > 0 && all isValidComponent cs
-- isValidComponent _                      = False
isValidComponent :: Component -> Bool
isValidComponent comp = isValidComponentAux comp 0
  where
    depthThreshold = 4 -- Adjust this value based on your requirements
    isValidComponentAux :: Component -> Int -> Bool
    isValidComponentAux (T.Node (Scalar n) []) _ = n > 0
    isValidComponentAux (T.Node (Gap n) []) _ = n > 0
    isValidComponentAux (T.Node (Vector n) cs) depth
      | depth >= depthThreshold = n > 0 && all isLeaf cs
      | otherwise = n > 0 && all (`isValidComponentAux` (depth + 1)) cs
    isValidComponentAux _ _ = False
    isLeaf (T.Node (Scalar _) []) = True
    isLeaf (T.Node (Gap _) [])    = True
    isLeaf _                      = False

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

--
-- QUICKCHECK
-- Define Arbitrary instances for ComponentLabel and Component
instance Arbitrary ComponentLabel where
  arbitrary =
    oneof [Scalar <$> positiveInt, Gap <$> positiveInt, Vector <$> positiveInt]

arbitraryComponent :: Gen Component
arbitraryComponent = sized genComponent

genComponent :: Int -> Gen Component
genComponent 0 = T.Node <$> arbitraryLeaf <*> pure []
genComponent n
  | n > 0 =
    oneof
      [vectorWithChildren n, arbitraryLeaf >>= \label -> pure (T.Node label [])]

vectorWithChildren :: Int -> Gen Component
vectorWithChildren n = do
  label <- Vector <$> positiveInt
  numChildren <- choose (2, 8)
  children <- vectorOf numChildren (genComponent (n `div` (numChildren + 1))) -- Distributing remaining size amongst children
  return (T.Node label children)

arbitraryLeaf :: Gen ComponentLabel
arbitraryLeaf = oneof [Scalar <$> positiveInt, Gap <$> positiveInt]

positiveInt :: Gen Int
positiveInt = choose (1, 9)

prop_isValidComponent :: Property
prop_isValidComponent =
  forAll arbitraryComponent $ \component ->
    traceShowId (show component) `seq` isValidComponent component ==> True

-- Round-Trip Test
testRoundTrip :: Test
testRoundTrip =
  TestCase $ do
    let original = createComponent
    let serialized = componentToString original
    let parsed =
          fromRight (error "Parse failed") $ parseComponentString serialized
    assertEqual "Round trip test failed" original parsed

prop_roundTrip :: Property
prop_roundTrip =
  forAll arbitraryComponent $ \validComponent ->
    let serialized = componentToString validComponent
        parsed = parseComponentString serialized
     in isRight parsed
          && fromRight (error "Parse failed") parsed == validComponent

-- QuickCheck Property for Random Component Parsing
prop_parseComponent :: Property
prop_parseComponent =
  forAll arbitraryComponent $ \validComponent ->
    let serialized = componentToString validComponent
        parsed = parseComponentString serialized
     in isRight parsed
          && fromRight (error "Parse failed") parsed == validComponent

-- QuickCheck Property to ensure Vector has children and leafs don't
prop_structureValidity :: Property
prop_structureValidity =
  forAll arbitraryComponent $ \validComponent ->
    case validComponent of
      (T.Node (Vector _) cs) -> not (null cs)
      (T.Node (Scalar _) cs) -> null cs
      (T.Node (Gap _) cs)    -> null cs
      _                      -> False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

-- main :: IO ()
-- main = do
--   verboseCheck prop_isValidComponent
-- -- Grouping tests
-- tests :: Test
-- tests =
--   TestList
--     [testComponentToString, testParseComponentString, testIsValidComponent]
-- Running tests in main
main3 :: IO Counts
main3 = do
  putStrLn "Running tests..."
  runTestTT tests

-- Grouping tests
tests :: Test
tests =
  TestList
    [ testComponentToString
    , testParseComponentString
    , testIsValidComponent
    , testRoundTrip
    ]

-- Running tests in main
main4 :: IO ()
main4 = do
  -- putStrLn "Running HUnit tests..."
  -- _ <- runTestTT tests
  putStrLn "\nRunning QuickCheck tests..."
  verboseCheck prop_roundTrip
  verboseCheck prop_parseComponent
  verboseCheck prop_structureValidity
{-
t = Node
    { rootLabel = Vector 2
    , subForest =
        [ Node {rootLabel = Scalar 3, subForest = []}
        , Node {rootLabel = Scalar 9, subForest = []}
        , Node {rootLabel = Scalar 1, subForest = []}
        , Node
            { rootLabel = Vector 4
            , subForest =
                [ Node {rootLabel = Gap 8, subForest = []}
                , Node
                    { rootLabel = Vector 1
                    , subForest =
                        [ Node {rootLabel = Scalar 7, subForest = []}
                        , Node {rootLabel = Scalar 7, subForest = []}
                        ]
                    }
                , Node {rootLabel = Gap 1, subForest = []}
                ]
            }
        , Node
            { rootLabel = Vector 2
            , subForest =
                [ Node {rootLabel = Scalar 3, subForest = []}
                , Node
                    { rootLabel = Vector 5
                    , subForest =
                        [ Node {rootLabel = Gap 6, subForest = []}
                        , Node {rootLabel = Scalar 6, subForest = []}
                        , Node {rootLabel = Gap 2, subForest = []}
                        , Node {rootLabel = Scalar 5, subForest = []}
                        , Node {rootLabel = Gap 2, subForest = []}
                        , Node {rootLabel = Scalar 2, subForest = []}
                        , Node {rootLabel = Scalar 1, subForest = []}
                        ]
                    }
                , Node {rootLabel = Gap 9, subForest = []}
                , Node
                    { rootLabel = Vector 7
                    , subForest =
                        [ Node {rootLabel = Scalar 5, subForest = []}
                        , Node {rootLabel = Scalar 9, subForest = []}
                        ]
                    }
                , Node {rootLabel = Scalar 2, subForest = []}
                , Node
                    { rootLabel = Vector 5
                    , subForest =
                        [ Node {rootLabel = Gap 8, subForest = []}
                        , Node {rootLabel = Gap 9, subForest = []}
                        , Node {rootLabel = Scalar 5, subForest = []}
                        ]
                    }
                , Node
                    { rootLabel = Vector 1
                    , subForest =
                        [ Node {rootLabel = Gap 9, subForest = []}
                        , Node {rootLabel = Scalar 4, subForest = []}
                        , Node {rootLabel = Scalar 2, subForest = []}
                        , Node {rootLabel = Gap 4, subForest = []}
                        ]
                    }
                ]
            }
        , Node
            { rootLabel = Vector 6
            , subForest =
                [ Node {rootLabel = Scalar 5, subForest = []}
                , Node
                    { rootLabel = Vector 7
                    , subForest =
                        [ Node {rootLabel = Scalar 7, subForest = []}
                        , Node {rootLabel = Scalar 2, subForest = []}
                        , Node {rootLabel = Gap 7, subForest = []}
                        , Node {rootLabel = Scalar 1, subForest = []}
                        , Node {rootLabel = Scalar 5, subForest = []}
                        ]
                    }
                , Node {rootLabel = Gap 1, subForest = []}
                , Node
                    { rootLabel = Vector 3
                    , subForest =
                        [ Node {rootLabel = Scalar 3, subForest = []}
                        , Node {rootLabel = Gap 3, subForest = []}
                        , Node {rootLabel = Scalar 3, subForest = []}
                        ]
                    }
                , Node {rootLabel = Gap 5, subForest = []}
                , Node {rootLabel = Scalar 8, subForest = []}
                , Node {rootLabel = Gap 2, subForest = []}
                , Node
                    { rootLabel = Vector 4
                    , subForest =
                        [ Node {rootLabel = Scalar 8, subForest = []}
                        , Node {rootLabel = Gap 3, subForest = []}
                        , Node {rootLabel = Gap 1, subForest = []}
                        , Node {rootLabel = Scalar 4, subForest = []}
                        ]
                    }
                ]
            }
        , Node {rootLabel = Gap 3, subForest = []}
        , Node
            { rootLabel = Vector 6
            , subForest =
                [ Node {rootLabel = Scalar 6, subForest = []}
                , Node {rootLabel = Scalar 8, subForest = []}
                , Node {rootLabel = Gap 3, subForest = []}
                , Node {rootLabel = Gap 5, subForest = []}
                ]
            }
        ]
    }


printTree t


Vector 2
|
+- Scalar 3
|
+- Scalar 9
|
+- Scalar 1
|
+- Vector 4
|  |
|  +- Gap 8
|  |
|  +- Vector 1
|  |  |
|  |  +- Scalar 7
|  |  |
|  |  `- Scalar 7
|  |
|  `- Gap 1
|
+- Vector 2
|  |
|  +- Scalar 3
|  |
|  +- Vector 5
|  |  |
|  |  +- Gap 6
|  |  |
|  |  +- Scalar 6
|  |  |
|  |  +- Gap 2
|  |  |
|  |  +- Scalar 5
|  |  |
|  |  +- Gap 2
|  |  |
|  |  +- Scalar 2
|  |  |
|  |  `- Scalar 1
|  |
|  +- Gap 9
|  |
|  +- Vector 7
|  |  |
|  |  +- Scalar 5
|  |  |
|  |  `- Scalar 9
|  |
|  +- Scalar 2
|  |
|  +- Vector 5
|  |  |
|  |  +- Gap 8
|  |  |
|  |  +- Gap 9
|  |  |
|  |  `- Scalar 5
|  |
|  `- Vector 1
|     |
|     +- Gap 9
|     |
|     +- Scalar 4
|     |
|     +- Scalar 2
|     |
|     `- Gap 4
|
+- Vector 6
|  |
|  +- Scalar 5
|  |
|  +- Vector 7
|  |  |
|  |  +- Scalar 7
|  |  |
|  |  +- Scalar 2
|  |  |
|  |  +- Gap 7
|  |  |
|  |  +- Scalar 1
|  |  |
|  |  `- Scalar 5
|  |
|  +- Gap 1
|  |
|  +- Vector 3
|  |  |
|  |  +- Scalar 3
|  |  |
|  |  +- Gap 3
|  |  |
|  |  `- Scalar 3
|  |
|  +- Gap 5
|  |
|  +- Scalar 8
|  |
|  +- Gap 2
|  |
|  `- Vector 4
|     |
|     +- Scalar 8
|     |
|     +- Gap 3
|     |
|     +- Gap 1
|     |
|     `- Scalar 4
|
+- Gap 3
|
`- Vector 6
   |
   +- Scalar 6
   |
   +- Scalar 8
   |
   +- Gap 3
   |
   `- Gap 5
 -}
