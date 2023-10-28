{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Parsers.RtmParser where

import           Control.Applicative
import           Data.Either         (fromRight)
import qualified Data.Tree           as T
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

isValidComponent :: Component -> Bool
isValidComponent (T.Node (Scalar n) []) = n > 0
isValidComponent (T.Node (Gap n) [])    = n > 0
isValidComponent (T.Node (Vector n) cs) = n > 0 && all isValidComponent cs
isValidComponent _                      = False

-- parse rtm
componentLabel :: Parser ComponentLabel
componentLabel = try vectorParser <|> scalarParser <|> gapParser
  where
    scalarParser = Scalar . read <$> many1 digit
    gapParser = Gap . abs <$> (char '-' *> (read <$> many1 digit))
    vectorParser =
      Vector . read
        <$> (char '[' *> spaces *> many1 digit <* spaces <* char ',' <* spaces)

component :: Parser Component
component = scalarComponent <|> gapComponent <|> vectorComponent
  where
    scalarComponent = scalar . read <$> many1 digit
    gapComponent = gap . abs . read <$> (char '-' *> many1 digit)
    vectorComponent = do
      _ <- char '['
      n <- read <$> many1 digit
      _ <- char ','
      _ <- spaces
      _ <- char '['
      cs <- sepBy component (spaces >> char ',' >> spaces)
      _ <- char ']'
      _ <- char ']'
      return $ vector n cs

parseComponent :: String -> Either ParseError Component
parseComponent input = parse (component <* eof) "" input

main :: IO ()
main = do
  let example = "[2, [3,-1,[4, [5,-2]]]]"
  let parsed = parseComponent example
  case parsed of
    Right comp -> do
      putStrLn "Parsed successfully:"
      print comp
      printTree comp
    Left err -> do
      putStrLn "Failed to parse:"
      print err

{-
Parsed successfully:
Node {rootLabel = Vector 2, subForest = [Node {rootLabel = Scalar 3, subForest = []},Node {rootLabel = Gap 1, subForest = []},Node {rootLabel = Vector 4, subForest = [Node {rootLabel = Scalar 5, subForest = []},Node {rootLabel = Gap 2, subForest = []}]}]}
Vector 2
|
+- Scalar 3
|
+- Gap 1
|
`- Vector 4
   |
   +- Scalar 5
   |
   `- Gap 2
 -}
-- test simple case
data NestedList
  = Number Int
  | List [NestedList]
  deriving (Show)

-- Parser for NestedList
value :: Parser NestedList
value =
  List <$> (char '(' *> sepBy value spaces <* char ')')
    <|> Number . read <$> many1 (digit <|> char '-')

-- Function to parse a LISP-style list
parseLISP :: String -> Either ParseError NestedList
parseLISP input = parse (value <* eof) "" input

-- test1 :: IO ()
-- test1 = do
--   let example = "(1 (2 (3 -4)) 5 (6 (7 (8 (-9 10)))))"
--   let parsed = parseLISP example
--   case parsed of
--     Right list -> do
--       putStrLn "Parsed successfully:"
--       print list
--     Left err -> do
--       putStrLn "Failed to parse:"
--       print err
{-
Parsed successfully:
List [Number 1,List [Number 2,List [Number 3,Number (-4)]],Number 5,List [Number 6,List [Number 7,List [Number 8,List [Number (-9),Number 10]]]]]
 -}
nestedListToComponent :: NestedList -> Component
nestedListToComponent (Number n) = scalar n
nestedListToComponent (List []) = vector 0 []
nestedListToComponent (List (x:xs)) =
  vector (getNumber x) (map nestedListToComponent xs)
  where
    getNumber (Number n) = n
    getNumber _          = 0

example :: String
example = "(1 1 (1 (1 1 1)) 1)"

parsed :: Either ParseError NestedList
parsed = parseLISP example

comp :: Component
comp = nestedListToComponent $ fromRight (List []) parsed
{-
>>> isValidComponent comp
True

printTree comp

-}
