module Rtm.Parser (
    rtmParser,
    parseRtm,
) where

import Rtm.Common (Rtm, gap, printRtm, rtm', scalar, vector)
import Text.Parsec (
    ParseError,
    char,
    digit,
    lookAhead,
    many1,
    manyTill,
    oneOf,
    option,
    parse,
    skipMany,
    spaces,
    try,
    (<|>),
 )
import Text.Parsec.String (Parser)


-- Consume spaces before and after the parser.
spaced :: Parser a -> Parser a
spaced p = spaces *> p <* spaces

signedNumber :: Parser Int
signedNumber = spaced $ do
    sign <- option '+' (oneOf "+-")
    num <- read <$> many1 digit
    if num > 0 && num <= 20
        then pure $ if sign == '-' then (-num) else num
        else fail "Number out of range (1-20)"

elemParser :: Parser Rtm
elemParser = spaced $ try divParser <|> try restParser <|> try noteParser

divParser :: Parser Rtm
divParser = spaced $ do
    _ <- char '('
    num <- signedNumber
    _ <- spaces
    _ <- char '('
    elems <- manyTill (spaces_ >> elemParser) (lookAhead (char ')'))
    _ <- char ')'
    _ <- char ')'
    pure $ vector num elems

noteParser :: Parser Rtm
noteParser = do
    num <- signedNumber
    if num > 0
        then pure $ scalar num
        else fail "Non-positive note encountered"

restParser :: Parser Rtm
restParser = do
    num <- signedNumber
    if num < 0
        then pure $ gap (-num)
        else fail "Non-negative rest encountered"

rtmParser :: Parser Rtm
rtmParser = spaced $ do
    _ <- char '('
    elems <- spaced $ many1 (elemParser <* spaces_)
    _ <- char ')'
    pure $ rtm' elems

spaces_ :: Parser ()
spaces_ = skipMany (char ' ')

parseRtm :: String -> Either ParseError Rtm
parseRtm = parse rtmParser ""

{-
-- Tests

test_01 :: IO ()
test_01 = do
    let input = "(1 -1 (1 (-1 -1 1)))"
    case parseRtm input of
        Left err -> print err
        Right tree -> printRtm tree

test_02 :: IO ()
test_02 = do
    let input = "(2 -1 2 (1 (-1 (1 (1 1 1)) 1)))"
    case parseRtm input of
        Left err -> print err
        Right tree -> printRtm tree

test_03 :: IO ()
test_03 = do
    let input = "(1 -1 (1 ((1 (-1 -1 1)) -1 1)))"
    case parseRtm input of
        Left err -> print err
        Right tree -> printRtm tree

test_04 :: IO ()
test_04 = do
    let input = "(1 -2 1 (1 (1 (1 (1 1 1)) 1)))"
    case parseRtm input of
        Left err -> print err
        Right tree -> printRtm tree
 -}
