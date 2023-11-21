{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ToXML where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Text.XML
import Text.XML.Writer

-- | Create a MusicXML Document with a specific DOCTYPE
documentMusicXML ::
  -- | Root node name
  Name ->
  -- | Attributes
  [(Name, T.Text)] ->
  -- | Contents
  XML ->
  Document
documentMusicXML name attrs children =
  Document
    { documentPrologue =
        Prologue
          { prologueBefore = [],
            prologueDoctype =
              Just
                $ Doctype
                  "score-partwise"
                  ( Just
                      $ PublicID
                        "-//Recordare//DTD MusicXML 4.0 Partwise//EN"
                        "http://www.musicxml.org/dtds/partwise.dtd"
                  ),
            prologueAfter = []
          },
      documentRoot = Element name (M.fromList attrs) (render children),
      documentEpilogue = []
    }

createMusicXmlDoc :: XML
createMusicXmlDoc =
  do
    elementA "part-list" [] $ do
      elementA "score-part" [("id", "P1")] $ do
        element "part-name" $ content "Music"
    elementA "part" [("id", "P1")] $ do
      elementA "measure" [("number", "1")] $ do
        element "attributes" $ do
          element "divisions" $ content "1"
          element "key" $ do
            element "fifths" $ content "0"
          element "time" $ do
            element "beats" $ content "4"
            element "beat-type" $ content "4"
          element "clef" $ do
            element "sign" $ content "G"
            element "line" $ content "2"
        element "note" $ do
          element "pitch" $ do
            element "step" $ content "C"
            element "octave" $ content "4"
          element "duration" $ content "4"
          element "type" $ content "whole"

main :: IO ()
main = do
  let musicXmlDoc =
        documentMusicXML
          (Name "score-partwise" Nothing Nothing)
          [("version", "4.0")]
          createMusicXmlDoc
  pprint musicXmlDoc

-- example
-- Serializing is easy!
data ExampleADT
  = NullaryExample
  | UnaryExample String
  | RecordExample
      { earFoo :: Int,
        earBar :: Maybe Bool,
        earBaz :: [Float]
      }

instance ToXML ExampleADT where
  toXML NullaryExample = empty
  toXML (UnaryExample s) = element "unary" $ content (T.pack s)
  toXML (RecordExample {earFoo = foo, earBar = bar, earBaz = baz}) =
    element "record" $ do
      element "foo" $ toXML foo
      element "bar" $ toXML bar
      element "baz" $ many "fnord" baz