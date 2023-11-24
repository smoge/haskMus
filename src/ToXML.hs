{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

main2 :: IO ()
main2 = do
  let musicXmlDoc = 
        documentMusicXML
          (Name "score-partwise" Nothing Nothing)
          [("version", "4.0")]
          createMusicXmlDoc
  print musicXmlDoc

------------------------------------------------------------

data TimeSignature = TimeSignature
  { upper :: Integer,
    lower :: Integer
  }
  deriving (Eq, Ord)

instance ToXML TimeSignature where
  toXML (TimeSignature n d) = element "time" $ do
    element "beats" $ content (T.pack $ show n)
    element "beat-type" $ content (T.pack $ show d)

-- <pitch>
--    <step>B</step>
--    <alter>-0.5</alter>
--    <octave>4</octave>
-- </pitch>

-- instance ToXML Pitch where
--   toXML (Pitch pn acc o) = element "pitch" $ do
--     element "step" $ content (T.pack $ show pn)
--     element "alter" $ content (T.pack $ accidentalXMLString acc)
--     element "octave" $ content (T.pack $ show o)

-- See: https://github.com/w3c/musicxml/issues/263
-- <accidental smufl="accidentalFlatThreeArrowsUp">other</accidental>

-- https://www.w3.org/2021/06/musicxml40/musicxml-reference/data-types/accidental-value/

-- musicxmlxsd line 1521

-- <xs:simpleType name="accidental-value">
-- 	<xs:annotation>
-- 		<xs:documentation>The accidental-value type represents notated accidentals supported by MusicXML. In the MusicXML 2.0 DTD this was a string with values that could be included. The XSD strengthens the data typing to an enumerated list. The quarter- and three-quarters- accidentals are Tartini-style quarter-tone accidentals. The -down and -up accidentals are quarter-tone accidentals that include arrows pointing down or up. The slash- accidentals are used in Turkish classical music. The numbered sharp and flat accidentals are superscripted versions of the accidental signs, used in Turkish folk music. The sori and koron accidentals are microtonal sharp and flat accidentals used in Iranian and Persian music. The other accidental covers accidentals other than those listed here. It is usually used in combination with the smufl attribute to specify a particular SMuFL accidental. The smufl attribute may be used with any accidental value to help specify the appearance of symbols that share the same MusicXML semantics.</xs:documentation>
-- 	</xs:annotation>
-- 	<xs:restriction base="xs:string">
-- 		<xs:enumeration value="sharp"/>
-- 		<xs:enumeration value="natural"/pp
-- 		<xs:enumeration value="flat"/>
-- 		<xs:enumeration value="double-sharp"/>
-- 		<xs:enumeration value="sharp-sharp"/>
-- 		<xs:enumeration value="flat-flat"/>
-- 		<xs:enumeration value="natural-sharp"/>
-- 		<xs:enumeration value="natural-flat"/>
-- 		<xs:enumeration value="quarter-flat"/>
-- 		<xs:enumeration value="quarter-sharp"/>
-- 		<xs:enumeration value="three-quarters-flat"/>
-- 		<xs:enumeration value="three-quarters-sharp"/>
-- 		<xs:enumeration value="sharp-down"/>
-- 		<xs:enumeration value="sharp-up"/>
-- 		<xs:enumeration value="natural-down"/>
-- 		<xs:enumeration value="natural-up"/>
-- 		<xs:enumeration value="flat-down"/>
-- 		<xs:enumeration value="flat-up"/>
-- 		<xs:enumeration value="double-sharp-down"/>
-- 		<xs:enumeration value="double-sharp-up"/>
-- 		<xs:enumeration value="flat-flat-down"/>
-- 		<xs:enumeration value="flat-flat-up"/>
-- 		<xs:enumeration value="arrow-down"/>
-- 		<xs:enumeration value="arrow-up"/>
-- 		<xs:enumeration value="triple-sharp"/>
-- 		<xs:enumeration value="triple-flat"/>
-- 		<xs:enumeration value="slash-quarter-sharp"/>
-- 		<xs:enumeration value="slash-sharp"/>
-- 		<xs:enumeration value="slash-flat"/>
-- 		<xs:enumeration value="double-slash-flat"/>
-- 		<xs:enumeration value="sharp-1"/>
-- 		<xs:enumeration value="sharp-2"/>
-- 		<xs:enumeration value="sharp-3"/>
-- 		<xs:enumeration value="sharp-5"/>
-- 		<xs:enumeration value="flat-1"/>
-- 		<xs:enumeration value="flat-2"/>
-- 		<xs:enumeration value="flat-3"/>
-- 		<xs:enumeration value="flat-4"/>
-- 		<xs:enumeration value="sori"/>
-- 		<xs:enumeration value="koron"/>
-- 		<xs:enumeration value="other"/>
-- 	</xs:restriction>
-- </xs:simpleType>

-- <xs:element name="divisions" type="positive-divisions" minOccurs="0">
-- 	<xs:annotation> <xs:documentation>Musical notation duration is
-- 	commonly represented as fractions. The divisions element indicates
-- 	how many divisions per quarter note are used to indicate a note's
-- 	duration. For example, if duration = 1 and divisions = 2, this is an
-- 	eighth note duration. Duration and divisions are used directly for
-- 	generating sound output, so they must be chosen to take tuplets into
-- 	account. Using a divisions element lets us use just one number to
-- 	represent a duration for each note in the score, while retaining the
-- 	full power of a fractional representation. If maximum compatibility
-- 	with Standard MIDI 1.0 files is important, do not have the divisions
-- 	value exceed 16383.</xs:documentation> </xs:annotation>
-- 	</xs:element>

------------------------------------------------------------

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
