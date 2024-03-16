module TestXml where

import Text.XML.Light

-- import Language.Haskell.TH
-- import Language.Haskell.TH.Quote
import Data.Data
import Data.Maybe

-- Create a MusicXML note element with a single note
createNoteElement :: Element
createNoteElement =
  Element
    { elName = QName "note" Nothing Nothing
    , elAttribs = []
    , elLine = Nothing
    , elContent =
        [ Elem $
            Element
              { elName = QName "pitch" Nothing Nothing
              , elAttribs = []
              , elLine = Nothing
              , elContent =
                  [ Elem $
                      Element
                        { elName = QName "step" Nothing Nothing
                        , elAttribs = []
                        , elLine = Nothing
                        , elContent = [Text $ CData CDataText "C" Nothing]
                        }
                  , Elem $
                      Element
                        { elName = QName "octave" Nothing Nothing
                        , elAttribs = []
                        , elLine = Nothing
                        , elContent = [Text $ CData CDataText "4" Nothing]
                        }
                  ]
              }
        , Elem $
            Element
              { elName = QName "duration" Nothing Nothing
              , elAttribs = []
              , elLine = Nothing
              , elContent = [Text $ CData CDataText "4" Nothing]
              }
        ]
    }

-- Create a MusicXML part with the note
createMusicXMLPart :: Element
createMusicXMLPart =
  Element
    { elName = QName "part" Nothing Nothing
    , elAttribs = [Attr (QName "id" Nothing Nothing) "P1"]
    , elLine = Nothing
    , elContent =
        [ Elem $
            Element
              { elName = QName "measure" Nothing Nothing
              , elAttribs = [Attr (QName "number" Nothing Nothing) "1"]
              , elLine = Nothing
              , elContent = [Elem createNoteElement]
              }
        ]
    }

-- Create a complete MusicXML score
createMusicXMLScore :: Element
createMusicXMLScore =
  Element
    { elName = QName "score-partwise" Nothing Nothing
    , elAttribs = [Attr (QName "version" Nothing Nothing) "3.0"]
    , elLine = Nothing
    , elContent =
        [ Elem $
            Element
              { elName = QName "part-list" Nothing Nothing
              , elAttribs = []
              , elLine = Nothing
              , elContent = []
              }
        , Elem createMusicXMLPart
        ]
    }

main :: IO ()
main = do
  let xmlString = showTopElement createMusicXMLScore
  putStrLn xmlString

main2 :: IO ()
main2 = do
  let formattedXml = ppTopElement createMusicXMLScore
  putStrLn formattedXml

{-
λ> main2
<?xml version='1.0' ?>
<score-partwise version="3.0">
  <part-list />
  <part id="P1">
    <measure number="1">
      <note>
        <pitch>
          <step>C</step>
          <octave>4</octave>
        </pitch>
        <duration>4</duration>
      </note>
    </measure>
  </part>
</score-partwise>

-}
