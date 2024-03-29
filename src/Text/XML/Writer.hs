{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Overcome XML insanity, node by node.
-- > {\-# LANGUAGE OverloadedStrings #-\}
-- >
-- > let doc = document "root" $ do
-- >     element "hello" $ content "world"
-- >     element "hierarchy" $ do
-- >         element "simple" True
-- >         element "as" ("it should be" :: Text)
-- >         toXML $ Just . T.pack $ "like this"
-- >     comment "that's it!"
module Text.XML.Writer
  ( -- * Documents
    document,
    -- soap,
    pprint,

    -- * Elements
    XML,

    -- ** Node creation
    node,
    instruction,
    comment,
    element,
    elementMaybe,
    elementA,
    content,
    empty,
    many,

    -- ** Element helpers
    render,
    (!:),

    -- ** Converting data
    ToXML (..),
    documentA,
    documentD,
    documentAD,
    soap,
  )
where

import Control.Monad.Writer.Strict
import Data.DList qualified as DL
import Data.Default
import Data.Map qualified as M
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy.IO qualified as TL
import Text.XML

-- | Node container to be rendered as children nodes.
type XML = Writer (DL.DList Node) ()

-- | Create a simple Document starting with a root element.
document ::
  -- | Root node name
  Name ->
  -- | Contents
  XML ->
  Document
document name children =
  Document
    { documentPrologue = Prologue def def def,
      documentRoot = Element name def (render children),
      documentEpilogue = def
    }

-- | Create a simple Document starting with a root element with attributes.
documentA ::
  -- | Root node name
  Name ->
  -- | Attributes
  [(Name, Text)] ->
  -- | Contents
  XML ->
  Document
documentA name attrs children =
  Document
    { documentPrologue = Prologue def def def,
      documentRoot = Element name (M.fromList attrs) (render children),
      documentEpilogue = def
    }

-- | Create a simple Document starting with a root element with a doctype.
documentD ::
  -- | Root node name
  Name ->
  -- | DOCTYPE
  Maybe Doctype ->
  -- | Contents
  XML ->
  Document
documentD name dt children =
  Document
    { documentPrologue = Prologue def dt def,
      documentRoot = Element name def (render children),
      documentEpilogue = def
    }

-- | Create a simple Document starting with a root element with attributes and doctype.
documentAD ::
  -- | Root node name
  Name ->
  -- | Attributes
  [(Name, Text)] ->
  -- | DOCTYPE
  Maybe Doctype ->
  -- | Contents
  XML ->
  Document
documentAD name attrs dt children =
  Document
    { documentPrologue = Prologue def dt def,
      documentRoot = Element name (M.fromList attrs) (render children),
      documentEpilogue = def
    }

-- | Render document using xml-conduit's pretty-printer.
pprint :: Document -> IO ()
pprint = TL.putStrLn . renderText def {rsPretty = True}

-- | Convert collected nodes to a list of child nodes.
render :: XML -> [Node]
render = DL.toList . execWriter

-- | Do nothing.
empty :: XML
empty = pure ()

-- | Insert one node.
node :: Node -> XML
node = tell . DL.singleton

-- | Insert an "Element" node constructed with name and children.
element :: (ToXML a) => Name -> a -> XML
element name children = node . NodeElement $! Element name def (render $ toXML children)

-- | Insert an "Element" node converted from Maybe value or do nothing.
elementMaybe :: (ToXML a) => Name -> Maybe a -> XML
elementMaybe name = maybe empty (element name)

-- | Insert an "Element" node constructed with name, attributes and children.
elementA :: (ToXML a) => Name -> [(Name, Text)] -> a -> XML
elementA name attrs children = node . NodeElement $! Element name (M.fromList attrs) (render $ toXML children)

-- | Insert an "Instruction" node.
instruction :: Text -> Text -> XML
instruction target data_ = node . NodeInstruction $! Instruction target data_

-- | Insert a text comment node.
comment :: Text -> XML
comment = node . NodeComment

-- | Insert text content node.
content :: Text -> XML
content = node . NodeContent

-- | Mass-convert to nodes.
--
-- > let array = element "container" $ many "wrapper" [1..3]
--
-- Which gives:
--
-- > <container>
-- >     <wrapper>1</wrapper>
-- >     <wrapper>2</wrapper>
-- >     <wrapper>3</wrapper>
-- > </container>
--
-- Use `mapM_ toXML xs` to convert a list without wrapping
-- each item in separate element.
--
-- > let mess = element "container" $ mapM_ toXML ["chunky", "chunk"]
--
-- Content nodes tend to glue together:
--
-- > <container>chunkychunk</container>
many ::
  (ToXML a) =>
  -- | Container element name.
  Name ->
  -- | Items to convert.
  [a] ->
  XML
many n = mapM_ (element n . toXML)

-- | Attach a prefix to a Name.
--
-- Because simply placing a colon in an element name
-- yields 'Nothing' as a prefix and children will
-- revert to en empty namespace.
(!:) :: Text -> Name -> Name
pref !: name = name {namePrefix = Just pref}

-- | Provide instances for this class to use your data
-- as "XML" nodes.
class ToXML a where
  toXML :: a -> XML

-- | Do nothing.
instance ToXML () where
  toXML () = empty

-- | Insert already prepared nodes.
instance ToXML XML where
  toXML = id

-- | Don't use [Char] please, it will scare OverloadedStrings.
instance ToXML Text where
  toXML = content

-- | XML schema uses lower case.
instance ToXML Bool where
  toXML True = "true"
  toXML False = "false"

instance ToXML Float where
  toXML = content . T.pack . show

instance ToXML Double where
  toXML = content . T.pack . show

instance ToXML Int where
  toXML = content . T.pack . show

instance ToXML Integer where
  toXML = content . T.pack . show

instance ToXML Char where
  toXML = content . T.singleton

-- | Insert node if available. Otherwise do nothing.
instance (ToXML a) => ToXML (Maybe a) where
  toXML = maybe empty toXML

instance IsString XML where
  fromString = content . T.pack

-- | Generate a SOAPv1.1 document.
--
-- Empty header will be ignored.
-- Envelope uses a `soapenv` prefix.
-- Works great with 'ToXML' class.
--
-- > data BigData = BigData { webScale :: Bool }
-- > instance ToXML BigData where
-- >     toXML (BigData ws) = element ("v" !: "{vendor:uri}bigData") $ toXML ws
-- > let doc = soap () (BigData True)
soap ::
  (ToXML h, ToXML b) =>
  h ->
  b ->
  Document
soap header body = document (sn "Envelope") $ do
  node . NodeElement $! Element (sn "Header") def headerContent
  element (sn "Body") (toXML body)
  where
    sn n = Name n (Just ns) (Just "soapenv")
    ns = "http://schemas.xmlsoap.org/soap/envelope/"
    headerContent = render (toXML header)
