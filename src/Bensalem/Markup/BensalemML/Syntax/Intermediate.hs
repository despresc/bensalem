{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Intermediate bensalem document syntax
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- Types respresenting the surface syntax of a bensalem document. Note that
-- these types do not represent source documents exactly; they discard
-- information like the precise indentation contexts of layout elements or the
-- exact levels of level elements. They also do not collect all the relevant
-- parts of elements together.
module Bensalem.Markup.BensalemML.Syntax.Intermediate
  ( -- * Intermediate syntax types
    Node (..),
    Element (..),
    Arg (..),
    Presentation (..),
    Attrs (..),
    AttrKey,
    AttrVal (..),

    -- * Intermediate syntax builders
    NodeSequence (..),
    Attr,
    text,
    lineSpace,
    blanks,
    escapeSequence,
    inlineComment,
    elementNode,
    element,
    singleAttr,
    addAttr,
    bracedAttrVal,
    setAttrVal,
  )
where

import Bensalem.Markup.BensalemML.ParserDefs
  ( Located (..),
    SrcSpan (..),
  )
import Bensalem.Markup.BensalemML.Token (EltName)
import qualified Bensalem.Markup.BensalemML.Token as Tok
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T

-- | A single node in bensalem syntax
data Node
  = -- | normal text not containing line space or newlines
    PlainText !Text
  | -- | a run of single space characters
    LineSpace !Int
  | -- | a line ending
    LineEnd
  | ElementNode !Element
  | InlineComment !Text
  deriving (Eq, Ord, Show)

-- | An element tag. Note that we only save the position of the element tag
-- itself. We also include all of the scope content of an element, which for
-- level and layout elements includes the attribute map, any arguments, as well
-- as the body argument content. Note that the scope content of an inline
-- element is always empty.
data Element = Element
  { elementTagPos :: !SrcSpan,
    elementName :: !EltName,
    elementAttrs :: !Attrs,
    elementArg :: !Arg
  }
  deriving (Eq, Ord, Show)

data Attrs = NoAttrs | Attrs !(Seq Attr)
  deriving (Eq, Ord, Show)

data Arg = NoArg | Arg !(Seq Node)
  deriving (Eq, Ord, Show)

-- | A raw attribute
type Attr = (Located Text, AttrVal)

data AttrVal
  = BracedAttrVal !(Seq Node)
  | SetAttrVal !(Seq Attr)
  deriving (Eq, Ord, Show)

data Presentation = PresentInline | PresentLayout | PresentLevel
  deriving (Eq, Ord, Show)

-- | The possible scope content of an element
data ScopeContent
  = -- | inline elements have no scope content
    InlineScopeContent
  | -- | layout elements contain everything in their layout scope
    LayoutScopeContent !(Seq Node)
  | -- | level elements contain everything in their level scope
    LevelScopeContent !(Seq Node)
  deriving (Eq, Ord, Show)

-- | An attribute key is a non-empty string of alphanumeric characters. This
-- currently matches the definition of element names.
type AttrKey = Text

-- TODO: really need to reintroduce this!
-- validAttrKey :: Text -> Maybe AttrKey
-- validAttrKey = Tok.validEltName

-- | A newtype over a sequence of nodes whose semigroup instance will merge
-- adjacent 'LineSpace' and 'PlainText' nodes

-- TODO: make opaque? want to maintain invariant that line space and plain text
-- is always merged
newtype NodeSequence = NodeSequence {unNodeSequence :: Seq Node}
  deriving (Eq, Ord, Show)

singleNode :: Node -> NodeSequence
singleNode = NodeSequence . Seq.singleton

instance Semigroup NodeSequence where
  NodeSequence x@(s :|> n) <> NodeSequence y@(m :<| t) = case (n, m) of
    (LineSpace a, LineSpace b) -> NodeSequence $ s <> Seq.singleton (LineSpace $ a + b) <> t
    (PlainText a, PlainText b) -> NodeSequence $ s <> Seq.singleton (PlainText $ a <> b) <> t
    _ -> NodeSequence $ x <> y
  NodeSequence x <> NodeSequence Empty = NodeSequence x
  NodeSequence Empty <> NodeSequence y = NodeSequence y

instance Monoid NodeSequence where
  mempty = NodeSequence mempty

-- | Unsafely construct a 'MixedContent' sequence from 'Text'. This function
-- does not check that the 'Text' is free of syntactically-significant
-- characters.
text :: Text -> NodeSequence
text t = singleNode $ PlainText t

-- | Unsafely construct a 'NodesBuilder' sequence from 'Text' consisting of
-- newlines and spaces
blanks :: Located Text -> NodeSequence
blanks (Located _ t) = NodeSequence $ Seq.fromList chunks
  where
    -- slightly different behaviour from Text.lines, since that function doesn't
    -- handle trailing newlines the way it's needed to here
    customLines x
      | T.null x = []
      | otherwise = begin : customLines end
      where
        (begin, end) = T.break (== '\n') $ T.drop 1 x
    chunks = concatMap go $ customLines t
    go x
      | n <= 0 = [LineEnd]
      | otherwise = [LineEnd, LineSpace n]
      where
        n = T.length x

escapeSequence :: Located Tok.Escape -> NodeSequence
escapeSequence (Located _ e) = case e of
  Tok.EscapeBackslash -> singleNode $ PlainText "\\"
  Tok.EscapeOpenBrace -> singleNode $ PlainText "{"
  Tok.EscapeCloseBrace -> singleNode $ PlainText "}"

-- | Unsafely construct a 'NodesBuilder' sequence representing a run of single
-- spaces.
lineSpace :: Located Int -> NodeSequence
lineSpace (Located _ n) = singleNode $ LineSpace n

inlineComment :: Located Text -> NodeSequence
inlineComment (Located _ t) = singleNode $ InlineComment t

elementNode :: Element -> NodeSequence
elementNode = singleNode . ElementNode

element :: Located EltName -> Attrs -> Maybe NodeSequence -> Element
element elname attrs content =
  Element (locatedSpan elname) (locatedVal elname) attrs content'
  where
    content' = maybe NoArg (Arg . unNodeSequence) content

singleAttr :: Attr -> Seq Attr
singleAttr = Seq.singleton

addAttr :: Seq Attr -> Attr -> Seq Attr
addAttr = (:|>)

bracedAttrVal :: NodeSequence -> AttrVal
bracedAttrVal = BracedAttrVal . unNodeSequence

setAttrVal :: Seq Attr -> AttrVal
setAttrVal = SetAttrVal
