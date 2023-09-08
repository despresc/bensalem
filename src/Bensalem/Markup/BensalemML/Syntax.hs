{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

-- |
-- Description : Intermediate bensalem document syntax
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- Types respresenting the surface syntax of a bensalem document. Note that
-- these types do not represent source documents exactly; they discard
-- information like the precise indentation contexts of layout elements or the
-- exact levels of level elements.
module Bensalem.Markup.BensalemML.Syntax
  ( -- * Syntax types
    Node (..),
    Element (..),
    ScopeContent (..),
    Attrs (..),
    AttrKey,
    AttrVal (..),
    SrcName (..),
    SrcNamePos (..),

    -- * Intermediate syntax builders
    NodeSequence (..),
    Attr,
    text,
    lineSpace,
    indent,
    escape,
    inlineComment,
    group,
    elementNode,
    element,
    layoutScopeContent,
    levelScopeContent,
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
import Bensalem.Markup.BensalemML.Token (AttrKey, EltName, Escape)
import qualified Bensalem.Markup.BensalemML.Token as Tok
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T

-- | A name as it appears in the source, before resolution
data SrcName = SrcName
  { srcNamePos :: !SrcNamePos,
    srcNameStr :: !Text
  }
  deriving (Eq, Ord, Show)

-- | The possible position of a source name

-- TODO: we're going to need to classify why the name doesn't have a position more precisely
data SrcNamePos
  = -- | we don't have any information about the location of this
    -- name
    NoSrcNamePos
  | SrcNamePos !SrcSpan
  deriving (Eq, Ord, Show)

-- | A single node in bensalem syntax
data Node name
  = -- | normal text not containing line space or newlines
    PlainText !Text
  | -- | a run of single space characters
    LineSpace !Int
  | -- | a line ending
    LineEnd
  | ElementNode !(Element name)
  | GroupNode !(Group name)
  | InlineComment !Text
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Group name = Group
  { groupStart :: !SrcSpan,
    groupEnd :: !SrcSpan,
    groupContent :: !(Seq (Node name))
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | An element tag. Note that we only save the position of the element tag
-- itself. We also include all of the scope content of an element, which for
-- level and layout elements includes the attribute map, any arguments, as well
-- as the body argument content. Note that the scope content of an inline
-- element is always empty.
data Element name = Element
  { elementTagPos :: !SrcSpan,
    elementName :: !name,
    elementAttrs :: !(Attrs name),
    elementScopeContent :: !(ScopeContent name)
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Attrs name = NoAttrs | Attrs !(Seq (Attr name))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | A raw attribute
type Attr name = (Located Text, AttrVal name)

data AttrVal name
  = BracedAttrVal !(Seq (Node name))
  | SetAttrVal !(Seq (Attr name))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Presentation = PresentInline | PresentLayout | PresentLevel
  deriving (Eq, Ord, Show)

-- | The possible scope content of an element
data ScopeContent name
  = -- | inline elements have no scope content
    InlineScopeContent
  | -- | layout elements contain everything in their layout scope
    LayoutScopeContent !(Seq (Node name))
  | -- | level elements contain everything in their level scope
    LevelScopeContent !(Seq (Node name))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | A newtype over a sequence of nodes whose semigroup instance will merge
-- adjacent 'LineSpace' and 'PlainText' nodes

-- TODO: make opaque? want to maintain invariant that line space and plain text
-- is always merged
newtype NodeSequence name = NodeSequence {unNodeSequence :: Seq (Node name)}
  deriving (Eq, Ord, Show)

singleNode :: Node name -> NodeSequence name
singleNode = NodeSequence . Seq.singleton

instance Semigroup (NodeSequence name) where
  NodeSequence x@(s :|> n) <> NodeSequence y@(m :<| t) = case (n, m) of
    (LineSpace a, LineSpace b) -> NodeSequence $ s <> Seq.singleton (LineSpace $ a + b) <> t
    (PlainText a, PlainText b) -> NodeSequence $ s <> Seq.singleton (PlainText $ a <> b) <> t
    _ -> NodeSequence $ x <> y
  NodeSequence x <> NodeSequence Empty = NodeSequence x
  NodeSequence Empty <> NodeSequence y = NodeSequence y

instance Monoid (NodeSequence name) where
  mempty = NodeSequence mempty

-- | Unsafely construct a 'MixedContent' sequence from 'Text'. This function
-- does not check that the 'Text' is free of syntactically-significant
-- characters.
text :: Text -> NodeSequence name
text t = singleNode $ PlainText t

-- | Unsafely construct a 'NodesBuilder' sequence from 'Text' consisting of
-- newlines and spaces
indent :: Located Text -> NodeSequence name
indent (Located _ t) = NodeSequence $ Seq.fromList chunks
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

escape :: Located Escape -> NodeSequence name
escape (Located _ e) = singleNode $ PlainText $ Tok.escapeText e

-- | Unsafely construct a 'NodesBuilder' sequence representing a run of single
-- spaces.
lineSpace :: Located Int -> NodeSequence name
lineSpace (Located _ n) = singleNode $ LineSpace n

inlineComment :: Located Text -> NodeSequence name
inlineComment (Located _ t) = singleNode $ InlineComment t

group :: Located Tok.Token -> NodeSequence name -> Located Tok.Token -> NodeSequence name
group startBrace ns endBrace =
  singleNode $
    GroupNode $
      Group (locatedSpan startBrace) (locatedSpan endBrace) (unNodeSequence ns)

elementNode :: Element name -> NodeSequence name
elementNode = singleNode . ElementNode

element :: Located EltName -> Attrs SrcName -> ScopeContent SrcName -> Element SrcName
element elname attrs =
  Element (locatedSpan elname) (srcName elname) attrs

srcName :: Located EltName -> SrcName
srcName (Located s n) = SrcName (SrcNamePos s) n

layoutScopeContent :: NodeSequence name -> ScopeContent name
layoutScopeContent = LayoutScopeContent . unNodeSequence

levelScopeContent :: NodeSequence name -> ScopeContent name
levelScopeContent = LevelScopeContent . unNodeSequence

singleAttr :: Attr name -> Seq (Attr name)
singleAttr = Seq.singleton

addAttr :: Seq (Attr name) -> Attr name -> Seq (Attr name)
addAttr = (:|>)

bracedAttrVal :: NodeSequence name -> AttrVal name
bracedAttrVal = BracedAttrVal . unNodeSequence

setAttrVal :: Seq (Attr name) -> (AttrVal name)
setAttrVal = SetAttrVal
