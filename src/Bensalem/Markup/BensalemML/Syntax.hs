{-# LANGUAGE BangPatterns #-}

-- |
-- Description : Bensalem document syntax
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
    SI.Presentation (..),
    AttrKey,
    AttrVal (..),

    -- * Parsing
    parseNodes,
  )
where

import qualified Bensalem.Markup.BensalemML.Parser as SP
import Bensalem.Markup.BensalemML.ParserDefs
  ( Located (..),
    ParseError (..),
    SrcSpan (..),
    evalParser,
    initAlexInput,
  )
import qualified Bensalem.Markup.BensalemML.Syntax.Intermediate as SI
import Bensalem.Markup.BensalemML.Token (EltName)
import Data.Sequence (Seq (..))
import Data.Text (Text)

-- | Parse a sequence of bensalem nodes from the given input. These nodes are
-- assumed to be in the outermost scope of a document (in particular, not in an
-- indentation or level scope).
parseNodesTW ::
  -- | tab width (for error location reporting only)
  Int ->
  -- | input name
  Text ->
  -- | input
  Text ->
  Either ParseError (Seq Node)
parseNodesTW tw nm inp = do
  nodes <- evalParser SP.parseNodes $ initAlexInput tw nm inp
  fromIntermediateNodes nodes

-- | Parse a sequence of bensalem nodes from the given input with
-- 'parseNodesTW', with a default tab width of 8
parseNodes ::
  -- | input name
  Text ->
  -- | input
  Text ->
  Either ParseError (Seq Node)
parseNodes = parseNodesTW 8

-- | A single node in bensalem syntax
data Node
  = -- | normal text not containing line space or newlines
    PlainText !Text
  | -- | a run of single space characters
    LineSpace !Int
  | -- | a line ending
    LineEnd
  | ElementNode !Element
  deriving (Eq, Ord, Show)

-- | An element tag. Note that we only save the position of the element tag
-- itself. We also include all of the scope content of an element, which for
-- level and layout elements includes the attribute map, any arguments, as well
-- as the body argument content. Note that the scope content of an inline
-- element is always empty.
data Element = Element
  { elementTagPos :: !SrcSpan,
    elementArgPresentation :: !SI.Presentation,
    elementName :: !EltName,
    elementAttrs :: !(Seq Attr),
    elementArg :: !(Seq Node)
  }
  deriving (Eq, Ord, Show)

data ElementArg = ElementArg
  { elementArgTy :: ElementArgTy,
    elementArgContent :: Seq Node
  }
  deriving (Eq, Ord, Show)

data ElementArgTy
  = ElementArgBraced SrcSpan
  | ElementArgLayout SrcSpan
  | ElementArgBody
  deriving (Eq, Ord, Show)

-- TODO: opaqueness and checking of the AttrKey
data Attr = Attr !SrcSpan !AttrKey !AttrVal
  deriving (Eq, Ord, Show)

type AttrKey = Text

-- | An attribute value
data AttrVal
  = AttrValMarkup !(Seq Node)
  | AttrValSet !(Seq Attr)
  deriving (Eq, Ord, Show)

-- TODO: need to add in the checking and the indentation stripping!
fromIntermediateNodes :: Seq SI.Node -> Either ParseError (Seq Node)
fromIntermediateNodes = go mempty
  where
    go !acc (SI.PlainText t :<| xs) = go (acc :|> PlainText t) xs
    go !acc (SI.LineSpace n :<| xs) = go (acc :|> LineSpace n) xs
    go !acc (SI.LineEnd :<| xs) = go (acc :|> LineEnd) xs
    go !acc (SI.InlineComment _ :<| xs) = go acc xs
    go !acc (SI.ElementNode e :<| xs) = do
      attrs' <- convertAttrs $ SI.elementAttrs e
      content' <- convertContent $ SI.elementArg e
      let elt =
            Element
              (SI.elementTagPos e)
              (SI.elementPresentation e)
              (SI.elementName e)
              attrs'
              content'
      go (acc :|> ElementNode elt) xs
    go !acc Empty = pure acc

    convertAttrs :: SI.Attrs -> Either ParseError (Seq Attr)
    convertAttrs SI.NoAttrs = pure mempty
    convertAttrs (SI.Attrs x) = traverse convertAttr x

    convertAttr (locT, v) = case v of
      SI.BracedAttrVal nodes -> do
        nodes' <- fromIntermediateNodes nodes
        pure $ Attr (locatedSpan locT) (locatedVal locT) $ AttrValMarkup nodes'
      SI.SetAttrVal attrs -> do
        attrs' <- convertAttrs $ SI.Attrs attrs
        pure $ Attr (locatedSpan locT) (locatedVal locT) $ AttrValSet attrs'

    convertContent SI.NoArg = pure mempty
    convertContent (SI.Arg x) = fromIntermediateNodes x
