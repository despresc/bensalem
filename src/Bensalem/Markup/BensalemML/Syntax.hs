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
    AttrMap (..),
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
import Data.Foldable (toList)
import Data.Map.Strict (Map)
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
  Either ParseError [Node]
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
  Either ParseError [Node]
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
    elementName :: !EltName,
    elementAttrs :: Maybe (Located AttrMap),
    elementArgs :: [ElementArg]
  }
  deriving (Eq, Ord, Show)

data ElementArg = ElementArg
  { elementArgTy :: ElementArgTy,
    elementArgContent :: [Node]
  }
  deriving (Eq, Ord, Show)

data ElementArgTy
  = ElementArgBraced SrcSpan
  | ElementArgLayout SrcSpan
  | ElementArgBody
  deriving (Eq, Ord, Show)

-- | An attribute value
data AttrVal
  = AttrValMarkup [Node]
  | AttrValMap AttrMap
  deriving (Eq, Ord, Show)

-- | An attribute map that also stores the positions of the /keys/ of the map

-- n.b. this representation may have to change if we allow attribtues like
-- [x.y.z=something]. we could always just have the SrcSpan of the entire
-- sequence stored in every intermediate, I suppose, but we'd have to be careful
-- when reporting errors and such that we recognize that that may happen and not
-- duplicate information.
newtype AttrMap = AttrMap
  { unAttrMap :: Map AttrKey (Located AttrVal)
  }
  deriving (Eq, Ord, Show)

-- | An attribute key is a non-empty string of alphanumeric characters. This
-- currently matches the definition of element names.
type AttrKey = Text

-- | Convert a sequence of intermediate nodes to full syntax nodes, possibly
-- failing if there are incorrectly positioned braced groups or attribute sets.
-- This function assumes that indentation and /trailing/ content white space has
-- been stripped as necessary, but does not assume that initial content white
-- space has been stripped.
--
-- The syntax may be relaxed in future to allow for braced groups and attribute
-- sets to appear apart from elements, in which case this function will no
-- longer fail.
fromIntermediateNodes :: [SI.Node] -> Either ParseError [Node]
fromIntermediateNodes = go id
  where
    go acc (SI.PlainText t : xs) = go (acc . (PlainText t :)) xs
    go acc (SI.LineSpace n : xs) = go (acc . (LineSpace n :)) xs
    go acc (SI.LineEnd : xs) = go (acc . (LineEnd :)) xs
    go acc (SI.InlineComment _ : xs) = go acc xs
    go acc (SI.ElementNode e : xs) =
      let con = Element (SI.elementTagPos e) (SI.elementName e)
       in case SI.elementScopeContent e of
            SI.InlineScopeContent -> do
              (elt, xs') <- handleInlineElement con xs
              go (acc . (elt :)) xs'
            SI.LayoutScopeContent content -> do
              elt <- handleLayoutElement con (toList content)
              go (acc . (elt :)) xs
            SI.LevelScopeContent content -> do
              elt <- handleLevelElement con (toList content)
              go (acc . (elt :)) xs
    go _ (SI.BracedGroup _ _ : _) = Left $ ParserError Nothing
    go _ (SI.LayoutBracedGroup _ _ : _) = Left $ ParserError Nothing
    go _ (SI.AttrMapNode _ _ : _) = Left $ ParserError Nothing
    go acc [] = pure $ acc []

    fromAttrMap (SI.AttrMap m) = AttrMap <$> traverse fromAttrVal m

    fromAttrVal (Located sp (SI.AttrValMarkup n)) = do
      n' <- fromIntermediateNodes n
      pure $ Located sp $ AttrValMarkup n'
    fromAttrVal (Located sp (SI.AttrValMap m)) = do
      m' <- fromAttrMap m
      pure $ Located sp $ AttrValMap m'

    gatherWhiteSpace acc (SI.LineSpace n : xs) = gatherWhiteSpace (acc . (LineSpace n :)) xs
    gatherWhiteSpace acc (SI.LineEnd : xs) = gatherWhiteSpace (acc . (LineEnd :)) xs
    gatherWhiteSpace acc xs = (acc, xs)

    resolveElement con xs = case gatherWhiteSpace id xs of
      (_, SI.AttrMapNode sp am : xs') -> do
        am' <- fromAttrMap am
        resolveArgs (con $ Just $ Located sp am') xs'
      (_, SI.BracedGroup sp n : xs') -> do
        n' <- fromIntermediateNodes n
        resolveArgs (con Nothing . (ElementArg (ElementArgBraced sp) n' :)) xs'
      (_, SI.LayoutBracedGroup sp n : xs') -> do
        n' <- fromIntermediateNodes n
        resolveArgs (con Nothing . (ElementArg (ElementArgLayout sp) n' :)) xs'
      _ -> pure (con Nothing, xs)

    resolveArgs con xs = case gatherWhiteSpace id xs of
      (_, SI.BracedGroup sp n : xs') -> do
        n' <- fromIntermediateNodes n
        resolveArgs (con . (ElementArg (ElementArgBraced sp) n' :)) xs'
      (_, SI.LayoutBracedGroup sp n : xs') -> do
        n' <- fromIntermediateNodes n
        resolveArgs (con . (ElementArg (ElementArgLayout sp) n' :)) xs'
      _ -> pure (con, xs)

    handleInlineElement con stream = do
      (elt, stream') <- resolveElement con stream
      pure (ElementNode $ elt [], stream')
    handleLayoutElement con content = do
      (elt, content') <- resolveElement con content
      let content'' = stripLayoutBegin content'
      contentN <- fromIntermediateNodes content''
      pure $ ElementNode $ elt [ElementArg ElementArgBody contentN]
    handleLevelElement con content = do
      (elt, content') <- resolveElement con content
      let content'' = stripLevelBegin content'
      contentN <- fromIntermediateNodes content''
      pure $ ElementNode $ elt [ElementArg ElementArgBody contentN]

    stripLayoutBegin (SI.LineSpace _ : x) = stripLayoutBegin x
    stripLayoutBegin (SI.LineEnd : x) = x
    stripLayoutBegin x = x

    stripLevelBegin (SI.LineSpace _ : x) = stripLevelBegin x
    stripLevelBegin (SI.LineEnd : x) = stripLevelBegin x
    stripLevelBegin x = x
