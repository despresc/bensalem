{-# LANGUAGE DeriveFunctor #-}
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
    Presentation (..),
    AttrMap (..),
    AttrKey,
    AttrVal (..),

    -- * Intermediate syntax builders
    NodeSequence (..),
    AttrsBuilder,
    Attr,
    text,
    lineSpace,
    blanks,
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
import Data.Map.Strict (Map)
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
    elementPresentation :: !Presentation,
    elementName :: !EltName,
    elementAttrs :: !(Maybe [Attr]),
    elementArg :: !(Maybe (Seq Node))
  }
  deriving (Eq, Ord, Show)

-- | A raw attribute
type Attr = (Located Text, AttrVal)

data AttrVal
  = BracedAttrVal !(Seq Node)
  | SetAttrVal ![Attr]
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

type AttrsBuilder = [Attr] -> [Attr]

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

-- | Unsafely construct a 'NodesBuilder' sequence representing a run of single
-- spaces.
lineSpace :: Located Int -> NodeSequence
lineSpace (Located _ n) = singleNode $ LineSpace n

inlineComment :: Located Text -> NodeSequence
inlineComment (Located _ t) = singleNode $ InlineComment t

elementNode :: Element -> NodeSequence
elementNode = singleNode . ElementNode

element :: Presentation -> Located EltName -> Maybe AttrsBuilder -> Maybe NodeSequence -> Element
element pres elname attrs content =
  Element (locatedSpan elname) pres (locatedVal elname) attrs' content'
  where
    attrs' = attrs <*> pure []
    content' = unNodeSequence <$> content

singleAttr :: Attr -> AttrsBuilder
singleAttr = (:)

addAttr :: AttrsBuilder -> Attr -> AttrsBuilder
addAttr x y = x . (y :)

bracedAttrVal :: NodeSequence -> AttrVal
bracedAttrVal = BracedAttrVal . unNodeSequence

setAttrVal :: AttrsBuilder -> AttrVal
setAttrVal = SetAttrVal . ($ [])

{-
inlineComment :: Located Text -> NodesBuilder
inlineComment (Located sp t) =
  Builder
    { builderMinCol = spanMinCol sp,
      builderVal = singleNode $ InlineComment t
    }
-}
{-
-- | Inline element builder
inlineElement ::
  Located EltName -> Maybe AttrsBuilder -> Maybe [Node] -> ElementBuilder
inlineElement nm =
  Builder
    { builderMinCol = n,
      builderVal = elt
    }
  where
    n = spanMinCol (locatedSpan nm)
    elt = Element (locatedSpan nm) (locatedVal nm) InlineScopeContent

levelElement :: Located EltName -> NodesBuilder -> ElementBuilder
levelElement nm nb =
  Builder
    { builderMinCol = n,
      builderVal = elt
    }
  where
    internalIndent = builderMinCol nb
    n = spanMinCol (locatedSpan nm) <> internalIndent
    elt =
      Element (locatedSpan nm) (locatedVal nm) $
        LevelScopeContent $
          mstripIndent internalIndent $
            stripAllEndSpace $
              unNodeSequence $
                builderVal nb

layoutElement :: Located EltName -> NodesBuilder -> ElementBuilder
layoutElement nm nb =
  Builder
    { builderMinCol = n,
      builderVal = elt
    }
  where
    internalIndent = builderMinCol nb
    n = spanMinCol (locatedSpan nm) <> internalIndent
    elt =
      Element (locatedSpan nm) (locatedVal nm) $
        LayoutScopeContent $
          mstripIndent internalIndent $
            stripEndBlank $
              unNodeSequence $
                builderVal nb

element :: ElementBuilder -> NodesBuilder
element (Builder n v) = Builder n $ singleNode $ ElementNode v

inlineVerbatimText :: Located Text -> NodesBuilder
inlineVerbatimText = text

verbatimBacktick :: SrcSpan -> NodesBuilder
verbatimBacktick sp =
  Builder
    { builderMinCol = spanMinCol sp,
      builderVal = singleNode $ PlainText "`"
    }

inlineVerbatim ::
  -- | span of the opening token
  SrcSpan ->
  -- | body of the inline verbatim
  NodesBuilder ->
  -- | span of the closing token
  SrcSpan ->
  NodesBuilder
inlineVerbatim sp1 nb sp2 =
  Builder
    { builderMinCol = spanMinCol sp1 <> spanMinCol sp2 <> builderMinCol nb,
      builderVal = NodeSequence nodes
    }
  where
    nodes = case stripEndBlank $ unNodeSequence $ builderVal nb of
      LineSpace _ :<| LineEnd :<| xs -> xs
      LineEnd :<| xs -> xs
      x -> x

data AttrBuilderEntry
  = AttrBuilderEntry !(Located EltName) !SrcSpan !AttrValBuilder

addAttr ::
  AttrsBuilder ->
  AttrValBuilder ->
  Maybe AttrsBuilder
addAttr (AttrBuilderEntry locKey assignSp val) msepSp ab =
  case M.alterF go (locatedVal locKey) $ builderVal ab of
    Just m ->
      Just $!
        Builder
          { builderMinCol = mincol,
            builderVal = m
          }
    Nothing -> Nothing
  where
    mincol =
      spanMinCol assignSp
        <> spanMinCol (locatedSpan locKey)
        <> builderMinCol val
        <> maybe NoMinCol spanMinCol msepSp
    go Nothing = Just $! Just $! locKey {locatedVal = builderVal val}
    go (Just _) = Nothing

-- | Record the brackets around the content of an 'AttrSet'
attrSet :: SrcSpan -> AttrBuilder -> SrcSpan -> AttrBuilder
attrSet = recordAround

-- attrSetNode :: SrcSpan -> AttrBuilder -> SrcSpan -> NodesBuilder
-- attrSetNode x a y = go <$> recordAround x a y
--   where
--     mapspan = x {srcSpanEnd = srcSpanEnd y}
--     go = singleNode . AttrMapNode mapspan . AttrMap

bracedAttrVal :: SrcSpan -> NodesBuilder -> SrcSpan -> AttrValBuilder
bracedAttrVal sp1 nb sp2 =
  Builder
    { builderMinCol = spanMinCol sp1 <> spanMinCol sp2 <> builderMinCol nb,
      builderVal = AttrValMarkup $ resolveBracedNodes nb
    }

openAttrVal :: NodesBuilder -> AttrValBuilder
openAttrVal nb = nb {builderVal = AttrValMarkup $ resolveUnbracedAttrVal nb}

attrSetVal :: AttrBuilder -> AttrValBuilder
attrSetVal nb = nb {builderVal = AttrValMap $ AttrMap $ builderVal nb}

-- | Create a 'MinCol' using the start column of the 'SrcSpan'. Assumes that the
-- span does in fact lie on a single line.
spanMinCol :: SrcSpan -> MinCol
spanMinCol = SomeMinCol . srcCol . srcSpanStart

-- | Record the minimum columns of the single-line tokens surrounding some content
recordAround :: SrcSpan -> Builder a -> SrcSpan -> Builder a
recordAround sp1 b sp2 =
  b {builderMinCol = spanMinCol sp1 <> spanMinCol sp2 <> builderMinCol b}

-- | Record the minimum column of a possibly-absent single-line token after some
-- content
recordTrailing :: Builder a -> Maybe SrcSpan -> Builder a
recordTrailing b Nothing = b
recordTrailing b (Just s) = b {builderMinCol = spanMinCol s <> builderMinCol b}

----------------------------------------------------------------
-- White space handling and resolving builders
----------------------------------------------------------------

-- | Strip a single trailing line from the sequence of nodes, if it exists
stripEndBlank :: Seq Node -> Seq Node
stripEndBlank (xs :|> LineEnd :|> LineSpace _) = xs
stripEndBlank (xs :|> LineEnd) = xs
stripEndBlank x = x

-- | Strip all trailing white space from the sequence of nodes
stripAllEndSpace :: Seq Node -> Seq Node
stripAllEndSpace (xs :|> LineSpace _) = stripAllEndSpace xs
stripAllEndSpace (xs :|> LineEnd) = stripAllEndSpace xs
stripAllEndSpace x = x

-- | Resolve a 'NodesBuilder' representing the body of a braced group by
-- stripping all relevant white space. In detail, we strip an optional trailing
-- blank line, an optional leading blank line, and all indentation in excess of
-- the 'builderMinCol'.
resolveBracedNodes :: NodesBuilder -> [Node]
resolveBracedNodes nb = toList $
  mstripIndent (builderMinCol nb) $
    case stripEndBlank $ unNodeSequence $ builderVal nb of
      LineSpace _ :<| LineEnd :<| xs -> xs
      LineEnd :<| xs -> xs
      x -> x

{- TODO: will want to rescue this for the intermediate -> full syntax conversion

-- | Resolve a 'NodesBuilder' representing the body argument of a level element
-- by stripping all relevant white space. In detail, we strip an optional
-- trailing blank line, all leading white space, and all indentation in excess
-- of the 'builderMinCol'.
resolveLevelNodes :: NodesBuilder -> [Node]
resolveLevelNodes nb =
  mstripIndent (builderMinCol nb) $
    stripBegin $ toList $ stripAllEndSpace $ unNodeSequence $ builderVal nb
  where
    stripBegin (LineSpace _ : x) = stripBegin x
    stripBegin (LineEnd : x) = stripBegin x
    stripBegin x = mstripIndent (builderMinCol nb) x

-- | Resolve a 'NodesBuilder' representing the body argument of a layout element
-- by stripping all relevant white space. In detail, we strip an optional
-- trailing blank line, optional leading white space up to and including the
-- first 'LineEnd', and all indentation in excess of the 'builderMinCol'.

-- TODO: not entirely sure that the stripEndBlank is necessary here
resolveLayoutNodes :: NodesBuilder -> [Node]
resolveLayoutNodes nb =
  mstripIndent (builderMinCol nb) $
    stripBegin $ toList $ stripEndBlank $ unNodeSequence $ builderVal nb
  where
    stripBegin (LineSpace _ : x) = stripBegin x
    stripBegin (LineEnd : x) = x
    stripBegin x = x
-}

-- | Resolve a 'NodesBuilder' representing an unbraced attribute value by
-- stripping all relevant white space. In detail, we strip all leading and
-- trailing white space, and all indentation.
resolveUnbracedAttrVal :: NodesBuilder -> [Node]
resolveUnbracedAttrVal nb =
  stripAllIndent $ stripBegin $ toList $ stripAllEndSpace $ unNodeSequence $ builderVal nb
  where
    stripBegin (LineSpace _ : x) = stripBegin x
    stripBegin (LineEnd : x) = stripBegin x
    stripBegin x = x

    stripAllIndent (LineEnd : LineSpace _ : xs) = LineEnd : stripAllIndent xs
    stripAllIndent (x : xs) = x : stripAllIndent xs
    stripAllIndent [] = []

-- | Strip the common indentation from a list of nodes, the indentation being
-- given by the minimum column among the nodes
stripIndent :: Int -> Seq Node -> Seq Node
stripIndent minCol = go
  where
    toStrip = minCol - 1
    go (LineEnd :<| LineSpace n :<| xs) = begin <> go xs
      where
        n' = n - toStrip
        begin
          | n' <= 0 = LineEnd :<| Empty
          | otherwise = LineEnd :<| LineSpace n' :<| Empty
    go (x :<| xs) = x :<| go xs
    go Empty = Empty

mstripIndent :: MinCol -> Seq Node -> Seq Node
mstripIndent (SomeMinCol n) = stripIndent n
mstripIndent NoMinCol = id
-}
