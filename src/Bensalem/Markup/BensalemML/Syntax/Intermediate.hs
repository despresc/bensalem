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
    ScopeContent (..),
    AttrMap (..),
    AttrKey,
    AttrVal (..),

    -- * Intermediate syntax builders
    Builder (..),
    MinCol (..),
    NodesBuilder,
    NodesCombine (..),
    text,
    insigLit,
    literalAt,
    lineSpace,
    blanks,
    inlineComment,
    bracedGroup,
    layoutBracedGroup,
    inlineVerbatim,
    inlineVerbatimText,
    verbatimBacktick,
    ElementBuilder,
    element,
    inlineElement,
    levelElement,
    layoutElement,
    AttrBuilder,
    AttrBuilderEntry (..),
    AttrValBuilder,
    initAttrBuilder,
    addToAttrBuilderM,
    attrSetNode,
    attrSet,
    recordTrailing,
    resolveAttrKey,
    bracedAttrVal,
    openAttrVal,
    attrSetVal,
  )
where

import Bensalem.Markup.BensalemML.ParserDefs
  ( Located (..),
    Parser,
    SrcPos (..),
    SrcSpan (..),
    throwParseErrorNil,
  )
import Bensalem.Markup.BensalemML.Token (EltName)
import qualified Bensalem.Markup.BensalemML.Token as Tok
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T

{-

may be worthwhile having an intermediate node type with lazy text in the plain
text and, say, decorated with minimum columns so that we can have a single
[IntermediateNode] -> [Node] pass at the end that resolves all of that.

may also be worthwhile to have the lexer keep track of the minimum columns along
with the scopes so that information can be reported in the closing token. would
probably save a decent amount of computation.

may want to change parsing model so that tokens keep track of their trailing
whitespace. could simplify the happy grammar.
-}

-- | A single node in bensalem syntax
data Node
  = -- | normal text not containing line space or newlines
    PlainText !Text
  | -- | a run of single space characters
    LineSpace !Int
  | -- | a line ending
    LineEnd
  | BracedGroup !SrcSpan ![Node]
  | LayoutBracedGroup !SrcSpan ![Node]
  | AttrMapNode !SrcSpan !AttrMap
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
    elementScopeContent :: !ScopeContent
  }
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

validAttrKey :: Text -> Maybe AttrKey
validAttrKey = Tok.validEltName

resolveAttrKey :: Located Text -> Parser (Located AttrKey)
resolveAttrKey (Located sp t) = case validAttrKey t of
  Just e -> pure $ Located sp e
  Nothing -> throwParseErrorNil

-- | A builder for some value @a@, also keeping track of the minimum column
-- achieved in whatever source span it occupies
data Builder a = Builder
  { builderMinCol :: !MinCol,
    builderVal :: !a
  }
  deriving (Eq, Ord, Show, Functor)

-- | A minimum column value. This is allowed to be 'NoMinCol' to accomodate
-- certain occurrences of white space that have no defined columns
data MinCol
  = SomeMinCol !Int
  | NoMinCol
  deriving (Eq, Ord, Show)

-- | take the minimum value, treating 'NoMinCol' as positive infinity
instance Semigroup MinCol where
  SomeMinCol n <> SomeMinCol m = SomeMinCol $ min n m
  SomeMinCol n <> NoMinCol = SomeMinCol n
  NoMinCol <> SomeMinCol m = SomeMinCol m
  NoMinCol <> NoMinCol = NoMinCol

-- | A newtype over a sequence of nodes whose semigroup instance will merge
-- adjacent 'LineSpace' and 'PlainText' nodes

-- TODO: make opaque? want to maintain invariant that line space and plain text
-- is always merged
newtype NodesCombine = NodesCombine {unNodesCombine :: Seq Node}
  deriving (Eq, Ord, Show)

singleNode :: Node -> NodesCombine
singleNode = NodesCombine . Seq.singleton

instance Semigroup NodesCombine where
  NodesCombine x@(s :|> n) <> NodesCombine y@(m :<| t) = case (n, m) of
    (LineSpace a, LineSpace b) -> NodesCombine $ s <> Seq.singleton (LineSpace $ a + b) <> t
    (PlainText a, PlainText b) -> NodesCombine $ s <> Seq.singleton (PlainText $ a <> b) <> t
    _ -> NodesCombine $ x <> y
  NodesCombine x <> NodesCombine Empty = NodesCombine x
  NodesCombine Empty <> NodesCombine y = NodesCombine y

instance Monoid NodesCombine where
  mempty = NodesCombine mempty

-- | take the minimum of the two minimum columns and also merge the builder
-- values
instance Semigroup a => Semigroup (Builder a) where
  Builder ind1 a <> Builder ind2 b = Builder (ind1 <> ind2) (a <> b)

instance Monoid a => Monoid (Builder a) where
  mempty = Builder NoMinCol mempty

type NodesBuilder = Builder NodesCombine

-- | An attribute set builder
type AttrBuilder = Builder (Map AttrKey (Located AttrVal))

-- | An element builder
type ElementBuilder = Builder Element

-- | An attribute value builder
type AttrValBuilder = Builder AttrVal

-- | Unsafely construct a 'MixedContent' sequence from 'Text' located at a
-- particular 'SrcSpan'. This function does not check that the 'Text' is free of
-- syntactically-significant characters, or that the given 'SrcSpan' occupies a
-- single line.
text :: Located Text -> NodesBuilder
text t =
  Builder
    { builderMinCol = spanMinCol $ locatedSpan t,
      builderVal = singleNode $ PlainText $ locatedVal t
    }

-- | Unsafely construct a 'NodesBuilder' sequence from 'Text' consisting of
-- newlines and spaces
blanks :: Located Text -> NodesBuilder
blanks (Located _ t) =
  Builder
    { builderMinCol = NoMinCol,
      builderVal = NodesCombine $ Seq.fromList chunks
    }
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
lineSpace :: Located Int -> NodesBuilder
lineSpace (Located _ n) =
  Builder
    { builderMinCol = NoMinCol,
      builderVal = singleNode $ LineSpace n
    }

bracedGroup :: SrcSpan -> NodesBuilder -> SrcSpan -> NodesBuilder
bracedGroup sp1 nb sp2 =
  Builder
    { builderMinCol = spanMinCol sp1 <> spanMinCol sp2 <> builderMinCol nb,
      builderVal = singleNode $ BracedGroup sp $ resolveBracedNodes nb
    }
  where
    sp = sp1 {srcSpanEnd = srcSpanEnd sp2}

layoutBracedGroup :: SrcSpan -> NodesBuilder -> NodesBuilder
layoutBracedGroup sp nb =
  Builder
    { builderMinCol = spanMinCol sp <> internalIndent,
      builderVal =
        singleNode $
          LayoutBracedGroup sp $
            toList $
              mstripIndent internalIndent $
                stripEndBlank $
                  unNodesCombine $
                    builderVal nb
    }
  where
    internalIndent = builderMinCol nb

inlineComment :: Located Text -> NodesBuilder
inlineComment (Located sp t) =
  Builder
    { builderMinCol = spanMinCol sp,
      builderVal = singleNode $ InlineComment t
    }

-- | Inline element builder
inlineElement :: Located EltName -> ElementBuilder
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
              unNodesCombine $
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
              unNodesCombine $
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
      builderVal = NodesCombine nodes
    }
  where
    nodes = case stripEndBlank $ unNodesCombine $ builderVal nb of
      LineSpace _ :<| LineEnd :<| xs -> xs
      LineEnd :<| xs -> xs
      x -> x

-- TODO: can probably just pass in the token and re-render it for the PlainText
insigLit :: Text -> SrcSpan -> NodesBuilder
insigLit t sp =
  Builder
    { builderMinCol = spanMinCol sp,
      builderVal = singleNode $ PlainText t
    }

-- TODO: obviously just insigLit
literalAt :: SrcSpan -> NodesBuilder
literalAt sp =
  Builder
    { builderMinCol = spanMinCol sp,
      builderVal = singleNode $ PlainText "@"
    }

data AttrBuilderEntry
  = AttrBuilderEntry !(Located EltName) !SrcSpan !AttrValBuilder

-- | Add a key-value pair to an 'AttrBuilder', also taking in the 'SrcSpan' of
-- the @=@ assignment and the optional comma separator. Fails if the given key
-- is already assigned in the 'AttrBuilder'.
addToAttrBuilder ::
  AttrBuilderEntry ->
  Maybe SrcSpan ->
  AttrBuilder ->
  Maybe AttrBuilder
addToAttrBuilder (AttrBuilderEntry locKey assignSp val) msepSp ab =
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

-- TODO: I think the Maybe SrcSpan here can be eliminated entirely, as that was
-- formerly the srcspan of the comma separator, I'm pretty sure
addToAttrBuilderM :: AttrBuilderEntry -> Maybe SrcSpan -> AttrBuilder -> Parser AttrBuilder
addToAttrBuilderM x y z = case addToAttrBuilder x y z of
  Just m -> pure m
  Nothing -> throwParseErrorNil

initAttrBuilder :: AttrBuilderEntry -> AttrBuilder
initAttrBuilder (AttrBuilderEntry locKey assignSp val) =
  Builder
    { builderMinCol = mincol,
      builderVal = M.singleton (locatedVal locKey) val'
    }
  where
    mincol =
      spanMinCol assignSp
        <> spanMinCol (locatedSpan locKey)
        <> builderMinCol val
    val' = locKey {locatedVal = builderVal val}

-- | Record the brackets around the content of an 'AttrSet'
attrSet :: SrcSpan -> AttrBuilder -> SrcSpan -> AttrBuilder
attrSet = recordAround

attrSetNode :: SrcSpan -> AttrBuilder -> SrcSpan -> NodesBuilder
attrSetNode x a y = go <$> recordAround x a y
  where
    mapspan = x {srcSpanEnd = srcSpanEnd y}
    go = singleNode . AttrMapNode mapspan . AttrMap

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
    case stripEndBlank $ unNodesCombine $ builderVal nb of
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
    stripBegin $ toList $ stripAllEndSpace $ unNodesCombine $ builderVal nb
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
    stripBegin $ toList $ stripEndBlank $ unNodesCombine $ builderVal nb
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
  stripAllIndent $ stripBegin $ toList $ stripAllEndSpace $ unNodesCombine $ builderVal nb
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
