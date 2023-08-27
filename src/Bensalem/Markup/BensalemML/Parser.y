{
-- |
-- Description : Bensalem document parser
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- The main document parser

module Bensalem.Markup.BensalemML.Parser
  (parseNodes) where

import Bensalem.Markup.BensalemML.Syntax.Intermediate
import Bensalem.Markup.BensalemML.ParserDefs
  (Located(..),
   Parser,
   ParseError(..),
   SrcSpan(..),
   throwParseError)
import qualified Bensalem.Markup.BensalemML.Token as Tok
import Bensalem.Markup.BensalemML.Lexer (lexToken)

import Data.Foldable (toList)
import Control.Monad.Except (throwError)
import Data.Text (Text)
}

{- TODO:

should I forbid level elements in open attribute value content (or layout
elements, for that matter)?

-}

%expect 0 -- shift/reduce conflicts

%name pManyNodes MixedContent
%tokentype { Located Tok.Token }
%monad { Parser }
%error { (\k -> throwParseError (Just k)) }
%lexer { lexer } { Located _ Tok.TokenEOF }

%token
  text { Located _ (Tok.PlainText _) }
  equals { Located _ Tok.Equals }
  comma { Located _ Tok.Comma }
  esc { Located _ (Tok.Escape _) }
  blanks { Located _ (Tok.Blanks _) }
  lineSpace { Located _ (Tok.LineSpace _) }
  inlineComment { Located _ (Tok.LineComment _) }
  inlineTag { Located _ (Tok.InlineTag _) }
  levelTag { Located _ (Tok.LevelTag _ _) }
  layoutTag { Located _ (Tok.LayoutTag _) }
  startInlineVerb { Located _ Tok.StartInlineVerbatim }
  inlineVerbatimText { Located _ (Tok.InlineVerbatimText _) }
  verbatimBacktick { Located _ Tok.VerbatimBacktick }
  endInlineVerb { Located _ Tok.EndInlineVerbatim }
  startBrace { Located _ Tok.StartBraceGroup }
  endBrace { Located _ Tok.EndBraceGroup }
  startAttr { Located _ Tok.StartAttrSet }
  endAttr { Located _ Tok.EndAttrSet }
  endImplicitScope { Located _ Tok.EndImplicitScope }

%%

-- a top-level node
MixedContentNode :: { NodesBuilder }
  : text { text (getPlainText $1) }
  | comma { insigComma (locatedSpan $1) }
  | equals { insigEquals (locatedSpan $1) }
  | esc { escapeSequence (getEscape $1) }
  | lineSpace { lineSpace (getLineSpace $1) }
  | blanks { blanks (getBlanks $1) }
  | inlineComment { inlineComment (getInlineComment $1) }
  | BracedGroup { $1 }
  | ElementNode { $1 }
  | VerbatimNode { $1 }
  | AttrSetNode { $1 }

MixedContent :: { NodesBuilder }
  : {- empty -} { mempty }
  | MixedContent1 { $1 }

MixedContent1 :: { NodesBuilder }
  : MixedContentNode { $1 }
  | MixedContent1 MixedContentNode { $1 <> $2 }

BracedGroup :: { NodesBuilder }
BracedGroup
  : startBrace MixedContent endBrace { bracedGroup (locatedSpan $1) $2 (locatedSpan $3) }

ElementNode :: { NodesBuilder }
  : InlineElement { element $1 }
  | LevelElement { element $1 }
  | LayoutElement { element $1 }

InlineElement :: { ElementBuilder }
  : inlineTag { inlineElement (getInlineTag $1) }

LevelElement :: { ElementBuilder }
  : levelTag MixedContent endImplicitScope { levelElement (getLevelTag $1) $2 }

LayoutElement :: { ElementBuilder }
  : layoutTag MixedContent endImplicitScope { layoutElement (getLayoutTag $1) $2 }

AttrSetNode :: { NodesBuilder }
  : startAttr Spaces AttrContent endAttr { attrSetNode (locatedSpan $1) $3 (locatedSpan $4) }

AttrSet :: { AttrBuilder }
  : startAttr Spaces AttrContent endAttr { attrSet (locatedSpan $1) $3 (locatedSpan $4) }

AttrContent :: { AttrBuilder }
  : {- empty -} { mempty }
  | AttrEntries1 AttrSepTrailing { recordTrailing $1 $2 }

AttrEntries1 :: { AttrBuilder }
  : AttrEntry { initAttrBuilder $1 }
  | AttrEntries1 AttrSep AttrEntry {% addToAttrBuilderM $3 (Just $2) $1 }

AttrEntry :: { AttrBuilderEntry }
  : AttrKey Spaces equals Spaces AttrVal { AttrBuilderEntry $1 (locatedSpan $3) $5 }

AttrKey :: { Located AttrKey }
  : text {% resolveAttrKey (getPlainText $1) }

AttrVal :: { AttrValBuilder }
  : startBrace MixedContent endBrace Spaces { bracedAttrVal (locatedSpan $1) $2 (locatedSpan $3) }
  | FirstOpenAttrNode OpenAttrContent { openAttrVal ($1 <> $2) }
  | AttrSet Spaces { attrSetVal $1 }

-- The first node in an open attribute list cannot be a braced group, attribute
--set, or space, to avoid ambiguity.
FirstOpenAttrNode :: { NodesBuilder }
  : text { text (getPlainText $1) }
  | comma { insigComma (locatedSpan $1) }
  | equals { insigEquals (locatedSpan $1) }
  | esc { escapeSequence (getEscape $1) }
  | ElementNode { $1 }
  | VerbatimNode { $1 }

OpenAttrContent :: { NodesBuilder }
OpenAttrContent
  : {- empty -} { mempty }
  | OpenAttrContent1 { $1 }

OpenAttrContent1 :: { NodesBuilder }
  : OpenAttrNode { $1 }
  | OpenAttrContent1 OpenAttrNode { $1 <> $2 }

-- like MixedContentNode, but without equals or comma. we parse all space as
-- being in the attribute value for convenience, since we can simply strip it
-- out later
OpenAttrNode :: { NodesBuilder }
  : text { text (getPlainText $1) }
  | esc { escapeSequence (getEscape $1) }
  | blanks { blanks (getBlanks $1) }
  | inlineComment { inlineComment (getInlineComment $1) }
  | lineSpace { lineSpace (getLineSpace $1) }
  | ElementNode { $1 }
  | VerbatimNode { $1 }
  | AttrSetNode { $1 }

Space :: { () }
  : lineSpace { () }
  | blanks { () }
  | inlineComment { () }

Spaces :: { () }
  : {- empty -} { () }
  | Spaces1 { () }

Spaces1 :: { () }
  : Space { () }
  | Spaces1 Space { () }

AttrSep :: { SrcSpan }
  : comma Spaces { locatedSpan $1 }

AttrSepTrailing :: { Maybe SrcSpan }
  : {- empty -} { Nothing }
  | AttrSep { Just $1 }

VerbatimNode :: { NodesBuilder }
  : startInlineVerb InlineVerbatimContent endInlineVerb
      { inlineVerbatim (locatedSpan $1) $2 (locatedSpan $3)}

InlineVerbatimContent :: { NodesBuilder }
  : {- empty -} { mempty }
  | InlineVerbatimContent1 { $1 }

InlineVerbatimContent1 :: { NodesBuilder }
InlineVerbatimContent1
  : InlineVerbatimNode { $1 }
  | InlineVerbatimContent1 InlineVerbatimNode { $1 <> $2 }

InlineVerbatimNode :: { NodesBuilder }
InlineVerbatimNode
  : inlineVerbatimText { inlineVerbatimText (getInlineVerbatimText $1) }
  | verbatimBacktick { verbatimBacktick (locatedSpan $1) }
  | blanks { blanks (getBlanks $1) }

{
lexer :: (Located Tok.Token -> Parser a) -> Parser a
lexer = (lexToken >>=)

-- | Parse a sequence of intermediate bensalem nodes at the top level
parseNodes :: Parser [Node]
parseNodes = toList . unNodesCombine . builderVal <$> pManyNodes

----------------------------------------------------------------
-- boring partial functions to extract information from tokens
----------------------------------------------------------------

-- | Partial function that matches a plain text token
getPlainText :: Located Tok.Token -> Located Text
getPlainText (Located x (Tok.PlainText t)) = Located x t
getPlainText _ = error "Internal error"

-- | Partial function that matches an escape sequence
getEscape :: Located Tok.Token -> Located Tok.EscSequence
getEscape (Located x (Tok.Escape esc)) = Located x esc
getEscape _ = error "Internal error"

-- | Partial function that matches an escape sequence
getLineSpace :: Located Tok.Token -> Located Int
getLineSpace (Located x (Tok.LineSpace n)) = Located x n
getLineSpace _ = error "Internal error"

-- | Partial function that matches an escape sequence
getBlanks :: Located Tok.Token -> Located Text
getBlanks (Located x (Tok.Blanks t)) = Located x t
getBlanks _ = error "Internal error"

-- | Partial function that matches an inline tag
getInlineTag :: Located Tok.Token -> Located Tok.EltName
getInlineTag (Located x (Tok.InlineTag e)) = Located x e
getInlineTag _ = error "Internal error"

-- | Partial function that matches an escape sequence
getInlineVerbatimText :: Located Tok.Token -> Located Text
getInlineVerbatimText (Located x (Tok.InlineVerbatimText t)) = Located x t
getInlineVerbatimText _ = error "Internal error"

-- | Partial function that matches an inline comment
getInlineComment :: Located Tok.Token -> Located Text
getInlineComment (Located x (Tok.LineComment t)) = Located x t
getInlineComment _ = error "Internal error"

-- | Partial function that matches an inline comment
getLevelTag :: Located Tok.Token -> Located Text
getLevelTag (Located x (Tok.LevelTag _ t)) = Located x t
getLevelTag _ = error "Internal error"

-- | Partial function that matches an inline comment
getLayoutTag :: Located Tok.Token -> Located Text
getLayoutTag (Located x (Tok.LayoutTag t)) = Located x t
getLayoutTag _ = error "Internal error"

}
