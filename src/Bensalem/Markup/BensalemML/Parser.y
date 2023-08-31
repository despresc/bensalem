{
{-# LANGUAGE OverloadedStrings #-}

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

%expect 0 -- shift/reduce conflicts

%name pManyNodes MixedContent
%tokentype { Located Tok.Token }
%monad { Parser }
%error { (\k -> throwParseError (Just k)) }
%lexer { lexer } { Located _ Tok.TokenEOF }

%token
  text { Located _ (Tok.PlainText _) }
  equals { Located _ Tok.Equals }
  blanks { Located _ (Tok.Blanks _) }
  lineSpace { Located _ (Tok.LineSpace _) }
  inlineComment { Located _ (Tok.LineComment _) }
  literalAt { Located _ Tok.LiteralAt }
  inlineTag { Located _ (Tok.InlineTag _) }
  levelTag { Located _ (Tok.LevelTag _ _) }
  layoutTag { Located _ (Tok.LayoutTag _) }
  startBrace { Located _ Tok.StartBraceGroup }
  endBrace { Located _ Tok.EndBraceGroup }
  startAttr { Located _ Tok.StartAttrSet }
  endAttr { Located _ Tok.EndAttrSet }
  endImplicitScope { Located _ Tok.EndImplicitScope }

%%

-- a top-level node
MixedContentNode :: { NodesBuilder }
  : text { text (getPlainText $1) }
  | equals { insigLit "=" (locatedSpan $1) }
  | startAttr { insigLit "[" (locatedSpan $1) }
  | endAttr { insigLit "]" (locatedSpan $1) }
  | literalAt { literalAt (locatedSpan $1) }
  | lineSpace { lineSpace (getLineSpace $1) }
  | blanks { blanks (getBlanks $1) }
  | InsigBracedGroup { $1 }
  | ElementNode { $1 }
  | inlineComment { inlineComment (getInlineComment $1) }

MixedContent :: { NodesBuilder }
  : {- empty -} { mempty }
  | MixedContent1 { $1 }

MixedContent1 :: { NodesBuilder }
  : MixedContentNode { $1 }
  | MixedContent1 MixedContentNode { $1 <> $2 }

InsigBracedGroup :: { NodesBuilder }
InsigBracedGroup
  : startBrace MixedContent endBrace { mconcat
                                         [ insigLit "{" (locatedSpan $1),
                                           $2,
                                           insigLit "}" (locatedSpan $3)] }

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
  : Spaces { mempty }
  | AttrEntries1 Spaces { $1 }

AttrEntries1 :: { AttrBuilder }
  : AttrEntry { initAttrBuilder $1 }
  | AttrEntries1 Spaces AttrEntry {% addToAttrBuilderM $3 Nothing $1 }

AttrEntry :: { AttrBuilderEntry }
  : AttrKey Spaces equals Spaces AttrVal { AttrBuilderEntry $1 (locatedSpan $3) $5 }

AttrKey :: { Located AttrKey }
  : text {% resolveAttrKey (getPlainText $1) }

AttrVal :: { AttrValBuilder }
  : startBrace MixedContent endBrace Spaces { bracedAttrVal (locatedSpan $1) $2 (locatedSpan $3) }
  | AttrSet Spaces { attrSetVal $1 }

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
