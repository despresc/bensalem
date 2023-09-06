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
import Data.Sequence (Seq)
}

%expect 1 -- shift/reduce conflicts
{- A note on the shift/reduce conflicts

The 1 conflict here is due to the fact that '{' immediately after a tag could be
parsed either as the start of an argument, or as insignificant text. The shift
(which happy chooses) represents the former and is correct.

-}

%name pManyNodes MixedContent
%tokentype { Located Tok.Token }
%monad { Parser }
%error { (\k -> throwParseError (Just k)) }
%lexer { lexer } { Located _ Tok.TokenEOF }

%token
  text { Located _ (Tok.PlainText _) }
  blanks { Located _ (Tok.Blanks _) }
  lineSpace { Located _ (Tok.LineSpace _) }
  inlineComment { Located _ (Tok.LineComment _) }
  inlineTag { Located _ (Tok.InlineTag _) }
  escapeSequence { Located _ (Tok.EscapeSeq _) }
  startBrace { Located _ Tok.StartBraceGroup }
  endBrace { Located _ Tok.EndBraceGroup }
  startAttrSet { Located _ Tok.StartAttrSet }
  endAttrSet { Located _ Tok.EndAttrSet }
  equals { Located _ Tok.Equals }
  attrKey { Located _ (Tok.AttrKey _) }

%%

-- a top-level node
MixedContentNode :: { NodeSequence }
  : text { text (locatedVal $ getPlainText $1) }
  | escapeSequence { escapeSequence (getEscapeSequence $1) }
  | lineSpace { lineSpace (getLineSpace $1) }
  | blanks { blanks (getBlanks $1) }
  | InsigBracedGroup { $1 }
  | ElementNode { $1 }
  | inlineComment { inlineComment (getInlineComment $1) }

MixedContent :: { NodeSequence }
  : {- empty -} { mempty }
  | MixedContent1 { $1 }

MixedContent1 :: { NodeSequence }
  : MixedContentNode { $1 }
  | MixedContent1 MixedContentNode { $1 <> $2 }

InsigBracedGroup :: { NodeSequence }
InsigBracedGroup
  : startBrace MixedContent endBrace { mconcat
                                         [ text "{"
                                         , $2
                                         , text "}" ] }

ElementNode :: { NodeSequence }
  : InlineElement { elementNode $1 }

InlineElement :: { Element }
  : inlineTag OptionalAttrSet OptionalBracedArg
      { element (getInlineTag $1) $2 $3 }

OptionalAttrSet :: { Attrs }
OptionalAttrSet
  : {- empty -} { NoAttrs }
  | AttrSet { Attrs $1 }

AttrSet :: { Seq Attr }
AttrSet
  : startAttrSet Spaces Attrs endAttrSet { $3 }

-- We sort out attribute validation later
Attrs :: { Seq Attr }
Attrs
  : {- empty -} { mempty }
  | Attrs1 { $1 }

Attrs1 :: { Seq Attr }
Attrs1
  : AttrEntry { singleAttr $1 }
  | Attrs1 AttrEntry { addAttr $1 $2 }

AttrEntry :: { Attr }
AttrEntry
  : AttrKey equals AttrVal { ($1, $3) }

AttrKey :: { Located Text }
AttrKey
  : attrKey { getAttrKey $1 }

AttrVal :: { AttrVal }
AttrVal
  : startBrace MixedContent endBrace { bracedAttrVal $2 }
  | AttrSet { setAttrVal $1 }

OptionalBracedArg :: { Maybe NodeSequence }
OptionalBracedArg
  : {- empty -} { Nothing }
  | startBrace MixedContent endBrace { Just $2 }

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
parseNodes :: Parser (Seq Node)
parseNodes = unNodeSequence <$> pManyNodes

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

-- | Partial function that matches an attribute key
getAttrKey :: Located Tok.Token -> Located Text
getAttrKey (Located x (Tok.AttrKey k)) = Located x k
getAttrKey _ = error "Internal error"

getEscapeSequence :: Located Tok.Token -> Located Tok.Escape
getEscapeSequence (Located x (Tok.EscapeSeq e)) = Located x e
getEscapeSequence _ = error "Internal error"
}
