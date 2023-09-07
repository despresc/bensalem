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
  (parseNodes,parseNodesTW,pNodes) where

import Bensalem.Markup.BensalemML.Syntax
import Bensalem.Markup.BensalemML.ParserDefs
  (Located(..),
   Parser,
   ParseError(..),
   SrcSpan(..),
   throwParseError,
   evalParser,
   initAlexInput)
import qualified Bensalem.Markup.BensalemML.Token as Tok
import Bensalem.Markup.BensalemML.Lexer (lexToken)

import Data.Foldable (toList)
import Control.Monad.Except (throwError)
import Data.Text (Text)
import Data.Sequence (Seq)
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
  indent { Located _ (Tok.Indent _) }
  lineSpace { Located _ (Tok.LineSpace _) }
  inlineComment { Located _ (Tok.LineComment _) }
  escape { Located _ (Tok.TokenEscape _) }
  inlineTag { Located _ (Tok.InlineTag _) }
  levelTag { Located _ (Tok.LevelTag _ _) }
  layoutTag { Located _ (Tok.LayoutTag _) }
  startBrace { Located _ Tok.StartBraceGroup }
  endBrace { Located _ Tok.EndBraceGroup }
  startAttrSet { Located _ Tok.StartAttrSet }
  endAttrSet { Located _ Tok.EndAttrSet }
  startVariableVerb { Located _ (Tok.StartVariableVerb _) }
  variableVerbPlainText { Located _ (Tok.VariableVerbPlainText _) }
  endVariableVerb { Located _ (Tok.EndVariableVerb _) }
  attrKey { Located _ (Tok.AttrKey _) }
  endImplicitScope { Located _ Tok.EndImplicitScope }

%%

-- a top-level node
MixedContentNode :: { NodeSequence }
  : text { text (getPlainText $1) }
  | escape { escape (getEscape $1) }
  | lineSpace { lineSpace (getLineSpace $1) }
  | indent { indent (getIndent $1) }
  | GroupNode { $1 }
  | ElementNode { $1 }
  | VariableVerbSpan { $1 }
  | inlineComment { inlineComment (getInlineComment $1) }

MixedContent :: { NodeSequence }
  : {- empty -} { mempty }
  | MixedContent1 { $1 }

MixedContent1 :: { NodeSequence }
  : MixedContentNode { $1 }
  | MixedContent1 MixedContentNode { $1 <> $2 }

VariableVerbSpan :: { NodeSequence }
VariableVerbSpan
  : startVariableVerb VariableVerbContent endVariableVerb { $2 }

VariableVerbContent :: { NodeSequence }
  : {- empty -} { mempty }
  | VariableVerbContent1 { $1 }

VariableVerbContent1 :: { NodeSequence }
VariableVerbContent1
  : VariableVerbPart { $1 }
  | VariableVerbContent1 VariableVerbPart { $1 <> $2 }

VariableVerbPart :: { NodeSequence }
VariableVerbPart
  : variableVerbPlainText { text (getVariableVerbPlainText $1) }
  | lineSpace { lineSpace (getLineSpace $1) }
  | indent { indent (getIndent $1) }

GroupNode :: { NodeSequence }
GroupNode
  : startBrace MixedContent endBrace { group $1 $2 $3 }

ElementNode :: { NodeSequence }
  : InlineElement { elementNode $1 }
  | LevelElement { elementNode $1 }
  | LayoutElement { elementNode $1 }

InlineElement :: { Element }
  : inlineTag OptionalAttrSet
      { element (getInlineTag $1) $2 InlineScopeContent }

LayoutElement :: { Element }
  : layoutTag OptionalAttrSet MixedContent endImplicitScope
      { element (getLayoutTag $1) $2 (layoutScopeContent $3) }

LevelElement :: { Element }
  : levelTag OptionalAttrSet MixedContent endImplicitScope
      { element (getLevelTag $1) $2 (levelScopeContent $3) }

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
  | Attrs1 Spaces AttrEntry { addAttr $1 $3 }

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

Space :: { () }
  : lineSpace { () }
  | indent { () }
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
pNodes :: Parser (Seq Node)
pNodes = unNodeSequence <$> pManyNodes

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
parseNodesTW tw nm inp = evalParser pNodes $ initAlexInput tw nm inp

-- | Parse a sequence of bensalem nodes from the given input with
-- 'parseNodesTW', with a default tab width of 8
parseNodes ::
  -- | input name
  Text ->
  -- | input
  Text ->
  Either ParseError (Seq Node)
parseNodes = parseNodesTW 8

----------------------------------------------------------------
-- boring partial functions to extract information from tokens
----------------------------------------------------------------

-- | Partial function that matches a plain text token
getPlainText :: Located Tok.Token -> Text
getPlainText (Located _ (Tok.PlainText t)) = t
getPlainText _ = error "Internal error"

-- | Partial function that matches an escape sequence
getLineSpace :: Located Tok.Token -> Located Int
getLineSpace (Located x (Tok.LineSpace n)) = Located x n
getLineSpace _ = error "Internal error"

-- | Partial function that matches an escape sequence
getIndent :: Located Tok.Token -> Located Text
getIndent (Located x (Tok.Indent t)) = Located x t
getIndent _ = error "Internal error"

-- | Partial function that matches an inline tag
getInlineTag :: Located Tok.Token -> Located Tok.EltName
getInlineTag (Located x (Tok.InlineTag e)) = Located x e
getInlineTag _ = error "Internal error"

-- | Partial function that matches an inline comment
getInlineComment :: Located Tok.Token -> Located Text
getInlineComment (Located x (Tok.LineComment t)) = Located x t
getInlineComment _ = error "Internal error"

-- | Partial function that matches a level tag
getLevelTag :: Located Tok.Token -> Located Text
getLevelTag (Located x (Tok.LevelTag _ t)) = Located x t
getLevelTag _ = error "Internal error"

-- | Partial function that matches a layout tag
getLayoutTag :: Located Tok.Token -> Located Text
getLayoutTag (Located x (Tok.LayoutTag t)) = Located x t
getLayoutTag _ = error "Internal error"

-- | Partial function that matches an attribute key
getAttrKey :: Located Tok.Token -> Located Text
getAttrKey (Located x (Tok.AttrKey k)) = Located x k
getAttrKey _ = error "Internal error"

-- | Partial function that matches an attribute key
getEscape :: Located Tok.Token -> Located Tok.Escape
getEscape (Located x (Tok.TokenEscape e)) = Located x e
getEscape _ = error "Internal error"

-- | Partial function that matches a variable verbatim plain text token
getVariableVerbPlainText :: Located Tok.Token -> Text
getVariableVerbPlainText (Located x (Tok.VariableVerbPlainText t)) = t
getVariableVerbPlainText _ = error "Internal error"
}
