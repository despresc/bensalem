{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Bensalem document tokens
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- The types representing the rough lexical structure of a bensalem document.
module Bensalem.Markup.BensalemML.Token where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T

-- | An individual bensalem token.
data Token
  = -- | text other than a line ending, space, or special character
    PlainText !Text
  | Escape !EscSequence
  | -- | zero or more blank lines, then zero or more spaces
    Blanks !Text
  | -- | spaces not at the beginning of a line
    LineSpace !Int
  | -- | @\\%comment text continues until just before end of line@
    LineComment !Text
  | InlineTag !EltName
  | -- | @#tagname@
    LevelTag !Int !EltName
  | -- | @&tagname@
    LayoutTag !EltName
  | -- | @&{}@
    StartLayoutBracedGroup
  | -- | @\\`@
    StartInlineVerbatim
  | -- | any text other than a newline, space, or backtick
    InlineVerbatimText !Text
  | -- | the literal @``@ in a verbatim context
    VerbatimBacktick
  | -- | @`/@
    EndInlineVerbatim
  | -- | @{@
    StartBraceGroup
  | -- | @}@
    EndBraceGroup
  | -- | @[@
    StartAttrSet
  | -- | @]@
    EndAttrSet
  | -- | @=@
    Equals
  | -- | @,@
    Comma
  | -- | virtual token (zero-width, not appearing explicitly in the source)
    -- denoting the end of content for a level or layout element
    EndImplicitScope
  | -- | the end of file token. should only appear as the final token in a
    -- stream
    TokenEOF
  deriving (Eq, Ord, Show)

-- | One of the recognized escape sequences in plain (not verbatim) text. These
-- all look like @\\@ followed by the indicated character.
data EscSequence
  = EscBackslash
  | EscLbrace
  | EscRbrace
  | EscLbracket
  | EscRbracket
  | EscAnd
  | EscNum
  deriving (Eq, Ord, Show)

-- | Render a single token back to 'Text'
renderToken :: Token -> Text
renderToken (PlainText t) = t
renderToken (Escape EscBackslash) = "\\\\"
renderToken (Escape EscLbrace) = "\\{"
renderToken (Escape EscRbrace) = "\\}"
renderToken (Escape EscLbracket) = "\\["
renderToken (Escape EscRbracket) = "\\]"
renderToken (Escape EscAnd) = "\\&"
renderToken (Escape EscNum) = "\\#"
renderToken (Blanks ls) = ls
renderToken (LineSpace n) = T.replicate n " "
renderToken (LineComment t) = "\\%" <> t
renderToken (InlineTag t) = "\\" <> t
renderToken (LevelTag n t) = T.replicate n "#" <> t
renderToken (LayoutTag t) = "&" <> t
renderToken StartLayoutBracedGroup = "&{}"
renderToken StartInlineVerbatim = "\\`"
renderToken (InlineVerbatimText t) = t
renderToken VerbatimBacktick = "``"
renderToken EndInlineVerbatim = "`/"
renderToken StartBraceGroup = "{"
renderToken EndBraceGroup = "}"
renderToken StartAttrSet = "["
renderToken EndAttrSet = "]"
renderToken Equals = "="
renderToken Comma = ","
renderToken EndImplicitScope = ""
renderToken TokenEOF = ""

-- | The line that a 'BlankLine' or 'Indent' token starts, or the line
-- on which a 'Comment' ends
type Line = Int

-- | The amount of indentation (number of space characters)
-- represented by an 'Indent' token
type Indent = Int

-- | A lexically-valid element name, which is a sequence of unicode alphanumeric
-- characters, dashes, and underscores
type EltName = Text

-- | Tests whether or not the 'Text' is a valid 'EltName'. Presently, a valid
-- 'EltName' is simply a string of alphanumeric characters, dashes, and
-- underscores.
validEltName :: Text -> Maybe EltName
validEltName t
  | T.all isValidChar t = Just t
  | otherwise = Nothing
  where
    isValidChar c = isAlphaNum c || c == '-' || c == '_'
