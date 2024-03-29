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
  | -- | a single line ending, then zero or more line endings and spaces
    Indent !Text
  | -- | spaces not at the beginning of a line
    LineSpace !Int
  | -- | @\@;;comment text@
    LineComment !Text
  | -- | @\@tag@
    InlineTag !EltName
  | -- | @\@#tagname@
    LevelTag !Int !EltName
  | -- | @\@&tagname@
    LayoutTag !EltName
  | -- | an escape sequence
    TokenEscape !Escape
  | -- | @{@
    StartBraceGroup
  | -- | @}@
    EndBraceGroup
  | -- | @[@ right after a tag
    StartAttrSet
  | -- | @]@
    EndAttrSet
  | -- | @=@
    Equals
  | -- | a potential attribute key
    AttrKey !Text
  | -- | @\\`|@, or with more backticks, recording the entire length of the
    -- token
    StartVariableVerb !Int
  | -- | verbatim text other than indentation or line space
    VariableVerbPlainText !Text
  | -- | @|`\/@, with a variable number of backticks, recording the entire
    -- length of the token
    EndVariableVerb !Int
  | -- | virtual token (zero-width, not appearing explicitly in the source)
    -- denoting the end of content for a level or layout element
    EndImplicitScope
  | -- | the end of file token. should only appear as the final token in a
    -- stream
    TokenEOF
  deriving (Eq, Ord, Show)

data Escape
  = EscapeBackslash
  | EscapeOpenBrace
  | EscapeCloseBrace
  | EscapeOpenBracket
  | EscapeCloseBracket
  deriving (Eq, Ord, Show)

-- | Render a single token back to 'Text'
renderToken :: Token -> Text
renderToken (PlainText t) = t
renderToken (Indent ls) = ls
renderToken (LineSpace n) = T.replicate n " "
renderToken (LineComment t) = "@;;" <> t
renderToken (InlineTag t) = "@" <> t
renderToken (LevelTag n t) = "@" <> T.replicate n "#" <> t
renderToken (LayoutTag t) = "@&" <> t
renderToken (TokenEscape e) = "\\" <> escapeText e
renderToken StartBraceGroup = "{"
renderToken EndBraceGroup = "}"
renderToken StartAttrSet = "["
renderToken EndAttrSet = "]"
renderToken Equals = "="
renderToken (AttrKey k) = k
renderToken (StartVariableVerb n) = "\\" <> T.replicate (n - 2) "`" <> "|"
renderToken (VariableVerbPlainText t) = t
renderToken (EndVariableVerb n) = "|" <> T.replicate (n - 2) "`" <> "/"
renderToken EndImplicitScope = ""
renderToken TokenEOF = ""

-- | Render the text that a particular escape sequence represents
escapeText :: Escape -> Text
escapeText EscapeBackslash = "\\"
escapeText EscapeOpenBrace = "{"
escapeText EscapeCloseBrace = "}"
escapeText EscapeOpenBracket = "["
escapeText EscapeCloseBracket = "]"

-- | The line that a 'BlankLine' or 'Indent' token starts, or the line
-- on which a 'Comment' ends
type Line = Int

-- | The amount of indentation (number of space characters)
-- represented by an 'Indent' token
type Indent = Int

-- | A lexically-valid element name, which is a sequence of unicode alphanumeric
-- characters, dashes, and underscores
type EltName = Text

type AttrKey = Text

-- | Tests whether or not the 'Text' is a valid 'EltName'. Presently, a valid
-- 'EltName' is simply a string of alphanumeric characters, dashes, and
-- underscores.
validEltName :: Text -> Maybe EltName
validEltName t
  | T.all isValidChar t = Just t
  | otherwise = Nothing
  where
    isValidChar c = isAlphaNum c || c == '-' || c == '_'

validAttrKey :: Text -> Maybe AttrKey
validAttrKey = validEltName
