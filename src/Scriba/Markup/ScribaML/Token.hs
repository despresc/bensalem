{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Scriba document tokens
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- The types representing the rough lexical structure of a scriba
-- document.
module Scriba.Markup.ScribaML.Token where

import Data.Text (Text)
import qualified Data.Text as T

-- | An individual scriba token.
data Token
  = -- | text other than a line ending, space, or special character
    PrintingText !Text
  | Escape !EscSequence
  | -- | zero or more blank lines, then zero or more spaces, also recording the
    -- last line's number
    Indent !Line ![Text] !Int
  | -- | spaces not at the beginning of a line
    LineSpace !Int
  | LineComment !Text
  | Verbatim !Line ![VerbatimLine]
  | BackslashTag !Text
  | BackslashAnon
  | AmpTag !Text
  | AmpAnon
  | Lbrace
  | Rbrace
  | Lbracket
  | Rbracket
  | Equals
  | Comma
  deriving (Eq, Ord, Show)

-- | One of the recognized escape sequences in plain (not verbatim) text. These
-- all look like @\\@ followed by the indicated character.
data EscSequence
  = EscBackslash
  | EscAnd
  | EscLbrace
  | EscRbrace
  | EscLbracket
  | EscRbracket
  deriving (Eq, Ord, Show)

-- | A line of verbatim text is an initial sequence of space indentation
-- followed by a sequence of 'VerbatimLineText'
data VerbatimLine = VerbatimLine
  { verbIndent :: !Indent,
    verbLine :: ![VerbatimLineText]
  }
  deriving (Eq, Ord, Show)

-- | Verbatim line text is either the sequence @\`\`@, interpreted as a single
-- @'`'@, or a sequence of characters other than the character @\'`'@
data VerbatimLineText
  = VerbatimLineText !Text
  | VerbatimBacktick
  deriving (Eq, Ord, Show)

-- | Render a single token back to 'Text'
renderToken :: Token -> Text
renderToken (PrintingText t) = t
renderToken (Escape EscAnd) = "\\&"
renderToken (Escape EscBackslash) = "\\\\"
renderToken (Escape EscLbrace) = "\\{"
renderToken (Escape EscRbrace) = "\\}"
renderToken (Escape EscLbracket) = "\\["
renderToken (Escape EscRbracket) = "\\]"
renderToken (Indent _ ls n) = "\n" <> T.concat ls <> T.replicate n " "
renderToken (LineSpace n) = T.replicate n " "
renderToken (LineComment t) = "\\%" <> t
renderToken (Verbatim _ ls) = "\\`" <> T.intercalate "\n" (go <$> ls) <> "`/"
  where
    fromLineText (VerbatimLineText t) = t
    fromLineText VerbatimBacktick = "``"
    go (VerbatimLine n ts) = T.replicate n " " <> T.concat (fromLineText <$> ts)
renderToken (BackslashTag t) = "\\" <> t
renderToken BackslashAnon = "\\."
renderToken (AmpTag t) = "\\" <> t
renderToken AmpAnon = "&."
renderToken Lbrace = "{"
renderToken Rbrace = "}"
renderToken Lbracket = "["
renderToken Rbracket = "]"
renderToken Equals = "="
renderToken Comma = ","

-- | The line that a 'BlankLine' or 'Indent' token starts, or the line
-- on which a 'Comment' ends
type Line = Int

-- | The amount of indentation (number of space characters)
-- represented by an 'Indent' token
type Indent = Int
