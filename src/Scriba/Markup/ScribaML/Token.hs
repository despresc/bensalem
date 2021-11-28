{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Scriba document tokens
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- The types representing the rough lexical structure of a scriba document.
module Scriba.Markup.ScribaML.Token where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T

-- TODO rename tokens as necessary (level tag, layout tag, etc. probably braced
-- tag or inline tag)

-- | An individual scriba token.
data Token
  = -- | text other than a line ending, space, or special character
    PlainText !Text
  | Escape !EscSequence
  | -- | zero or more blank lines, then zero or more spaces
    Indent !Text
  | -- | spaces not at the beginning of a line
    LineSpace !Int
  | LineComment !Text
  | BackslashTag !EltName
  | NumberSignTag !Int !EltName
  | StartVerbatim
  | -- | any text other than a newline, space, or backtick
    VerbatimPlainText !Text
  | -- | the literal @``@ in a verbatim context
    VerbatimBacktick
  | EndVerbatim
  | AmpTag !EltName
  | Lbrace
  | Rbrace
  | Lbracket
  | Rbracket
  | Equals
  | Comma
  | EndImplicitScope
  | -- | the end of file token. should only appear as the final token
    -- in a stream
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
renderToken (Indent ls) = ls
renderToken (LineSpace n) = T.replicate n " "
renderToken (LineComment t) = "\\%" <> t
renderToken (BackslashTag t) = "\\" <> t
renderToken (NumberSignTag n t) = "\\" <> T.replicate n "#" <> t
renderToken StartVerbatim = "\\`"
renderToken (VerbatimPlainText t) = t
renderToken VerbatimBacktick = "``"
renderToken EndVerbatim = "`/"
renderToken (AmpTag t) = "\\&" <> t
renderToken Lbrace = "{"
renderToken Rbrace = "}"
renderToken Lbracket = "["
renderToken Rbracket = "]"
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

-- | A lexically-valid element name, which is a list of non-empty unicode
-- alphanumeric strings, separated by periods. In future this type will have
-- more structure.
--
-- The lexical structure may also change, if it seems necessary to
type EltName = Text

-- | Tests whether or not the 'Text' is a valid 'EltName'
validEltName :: Text -> Maybe EltName
validEltName t
  | T.all isAlphaNum t = Just t
  | otherwise = Nothing
