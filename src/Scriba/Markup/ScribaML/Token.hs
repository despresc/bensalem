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
  | BackslashTag !Text
  | LevelTag !Int !Text
  | StartVerbatim
  | -- | any text other than a newline, space, or backtick
    VerbatimPrintingText !Text
  | -- | the literal @``@ in a verbatim context
    VerbatimBacktick
  | EndVerbatim
  | AmpTag !Text
  | Lbrace
  | Rbrace
  | Lbracket
  | Rbracket
  | Equals
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
renderToken (BackslashTag t) = "\\" <> t
renderToken (LevelTag n t) = "\\" <> T.replicate n "#" <> t
renderToken StartVerbatim = "\\`"
renderToken (VerbatimPrintingText t) = t
renderToken VerbatimBacktick = "``"
renderToken EndVerbatim = "`/"
renderToken (AmpTag t) = "\\" <> t
renderToken Lbrace = "{"
renderToken Rbrace = "}"
renderToken Lbracket = "["
renderToken Rbracket = "]"
renderToken Equals = "="

-- | The line that a 'BlankLine' or 'Indent' token starts, or the line
-- on which a 'Comment' ends
type Line = Int

-- | The amount of indentation (number of space characters)
-- represented by an 'Indent' token
type Indent = Int
