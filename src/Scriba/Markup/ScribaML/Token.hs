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

import qualified Data.List as List
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
  | BackslashAnon
  | -- | start of a verbatim span, then the first run of verbatim line text
    StartVerbatim ![VerbatimLineText]
  | -- | newline, then subsequent indentation and verbatim text
    VerbatimLine !Line !Indent ![VerbatimLineText]
  | EndVerbatim
  | AmpTag !Text
  | AmpAnon
  | Lbrace
  | Rbrace
  | Lbracket
  | Rbracket
  | Equals
  | Comma
  | -- | virtual token indicating a new indentation context
    IncIndent
  | -- | virtual token indicating the closing of a single indentation context
    DecIndent
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
renderToken (BackslashTag t) = "\\" <> t
renderToken BackslashAnon = "\\."
renderToken (StartVerbatim ts) = "\\`" <> renderVerbatimLineText ts
renderToken (VerbatimLine _ i ts) = "\n" <> T.replicate i " " <> renderVerbatimLineText ts
renderToken EndVerbatim = "`/"
renderToken (AmpTag t) = "\\" <> t
renderToken AmpAnon = "&."
renderToken Lbrace = "{"
renderToken Rbrace = "}"
renderToken Lbracket = "["
renderToken Rbracket = "]"
renderToken Equals = "="
renderToken Comma = ","
renderToken IncIndent = ""
renderToken DecIndent = ""

renderVerbatimLineText :: [VerbatimLineText] -> Text
renderVerbatimLineText = T.concat . fmap go
  where
    go (VerbatimLineText t) = t
    go VerbatimBacktick = "``"

-- | Returns the number of source characters represented by a particular
-- 'Token'. Satisfies
--
-- @
-- 'tokenLength' = 'T.length' . 'renderToken'
-- @
tokenLength :: Token -> Int
tokenLength (PrintingText t) = T.length t
tokenLength (Escape _) = 2
tokenLength (Indent _ ls n) = 1 + sum ls' + n
  where
    ls' = List.intersperse 1 $ T.length <$> ls
tokenLength (LineSpace n) = n
tokenLength (LineComment t) = 2 + T.length t
tokenLength (BackslashTag t) = 1 + T.length t
tokenLength BackslashAnon = 2
tokenLength (StartVerbatim ts) = 2 + verbatimLineTextLength ts
tokenLength (VerbatimLine _ i ts) = 1 + i + verbatimLineTextLength ts
tokenLength EndVerbatim = 2
tokenLength (AmpTag t) = 1 + T.length t
tokenLength AmpAnon = 2
tokenLength Lbrace = 1
tokenLength Rbrace = 1
tokenLength Lbracket = 1
tokenLength Rbracket = 1
tokenLength Equals = 1
tokenLength Comma = 1
tokenLength IncIndent = 0
tokenLength DecIndent = 0

verbatimLineTextLength :: [VerbatimLineText] -> Int
verbatimLineTextLength = sum . fmap go
  where
    go (VerbatimLineText t) = T.length t
    go VerbatimBacktick = 2

-- | The line that a 'BlankLine' or 'Indent' token starts, or the line
-- on which a 'Comment' ends
type Line = Int

-- | The amount of indentation (number of space characters)
-- represented by an 'Indent' token
type Indent = Int
