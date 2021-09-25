{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Description : Scriba document lexer
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- A lexer for scriba documents
module Scriba.Markup.ScribaML.Lexer where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (MonadPlus)
import qualified Control.Monad.State.Strict as S
import Data.Char (isAlphaNum)
import Data.Functor (void, ($>))
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Void (Void)
import Scriba.Markup.ScribaML.Token
import Text.Megaparsec
  ( MonadParsec,
    Parsec,
    option,
    takeWhile1P,
    takeWhileP,
  )
import Text.Megaparsec.Char
  ( newline,
  )

{- TODO

- improve the notElem bits of parsers

- attach "while parsing the verbatim span at ..." messages to improve errors in
  case of runaway verbatim spans

-}

-- | A 'Parsec' monad with added 'LexState'
newtype Lex a = Lex {unLex :: S.StateT LexState (Parsec Void Text) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      Alternative,
      MonadPlus,
      MonadParsec Void Text,
      S.MonadState LexState
    )

-- | The 'LexState' keeps track of the current line number and the current
-- lexing mode.
newtype LexState = LexState
  { lexMode :: LexMode
  }
  deriving (Eq, Ord, Show)

data LexMode
  = LexPlain
  | LexVerbatim
  deriving (Eq, Ord, Show)

-- | Run a 'Lex' action at line 1 in the initial plain mode
runLex :: Lex a -> Parsec Void Text a
runLex = fmap fst . go . S.runStateT . unLex
  where
    go f = f $ LexState LexPlain

instance (a ~ Text) => IsString (Lex a) where
  fromString = Lex . S.lift . fromString

getLexMode :: Lex LexMode
getLexMode = S.gets lexMode

setLexMode :: LexMode -> Lex ()
setLexMode m = S.modify $ const $ LexState m

-- | Parse a single token. Note that this parser has no way of knowing whether
-- or not it is at the beginning of a line, and so it will always parse an
-- initial run of space characters as 'LineSpace' and not 'Indent'.
token :: Lex Token
token = do
  m <- getLexMode
  case m of
    LexPlain ->
      plainText
        <|> indent
        <|> lineSpace
        <|> backslashTok
        <|> ampTag
        <|> lbrace
        <|> rbrace
        <|> lbracket
        <|> rbracket
        <|> equals
    LexVerbatim ->
      indent
        <|> lineSpace
        <|> verbatimPlainText
        <|> verbatimBacktick
  where
    lbrace = "{" $> Lbrace
    rbrace = "}" $> Rbrace
    lbracket = "[" $> Lbracket
    rbracket = "]" $> Rbracket
    equals = "=" $> Equals
    verbatimBacktick = "``" $> VerbatimBacktick

-- | Parse a run of 'PlainText', text with no significant characters in it
plainText :: Lex Token
plainText = PlainText <$> takeWhile1P Nothing notSpecial
  where
    notSpecial c = c `notElem` ['\\', '&', '[', ']', '{', '}', '=', ' ']

-- | Parse a run of 'LineSpace', a sequence of @\' \'@ space characters
lineSpace :: Lex Token
lineSpace = LineSpace . T.length <$> takeWhile1P (Just "line space") (== ' ')

-- | Parse one of the tokens that start with a @\\@
backslashTok :: Lex Token
backslashTok = "\\" >> tok
  where
    tok =
      escape
        <|> backslashTag
        <|> numberSignTag
        <|> lineComment
        <|> startVerbatim

-- | Parse an escape sequence in plain (not verbatim) text, assuming that we
-- have already parsed the initial @\\@. The recognized escape sequences are
-- @\\\\@, @\\&@, @\\{@, @\\}@, @\\[@, and @\\]@.
escape :: Lex Token
escape =
  fmap Escape $
    escBackslash <|> escAnd <|> escLbrace <|> escRbrace
      <|> escLbracket
      <|> escRbracket
  where
    escBackslash = "\\" $> EscBackslash
    escAnd = "&" $> EscAnd
    escLbrace = "{" $> EscLbrace
    escRbrace = "}" $> EscRbrace
    escLbracket = "[" $> EscLbracket
    escRbracket = "]" $> EscRbracket

-- | Parse a named backslash tag, assuming that we have already parsed the
-- initial @\\@. Backslash tags look like @\\@ followed by 'tagText'.
backslashTag :: Lex Token
backslashTag = BackslashTag <$> tagText

-- | Parse a line comment, assuming that we have already parsed the initial
-- @\\@. Line comments start with @\\%@ and continue until a newline or the end
-- of input is reached.
lineComment :: Lex Token
lineComment = "%" >> LineComment <$> takeWhileP Nothing (/= '\n')

-- | Parse the start of a verbatim span, assuming that we have already parsed
-- the initial @\\@. This parser also sets the lexing mode to 'LexVerbatim'.
startVerbatim :: Lex Token
startVerbatim = do
  void "`"
  setLexMode LexVerbatim
  pure StartVerbatim

-- | Parse the end of a verbatim span. This parser also sets the lexing mode to
-- 'LexPlain'.
endVerbatim :: Lex Token
endVerbatim = do
  void "`/"
  setLexMode LexPlain
  pure EndVerbatim

-- | Parse verbatim text other than the backtick escape and significant white
-- space.
verbatimPlainText :: Lex Token
verbatimPlainText = VerbatimPlainText <$> takeWhile1P Nothing p
  where
    p c = c `notElem` [' ', '\n', '`']

-- | Parse a level tag, assuming that we have already parsed the initial @\\@.
-- Level tags look like @\\@ followed by one or more @#@ characters.
numberSignTag :: Lex Token
numberSignTag = do
  n <- T.length <$> takeWhile1P (Just "#") (== '#')
  NumberSignTag n <$> tagText

-- | Parse a named layout tag, assuming that we have already parsed the initial
-- @\\@. Named layout tags look like @&@ followed by 'tagText'.
ampTag :: Lex Token
ampTag = fmap AmpTag $ "&" >> tagText

-- | Tag text is a sequence of alphanumeric characters (characters accepted by
-- 'isAlphaNum')
tagText :: Lex Text
tagText = takeWhile1P (Just "tag text") isAlphaNum

-- | Parse "indentation", which is a newline followed by zero or more blank
-- lines (zero or more spaces followed by a newline)
indent :: Lex Token
indent = newline >> go "\n"
  where
    lineStart = TB.fromText <$> takeWhileP Nothing (== ' ')
    go bacc = do
      ls <- lineStart
      option (Indent (TL.toStrict $ TB.toLazyText $ bacc <> ls)) $
        newline >> go (bacc <> ls <> "\n")
