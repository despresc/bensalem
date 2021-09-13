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
import Data.Void (Void)
import Scriba.Markup.ScribaML.Token
import Text.Megaparsec
  ( MonadParsec,
    Parsec,
    many,
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

- may want to lex verbatim spans with multiple tokens, in a line-oriented
  fashion. could be necessary for nicer parse errors later on. would require
  more in 'LexState', I imagine, unless we call the current tokens "raw tokens"
  and expand them into another type a later pass (which could also insert
  INDENT/DEINDENT tokens if we were so inclined)
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

-- | The 'LexState' keeps track of the current line number
newtype LexState = LexState
  { lexLine :: Line
  }
  deriving (Eq, Ord, Show)

-- | Run a 'Lex' action at line 1
runLex :: Lex a -> Parsec Void Text a
runLex = fmap fst . go . S.runStateT . unLex
  where
    go f = f $ LexState 0

instance (a ~ Text) => IsString (Lex a) where
  fromString = Lex . S.lift . fromString

-- | Get the current 'Line' number
getLexLine :: Lex Line
getLexLine = S.gets lexLine

-- | Increment the 'Line' and return the /new/ state
incLexLine :: Lex Line
incLexLine = do
  S.modify (\(LexState n) -> LexState $ n + 1)
  S.gets lexLine

-- | Parse a single token. Note that this parser has no way of knowing whether
-- or not it is at the beginning of a line, and so it will always parse an
-- initial run of space characters as 'LineSpace' and not 'Indent'.
token :: Lex Token
token =
  printingText
    <|> indent
    <|> lineSpace
    <|> backslashTok
    <|> ampTok
    <|> lbrace
    <|> rbrace
    <|> lbracket
    <|> rbracket
    <|> equals
    <|> comma
  where
    lbrace = "{" $> Lbrace
    rbrace = "}" $> Rbrace
    lbracket = "[" $> Lbracket
    rbracket = "]" $> Rbracket
    equals = "=" $> Equals
    comma = "{" $> Comma

-- | Parse a run of 'PrintingText', text with no significant characters in it
printingText :: Lex Token
printingText = PrintingText <$> takeWhile1P Nothing notSpecial
  where
    notSpecial c = c `notElem` ['\\', '&', '[', ']', '{', '}', '=', ',', ' ']

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
        <|> backslashAnon
        <|> lineComment
        <|> verbatim

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

-- | Parse an anonymous backslash tag, assuming that we have already parsed the
-- initial @\\@. Anonymous backslash tags look like @&@ followed by @.@ (a
-- period).
backslashAnon :: Lex Token
backslashAnon = "." $> BackslashAnon

-- | Parse a line comment, assuming that we have already parsed the initial
-- @\\@. Line comments start with @\\%@ and continue until a newline or the end
-- of input is reached.
lineComment :: Lex Token
lineComment = "%" >> LineComment <$> takeWhileP Nothing (/= '\n')

-- | Parse a verbatim span, assuming that we have already parsed the initial
-- @\\@. A verbatim span looks like @\\`@ followed by a sequence of verbatim
-- text, ending in @`/@. The verbatim text, in turn, is either the sequence
-- @\`\`@, interpreted as the single character @\'`\'@, or text not containing
-- the character @\'`\'@.
verbatim :: Lex Token
verbatim = do
  void "`"
  ln <- verbatimLine
  go (VerbatimLine 0 ln :)
  where
    go lacc =
      ( do
          ln <- fullVerbatimLine
          go (lacc . (ln :))
      )
        <|> ( do
                void "`/"
                lp <- getLexLine
                pure $ Verbatim lp (lacc [])
            )
    fullVerbatimLine = do
      void "\n"
      void incLexLine
      n <- verbatimIndent
      vlts <- verbatimLine
      pure $ VerbatimLine n vlts
    verbatimIndent = T.length <$> takeWhileP Nothing (== ' ')
    verbatimLine = many $ verbText <|> verbTick
    verbText = VerbatimLineText <$> takeWhile1P Nothing (/= '`')
    verbTick = "``" $> VerbatimBacktick

-- | Parse a layout tag, which is either an 'ampTag' or an 'ampAnon'. In either
-- case, a layout tag will start with @&@.
ampTok :: Lex Token
ampTok = "&" >> tok
  where
    tok = ampTag <|> ampAnon

-- | Parse a named layout tag, assuming that we have already parsed the initial
-- @\\@. Named layout tags look like @&@ followed by 'tagText'.
ampTag :: Lex Token
ampTag = AmpTag <$> tagText

-- | Parse an anonymous layout tag, assuming that we have already parsed the
-- initial @\\@. Anonymous layout tags look like @&@ followed by @.@ (a period).
ampAnon :: Lex Token
ampAnon = "." $> AmpAnon

-- | Tag text is a sequence of alphanumeric characters (characters accepted by
-- 'isAlphaNum')
tagText :: Lex Text
tagText = takeWhile1P (Just "tag text") isAlphaNum

-- | Parse "indentation", which is a newline followed by zero or more blank
-- lines (zero or more spaces followed by a newline)
indent :: Lex Token
indent = newline >> go id
  where
    lineStart = takeWhileP Nothing (== ' ')
    go bacc = do
      ls <- lineStart
      lp <- incLexLine
      option (Indent lp (bacc []) (T.length ls)) $
        newline >> go (bacc . (ls :))
