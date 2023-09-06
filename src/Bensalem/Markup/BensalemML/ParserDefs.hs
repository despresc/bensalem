{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Parser definitions
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- Definitions for the bensalem lexer and parser
module Bensalem.Markup.BensalemML.ParserDefs
  ( -- * Parser monad
    Parser (..),
    evalParser,
    setInput,
    ParseState (..),
    initParseState,
    ParseError (..),
    LexError (..),
    throwLexError,
    throwParseError,
    throwParseErrorNil,

    -- * Source positions
    SrcPos (..),
    initSrcPos,
    SrcSpan (..),
    Located (..),

    -- * Alex input
    AlexInput (..),
    alexGetByte,
    initAlexInput,
    fromAlexInput,

    -- * Scopes
    Scope (..),
    ScopeType (..),

    -- * Temporary testing function
    unsafeParseTest,
  )
where

import Bensalem.Markup.BensalemML.Token (Token)
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.State.Strict
  ( MonadState (..),
    StateT (..),
    evalStateT,
    modify,
  )
import qualified Data.Bits as Bits
import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)

newtype Parser a = Parser
  { unParser :: StateT ParseState (Except ParseError) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError ParseError,
      MonadState ParseState
    )

throwLexError :: SrcSpan -> LexError -> Parser a
throwLexError sp = throwError . LexerError sp

throwParseError :: Maybe (Located Token) -> Parser a
throwParseError = throwError . ParserError

-- TODO will want to replace uses of this with something more informative!
throwParseErrorNil :: Parser a
throwParseErrorNil = throwParseError Nothing

setInput :: AlexInput -> Parser ()
setInput ai = modify $ \ps -> ps {parseStateInput = ai}

-- | Evaluate the given parser action starting at the initial lexing state
evalParser :: Parser a -> AlexInput -> Either ParseError a
evalParser = go . evalStateT . unParser
  where
    go f = runExcept . f . initParseState

-- | The parser state. Be very, very careful when modifying this manually, as
-- lexing and parsing require the 'ParseState' to have particular properties at
-- particular points in the source code to function properly. Most of the fields
-- are self-explanatory, but it is important to note that if you modify the
-- 'parseStateScopeStack' manually then you will probably need to modify the
-- 'parseStateStartCode' as well, since the scopes keep track of the start code
-- to be restored when they are resolved. You are encouraged to use the internal
-- @gatherScopesDL@ function in "Bensalem.Markup.BensalemML.LexerActions" if you
-- need to resolve some scopes on the stack, as this function handles start code
-- restoration automatically for you.
data ParseState = ParseState
  { -- | the current lexer code
    parseStateStartCode :: !Int,
    -- | the current input
    parseStateInput :: !AlexInput,
    -- | any pending tokens to be emitted before further consumption of input
    parseStatePendingTokens :: ![Located Token],
    -- | the current layout depth - falling to at most this indent counts as a
    --   deindent
    parseStateLayoutDepth :: !Int,
    -- | the current stack of scopes
    parseStateScopeStack :: ![Scope]
  }
  deriving (Eq, Ord, Show)

initParseState :: AlexInput -> ParseState
initParseState ai =
  ParseState
    { parseStateStartCode = 0,
      parseStateInput = ai,
      parseStatePendingTokens = [],
      parseStateLayoutDepth = 0,
      parseStateScopeStack = []
    }

data Scope = Scope
  { scopeType :: !ScopeType,
    -- | the start code that needs to be restored upon scope exit
    scopeAmbientStartCode :: !Int,
    -- | the span of the token that started the scope
    scopePos :: !SrcSpan
  }
  deriving (Eq, Ord, Show)

data ScopeType
  = BraceScope
  | AttrSetScope
  | -- | the depth it defines, the ambient depth
    LayoutScope !Int !Int
  | LevelScope !Int
  deriving (Eq, Ord, Show)

-- | A possible error that can occur during parsing
data ParseError
  = -- | source span where the error occurred (see 'LexError'), lexer error
    LexerError SrcSpan LexError
  | -- | temporary - includes last-parsed token if possible (I think)
    ParserError (Maybe (Located Token))
  deriving (Eq, Ord, Show)

-- | A possible error that can occur in the lexing phase. Note that the meaning
-- of the 'SrcSpan' of the containing 'ParseError' depends on what the
-- 'LexError' itself is. The errors 'NoToken', 'UnmatchedEndBraceGroup',
-- 'UnmatchedEndAttrSet', and 'UnmatchedEndVerbatim' will have a zero-width
-- 'SrcSpan'; the 'NoTokenError' occurs at the position in input where a token
-- could not be read, and the three unmatched end errors all occur at the end of
-- input. The 'SrcSpan' of the remaining errors is the span of the
-- partially-recognized input (e.g., the span of the 'UnmatchedEndBraceGroup' is
-- the source position of the end of brace group).
data LexError
  = -- | lexically invalid element name
    InvalidEltName Text
  | -- | a token could not be parsed
    NoToken
  | -- | a start of brace group was not matched by an end of brace group (span
    -- of start of brace group)
    UnmatchedStartBraceGroup SrcSpan
  | -- | a start of brace group was not matched by an end of brace group (span
    -- of start of brace group)
    UnmatchedStartAttrSet SrcSpan
  | -- | an end of brace group was encountered with no matching start of brace
    -- | group
    UnmatchedEndBraceGroup
  | -- | an end of brace group was encountered with no matching start of brace
    -- | group
    UnmatchedEndAttrSet
  | -- | a start of attribute set was ended by an end of braced group (span of
    -- start of attribute set)
    AttrBraceMismatch SrcSpan
  | -- | a start of braced group attribute set was ended by an end of attribute
    -- set (span of start of braced group)
    BraceAttrMismatch SrcSpan
  | -- | a layout scope was ended by an end of attribute set (span of layout
    -- tag)
    LayoutAttrMismatch SrcSpan
  | -- | a start of braced group attribute set was ended by an end of attribute
    -- set (span of layout tag)
    LevelAttrMismatch SrcSpan
  | -- | a de-indent occurred in a braced group (span of start of braced group)
    DeIndentInBracedGroup SrcSpan
  | -- | a de-indent occurred in attribute set (span of start of attribute set)
    DeIndentInAttrSet SrcSpan
  deriving (Eq, Ord, Show)

-- | A position in a 'Char' stream. The 'srcOffset' is the index of the position
-- in the initial stream.
data SrcPos = SrcPos
  { srcOffset :: !Int,
    srcLine :: !Int,
    srcCol :: !Int
  }
  deriving (Eq, Ord, Show)

-- | The initial source position, with zero offset and at line and column one.
initSrcPos :: SrcPos
initSrcPos = SrcPos 0 1 1

data SrcSpan = SrcSpan
  { srcSpanName :: !Text,
    srcSpanStart :: !SrcPos,
    srcSpanEnd :: !SrcPos
  }
  deriving (Eq, Ord, Show)

data Located a = Located
  { locatedSpan :: !SrcSpan,
    locatedVal :: a
  }
  deriving (Eq, Ord, Show)

data MinCol
  = MinCol !Int
  | NoMinCol
  deriving (Eq, Ord, Show)

-- | The input for alex, keeping track of 'Text' input as if it were a stream of
-- bytes in the UTF-8 encoding, and also the current position in the input and
-- tab width.

-- TODO: should make this opaque
data AlexInput = AlexInput
  { alexInputTabWidth :: !Int,
    alexInputSrcName :: !Text,
    alexInputSrcPos :: !SrcPos,
    alexInputNumSurplusBytes :: !NumSurplusBytes,
    -- | see the documentation for 'NumSurplusBytes' for its interaction with
    -- these bytes
    alexInputByte0 :: !Word8,
    alexInputByte1 :: !Word8,
    alexInputByte2 :: !Word8,
    alexInputText :: !Text
  }
  deriving (Eq, Ord)

instance Show AlexInput where
  show = show . fromAlexInput

-- | Keeps track of the number of bytes leftover from an incompletely consumed
-- 'Char'. Note that this is effectively an index into the list 'byte0',
-- 'byte1', 'byte2', so in particular if we have three surplus bytes then
-- 'byte2' is consumed first. This is also reflected in the ordering of the list
-- that 'fromAlexInput' returns.
type NumSurplusBytes = Word8

-- | Pop the first byte from the given 'AlexInput', if it is non-empty,
-- considering the 'Text' input as a stream of bytes encoded in UTF-8. Adapted
-- from the basic wrapper in alex.
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte ai@(AlexInput tw sn sp ns b0 b1 b2 inp) = case ns of
  0 -> case T.uncons inp of
    Just (oc, inp') -> Just ret
      where
        sp' = case oc of
          '\n' ->
            sp
              { srcCol = 1,
                srcLine = srcLine sp + 1,
                srcOffset = srcOffset sp + 1
              }
          '\t' ->
            sp
              { srcCol = srcCol sp + tw - ((srcCol sp - 1) `mod` tw),
                srcOffset = srcOffset sp + 1
              }
          _ ->
            sp
              { srcCol = srcCol sp + 1,
                srcOffset = srcOffset sp + 1
              }
        oc' = ord oc
        -- yes, there should be no parentheses in b0'
        b0' = fromIntegral $ 0x80 + oc' Bits..&. 0x3f
        b1' = fromIntegral $ 0x80 + ((oc' `Bits.shiftR` 6) Bits..&. 0x3f)
        b2' = fromIntegral $ 0x80 + ((oc' `Bits.shiftR` 12) Bits..&. 0x3f)
        ret
          | oc' <= 0x7f =
              ( fromIntegral oc',
                AlexInput tw sn sp' 0 0 0 0 inp'
              )
          | oc' <= 0x7ff =
              ( fromIntegral $ 0xc0 + (oc' `Bits.shiftR` 6),
                AlexInput tw sn sp' 1 b0' 0 0 inp'
              )
          | oc' <= 0x7fff =
              ( fromIntegral $ 0xe0 + (oc' `Bits.shiftR` 12),
                AlexInput tw sn sp' 2 b0' b1' 0 inp'
              )
          | otherwise =
              ( fromIntegral $ 0xf0 + (oc' `Bits.shiftR` 18),
                AlexInput tw sn sp' 3 b0' b1' b2' inp'
              )
    Nothing -> Nothing
  1 -> Just (b0, ai {alexInputNumSurplusBytes = 0})
  2 -> Just (b1, ai {alexInputNumSurplusBytes = 1})
  _ -> Just (b2, ai {alexInputNumSurplusBytes = 2})

-- | Initialize the 'AlexInput'
initAlexInput ::
  -- | tab width
  Int ->
  -- | source name
  Text ->
  -- | initial input
  Text ->
  AlexInput
initAlexInput n sname t =
  AlexInput
    { alexInputSrcName = sname,
      alexInputTabWidth = n,
      alexInputSrcPos = initSrcPos,
      alexInputNumSurplusBytes = 0,
      alexInputByte0 = 0,
      alexInputByte1 = 0,
      alexInputByte2 = 0,
      alexInputText = t
    }

-- | Decompose the 'AlexInput' into the tab width, source name, current source
-- position, a list of bytes from an incompletely consumed initial character,
-- and the remainder of the 'Text' input
fromAlexInput :: AlexInput -> (Int, Text, SrcPos, [Word8], Text)
fromAlexInput (AlexInput tw sn sp ns b0 b1 b2 t) = (tw, sn, sp, ret, t)
  where
    ret = case ns of
      0 -> []
      1 -> [b0]
      2 -> [b1, b0]
      _ -> [b2, b1, b0]

-- | Evaluate the given parser action with the given input and return the
-- result, throwing an exception if a parse error was encountered
unsafeParseTest :: Parser a -> Text -> a
unsafeParseTest p t = case evalParser p (initAlexInput 2 "<input>" t) of
  Left e -> error $ show e
  Right a -> a
