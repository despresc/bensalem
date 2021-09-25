{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Parser definitions
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- Definitions for the scriba lexer and parser
module Scriba.Markup.ScribaML.ParserUtils
  ( -- * Parser monad
    Parser (..),
    evalParser,
    ParseState (..),
    initParseState,
    ParseError (..),

    -- * Source positions
    SrcPos (..),
    initSrcPos,
    SrcSpan (..),
    Located (..),

    -- * Alex input
    AlexInput,
    alexGetByte,
    initAlexInput,
    getAlexInputSrcPos,
    getAlexInputText,
    getAlexInputSrcName,
    fromAlexInput,

    -- * Temporary testing function
    unsafeParseTest,
  )
where

import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.State.Strict (MonadState (..), StateT (..), evalStateT)
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

-- | Evaluate the given parser action starting at the initial lexing state
evalParser :: Parser a -> AlexInput -> Either ParseError a
evalParser = go . evalStateT . unParser
  where
    go f = runExcept . f . initParseState

data ParseState = ParseState
  { parseStateStartCode :: !Int,
    parseStateInput :: !AlexInput
  }
  deriving (Eq, Ord, Show)

initParseState :: AlexInput -> ParseState
initParseState = ParseState 0

data ParseError = ParseError
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

-- | The input for alex, keeping track of 'Text' input as if it were a stream of
-- bytes in the UTF-8 encoding, and also the current position in the input and
-- tab width.
data AlexInput = AlexInput
  { tabWidth :: !Int,
    sourceName :: !Text,
    srcPos :: !SrcPos,
    numSurplusBytes :: !NumSurplusBytes,
    -- | see the documentation for 'NumSurplusBytes' for its interaction with
    -- these bytes
    byte0 :: !Word8,
    byte1 :: !Word8,
    byte2 :: !Word8,
    alexInput :: !Text
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
          '\n' -> sp {srcCol = 1, srcLine = srcLine sp + 1}
          '\t' -> sp {srcCol = srcCol sp + tw - ((srcCol sp - 1) `mod` tw)}
          _ -> sp {srcCol = srcCol sp + 1}
        oc' = ord oc
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
  1 -> Just (b0, ai {numSurplusBytes = 0})
  2 -> Just (b1, ai {numSurplusBytes = 1})
  _ -> Just (b2, ai {numSurplusBytes = 2})

getAlexInputSrcPos :: AlexInput -> SrcPos
getAlexInputSrcPos = srcPos

-- | Get the remainder of the 'Text' stream from an 'AlexInput', implicitly
-- discarding any buffered bytes from an incompletely consumed character
getAlexInputText :: AlexInput -> Text
getAlexInputText = alexInput

getAlexInputSrcName :: AlexInput -> Text
getAlexInputSrcName = sourceName

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
    { sourceName = sname,
      tabWidth = n,
      srcPos = initSrcPos,
      numSurplusBytes = 0,
      byte0 = 0,
      byte1 = 0,
      byte2 = 0,
      alexInput = t
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
  Left ParseError -> error "something went wrong"
  Right a -> a
