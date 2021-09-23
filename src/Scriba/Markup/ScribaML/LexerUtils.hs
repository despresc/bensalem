-- |
-- Description : Lexer definitions
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- Definitions for the scriba lexer in @Lexer.x@
module Scriba.Markup.ScribaML.LexerUtils
  ( -- * Source positions
    SrcPos (..),
    initSrcPos,

    -- * Alex input
    AlexInput,
    getAlexInputSrcPos,
    getAlexInputText,
    initAlexInput,
    fromAlexInput,
    alexGetByte,
  )
where

import qualified Data.Bits as Bits
import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)

-- | Keeps track of the number of bytes leftover from an incompletely consumed
-- 'Char'. Note that this is effectively an index into the list 'byte0',
-- 'byte1', 'byte2', so in particular if we have three surplus bytes then
-- 'byte2' is consumed first. This is also reflected in the ordering of the list
-- that 'fromAlexInput' returns.
type NumSurplusBytes = Word8

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

-- | The input for alex, keeping track of 'Text' input as if it were a stream of
-- bytes in the UTF-8 encoding, and also the current position in the input and
-- tab width.
data AlexInput = AlexInput
  { tabWidth :: !Int,
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

getAlexInputSrcPos :: AlexInput -> SrcPos
getAlexInputSrcPos = srcPos

-- | Get the remainder of the 'Text' stream from an 'AlexInput', implicitly
-- discarding any buffered bytes from an incompletely consumed character
getAlexInputText :: AlexInput -> Text
getAlexInputText = alexInput

-- | Pop the first byte from the given 'AlexInput', if it is non-empty,
-- considering the 'Text' input as a stream of bytes encoded in UTF-8. Adapted
-- from the basic wrapper in alex.
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte ai@(AlexInput tw sp ns b0 b1 b2 inp) = case ns of
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
              AlexInput tw sp' 0 0 0 0 inp'
            )
          | oc' <= 0x7ff =
            ( fromIntegral $ 0xc0 + (oc' `Bits.shiftR` 6),
              AlexInput tw sp' 1 b0' 0 0 inp'
            )
          | oc' <= 0x7fff =
            ( fromIntegral $ 0xe0 + (oc' `Bits.shiftR` 12),
              AlexInput tw sp' 2 b0' b1' 0 inp'
            )
          | otherwise =
            ( fromIntegral $ 0xf0 + (oc' `Bits.shiftR` 18),
              AlexInput tw sp' 3 b0' b1' b2' inp'
            )
    Nothing -> Nothing
  1 -> Just (b0, ai {numSurplusBytes = 0})
  2 -> Just (b1, ai {numSurplusBytes = 1})
  _ -> Just (b2, ai {numSurplusBytes = 2})

-- | Initialize the 'AlexInput'
initAlexInput ::
  -- | tab width
  Int ->
  -- | initial input
  Text ->
  AlexInput
initAlexInput n t =
  AlexInput
    { tabWidth = n,
      srcPos = initSrcPos,
      numSurplusBytes = 0,
      byte0 = 0,
      byte1 = 0,
      byte2 = 0,
      alexInput = t
    }

-- | Decompose the 'AlexInput' into the tab width, current source position, a
-- list of bytes from an incompletely consumed initial character, and the
-- remainder of the 'Text' input
fromAlexInput :: AlexInput -> (Int, SrcPos, [Word8], Text)
fromAlexInput (AlexInput tw sp ns b0 b1 b2 t) = (tw, sp, ret, t)
  where
    ret = case ns of
      0 -> []
      1 -> [b0]
      2 -> [b1, b0]
      _ -> [b2, b1, b0]
