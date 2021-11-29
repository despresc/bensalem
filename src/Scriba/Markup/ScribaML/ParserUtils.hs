{-# LANGUAGE BangPatterns #-}
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
    setInput,
    ParseState (..),
    initParseState,
    ParseError (..),
    LexError (..),
    throwLexError,

    -- ** Lexer actions
    AlexAction,
    thenCode,
    textTok,
    plainTok,
    doInlineTag,
    doLayoutTag,
    doLevelTag,
    doStartBraceGroup,
    doEndBraceGroup,
    doStartAttrSet,
    doEndAttrSet,
    doBlanks,
    doVerbatimBlanks,
    doLineComment,
    doStartInlineVerbatim,
    doEndInlineVerbatim,
    doEOF,

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
import Control.Monad.State.Strict
  ( MonadState (..),
    StateT (..),
    evalStateT,
    gets,
    modify,
  )
import qualified Data.Bits as Bits
import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Scriba.Markup.ScribaML.Token (Token)
import qualified Scriba.Markup.ScribaML.Token as Tok

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

setInput :: AlexInput -> Parser ()
setInput ai = modify $ \ps -> ps {parseStateInput = ai}

-- | The type of alex actions, taking in the length of the token, the span it
-- occupies in the source input, and the actual 'Text' of the token
type AlexAction = Int -> SrcSpan -> Text -> Parser (Located Token)

setCode :: Int -> Parser ()
setCode n = modify $ \ps -> ps {parseStateStartCode = n}

thenCode :: AlexAction -> Int -> AlexAction
thenCode f cd n sp t = f n sp t <* setCode cd
{-# INLINE thenCode #-}

plainTok :: Token -> AlexAction
plainTok tok _ sp _ = pure $ Located sp tok

textTok :: (Text -> Token) -> AlexAction
textTok tok _ sp t = pure $ Located sp $ tok t

-- | Evaluate the given parser action starting at the initial lexing state
evalParser :: Parser a -> AlexInput -> Either ParseError a
evalParser = go . evalStateT . unParser
  where
    go f = runExcept . f . initParseState

data ParseState = ParseState
  { -- | the current lexer code
    parseStateStartCode :: !Int,
    -- | the current input
    parseStateInput :: !AlexInput,
    -- | any pending tokens to be emitted before further consumption of input
    parseStatePendingTokens :: ![Located Token],
    -- | a possible pending 'Tok.Indent' token before an upcoming 'Tok.NumTag'
    parseStatePendingIndent :: !(Maybe (Located Token)),
    -- | the current layout depth (ambient indentation)
    parseStateLayoutDepth :: !Int,
    -- | the current stack of scopes
    parseStateScopeStack :: ![Scope],
    -- | the location of the start of verbatim span, if we are in one
    parseStateStartVerbatimLoc :: Maybe SrcSpan
  }
  deriving (Eq, Ord, Show)

data Scope = Scope
  { scopeType :: !ScopeType,
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

-- | Push a scope on top of the scope stack (and nothing else)
pushScope :: Scope -> Parser ()
pushScope s = modify $ \x -> x {parseStateScopeStack = s : parseStateScopeStack x}

setLayoutDepth :: Int -> Parser ()
setLayoutDepth s = modify $ \x -> x {parseStateLayoutDepth = s}

setScopeStack :: [Scope] -> Parser ()
setScopeStack s = modify $ \x -> x {parseStateScopeStack = s}

setPendingTokens :: [Located Token] -> Parser ()
setPendingTokens toks = modify $ \s -> s {parseStatePendingTokens = toks}

setPendingIndent :: Located Token -> Parser ()
setPendingIndent t = modify $ \s -> s {parseStatePendingIndent = Just t}

setStartVerbatim :: SrcSpan -> Parser ()
setStartVerbatim sp = modify $ \s -> s {parseStateStartVerbatimLoc = Just sp}

-- | Construct a zero-width 'Tok.EndImplicitScope' at the start of the given 'SrcSpan'
conVirtual :: SrcSpan -> Located Token
conVirtual sp = Located sp' Tok.EndImplicitScope
  where
    sp' = sp {srcSpanEnd = srcSpanStart sp}

-- | Resolve all active level scopes with depth greater than or equal to the
-- given 'Int', using the passed 'Text' and 'SrcPos' for the location of the
-- emitted virtual tokens. Also takes in the level token itself and returns the
-- appropriate token to be emitted immediately by the lexer.
resolveLevelScopes :: SrcSpan -> Int -> Token -> Parser (Located Token)
resolveLevelScopes spn n tok = do
  scopes <- gets parseStateScopeStack
  let (pendingTokLen, newScopes) = popLevelLen scopes
  setScopeStack newScopes
  pushScope $ Scope (LevelScope n) spn
  pendingIndent <- gets parseStatePendingIndent
  let (emitTok, toks) = replicateVirtuals' spn pendingIndent tok pendingTokLen
  setPendingTokens toks
  pure emitTok
  where
    popLevelLen = popLevelLen' (0 :: Int)
    popLevelLen' !len ss
      | scope : scopes <- ss,
        LevelScope m <- scopeType scope,
        m >= n =
        popLevelLen' (len + 1) scopes
      | otherwise = (len, ss)

-- | Handle a line comment
doLineComment :: AlexAction
doLineComment _ sp t = pure $ Located sp $ Tok.LineComment $ T.drop 2 t

-- | Handle a level element tag. Level elements close the scope of all level
-- elements with depth greater than or equal to that level element.
doLevelTag :: AlexAction
doLevelTag _ sp t =
  case Tok.validEltName tagt of
    Just eltname -> resolveLevelScopes sp level $ Tok.LevelTag level eltname
    Nothing -> throwLexError errSp $ InvalidEltName tagt
  where
    (nums, tagt) = T.span (== '#') t
    level = T.length nums
    errSp = sp {srcSpanStart = spStart {srcCol = srcCol spStart + level}}
      where
        spStart = srcSpanStart sp

-- | Handle the start of a braced group. The start of a braced group opens a
-- braced group scope.
doStartBraceGroup :: AlexAction
doStartBraceGroup _ sp _ = do
  pushScope $ Scope BraceScope sp
  pure $ Located sp Tok.StartBraceGroup

-- | Handle the start of an attribute set. The start of an attribute set opens
-- an attribute set scope.
doStartAttrSet :: AlexAction
doStartAttrSet _ sp _ = do
  pushScope $ Scope AttrSetScope sp
  pure $ Located sp Tok.StartAttrSet

-- | Handle the end of a braced group. The end of a braced group resolves all
-- layout and level scopes, then resolves a single braced scope. A lexical error
-- is thrown if an attribute set scope is encountered before a braced scope is
-- seen, or if no braced scope is found.
doEndBraceGroup :: AlexAction
doEndBraceGroup _ sp _ = do
  scopes <- gets parseStateScopeStack
  resolveScopes scopes
  where
    tokEnd = conVirtual sp
    tokBrace = Located sp Tok.EndBraceGroup
    resolveScopes (scope : scopes) = case scopeType scope of
      BraceScope -> do
        setScopeStack scopes
        pure tokBrace
      LayoutScope _ ambient -> do
        setLayoutDepth ambient
        resolveScopes' [tokBrace] scopes
      LevelScope _ -> resolveScopes' [tokBrace] scopes
      AttrSetScope -> throwLexError sp $ AttrBraceMismatch $ scopePos scope
    resolveScopes [] = throwLexError sp UnmatchedEndBraceGroup
    resolveScopes' acc (scope : scopes) = case scopeType scope of
      BraceScope -> do
        setScopeStack scopes
        setPendingTokens acc
        pure tokEnd
      LayoutScope _ ambient -> do
        setLayoutDepth ambient
        resolveScopes' (tokEnd : acc) scopes
      LevelScope _ -> resolveScopes' (tokEnd : acc) scopes
      AttrSetScope -> throwLexError sp $ AttrBraceMismatch $ scopePos scope
    resolveScopes' _ [] = throwLexError sp UnmatchedEndBraceGroup

-- | Handle the end of an attribute set. The end of an attribute set resolves
-- all layout and level scopes, then resolves a single attribute set scope. A
-- lexical error is thrown if an attribute set scope is encountered before a
-- braced scope is seen, or if no attribute set scope is found.
doEndAttrSet :: AlexAction
doEndAttrSet _ sp _ = do
  scopes <- gets parseStateScopeStack
  resolveScopes scopes
  where
    tokEnd = conVirtual sp
    tokBracket = Located sp Tok.EndAttrSet
    resolveScopes (scope : scopes) = case scopeType scope of
      AttrSetScope -> do
        setScopeStack scopes
        pure tokBracket
      LayoutScope _ ambient -> do
        setLayoutDepth ambient
        resolveScopes' [tokBracket] scopes
      LevelScope _ -> resolveScopes' [tokBracket] scopes
      BraceScope -> throwLexError sp $ BraceAttrMismatch $ scopePos scope
    resolveScopes [] = throwLexError sp UnmatchedEndAttrSet
    resolveScopes' acc (scope : scopes) = case scopeType scope of
      AttrSetScope -> do
        setScopeStack scopes
        setPendingTokens acc
        pure tokEnd
      LayoutScope _ ambient -> do
        setLayoutDepth ambient
        resolveScopes' (tokEnd : acc) scopes
      LevelScope _ -> resolveScopes' (tokEnd : acc) scopes
      BraceScope -> throwLexError sp $ BraceAttrMismatch $ scopePos scope
    resolveScopes' _ [] = throwLexError sp UnmatchedEndAttrSet

-- | Handle the start of a layout block, pushing a new 'LayoutScope' onto the
-- stack and setting the new layout depth
doLayoutTag :: AlexAction
doLayoutTag _ sp t = case Tok.validEltName tagt of
  Just eltname -> do
    ambient <- gets parseStateLayoutDepth
    pushScope $ Scope (LayoutScope layoutDepth ambient) sp
    setLayoutDepth layoutDepth
    pure $ Located sp $ Tok.LayoutTag eltname
  Nothing -> throwLexError errSp $ InvalidEltName tagt
  where
    layoutDepth = srcCol $ srcSpanStart sp
    tagt = T.drop 1 t
    errSp = sp {srcSpanStart = spStart {srcCol = srcCol spStart + 1}}
      where
        spStart = srcSpanStart sp

doInlineTag :: AlexAction
doInlineTag _ sp t = case Tok.validEltName tagt of
  Just eltname -> pure $ Located sp $ Tok.InlineTag eltname
  Nothing -> throwLexError errSp $ InvalidEltName tagt
  where
    tagt = T.drop 1 t
    errSp = sp {srcSpanStart = spStart {srcCol = srcCol spStart + 1}}
      where
        spStart = srcSpanStart sp

-- | Handle an 'Indent' token, which is a sequence of blank lines followed by a
-- run of spaces. If the 'Indent' token is not followed by the end of input then
-- the length of that run is its layout depth, and otherwise its layout depth is
-- equal to 1. An indentation token resolves all layout and level scopes until
-- the current layout depth is less than the level of the token. A lexical error
-- is thrown if a brace or attribute scope is encountered during this process.
-- This action must also look ahead to see if there is an upcoming level tag,
-- and if so, potentially delay the emission of the indent token to just before
-- the emission of that level tag.
doBlanks :: AlexAction
doBlanks _ sp t = do
  scopes <- gets parseStateScopeStack
  currInput <- gets $ alexInput . parseStateInput
  -- delicate handling: if we're just before EOF then any trailing spaces must
  -- be considered to be part of a blank line, and hence the Indent token in
  -- question must have a layout depth of 1 regardless of what its ending column
  -- is. the correct layout depth is 1 because EOF closes all pending layout and
  -- level scopes. we may also need to delay the emission of the indent if we
  -- are about to lex the start of a level element, hence the lookahead for that
  -- too.
  let (tokLevel, upcomingLevelDepth) =
        case T.uncons currInput of
          Just ('#', _) -> (srcCol $ srcSpanEnd sp, Just lvl)
          Just _ -> (srcCol $ srcSpanEnd sp, Nothing)
          Nothing -> (1, Nothing)
        where
          lvl = T.length $ T.takeWhile (== '#') currInput
  go upcomingLevelDepth tokLevel (0 :: Int) scopes
  where
    cleanUp Nothing numVirtuals = do
      let (tok, toks) = replicateVirtuals sp (Tok.Blanks t) numVirtuals
      setPendingTokens toks
      pure tok
    -- if we have a pending level scope that would be closed by the upcoming
    -- level tag, we must delay the emission of the indent token, so that after
    -- we lex that token we can close all of the level scopes as appropriate,
    -- emit the delayed indent token, then finally emit the level tag. we happen
    -- to resolve a single level scope ahead of time here, just so that we are
    -- guaranteed to have something to emit. this does slightly change where a
    -- level tag name lexer error will occur in the token stream, but this
    -- shouldn't really matter much in practice.
    cleanUp (Just upcomingLevelDepth) numVirtuals = do
      scopes <- gets parseStateScopeStack
      case scopes of
        scope : scopes'
          | LevelScope m <- scopeType scope,
            m >= upcomingLevelDepth -> do
            let toks = replicate numVirtuals $ conVirtual sp
            setPendingIndent $ Located sp $ Tok.Blanks t
            setPendingTokens toks
            setScopeStack scopes'
            pure $ conVirtual sp
        _ -> do
          let (tok, toks) = replicateVirtuals sp (Tok.Blanks t) numVirtuals
          setPendingTokens toks
          pure tok
    go upcomingLevelDepth tokLevel !numVirtuals ss@(scope : scopes) = do
      lvl <- gets parseStateLayoutDepth
      if lvl >= tokLevel
        then case scopeType scope of
          -- if we reach this point then this is the LayoutScope that defines
          -- the current layout depth, and so we already know it ought to be
          -- closed
          LayoutScope _ ambientLevel -> do
            setLayoutDepth ambientLevel
            go upcomingLevelDepth tokLevel (numVirtuals + 1) scopes
          LevelScope _ ->
            go upcomingLevelDepth tokLevel (numVirtuals + 1) scopes
          BraceScope -> throwLexError sp $ DeIndentInBracedGroup $ scopePos scope
          AttrSetScope -> throwLexError sp $ DeIndentInAttrSet $ scopePos scope
        else do
          setScopeStack ss
          cleanUp upcomingLevelDepth numVirtuals
    go upcomingLevelDepth _ !numVirtuals [] = do
      setScopeStack []
      cleanUp upcomingLevelDepth numVirtuals

-- | Handle an 'Indent' token while inside a verbatim span. The handling
-- required here is simpler than in 'doIndent', since de-indents are forbidden
-- within verbatim spans and nothing in a verbatim span can create nested
-- scopes.
doVerbatimBlanks :: AlexAction
doVerbatimBlanks _ sp t = do
  let tokLevel = srcCol $ srcSpanEnd sp
  depth <- gets parseStateLayoutDepth
  if depth < tokLevel
    then pure $ Located sp $ Tok.Blanks t
    else do
      mstart <- gets parseStateStartVerbatimLoc
      case mstart of
        Just spStart -> throwLexError sp $ DeIndentInVerbatimSpan spStart
        Nothing -> error "internal error - doVerbatimIndent"

-- | Given a 'Token' and the 'SrcSpan' that it spans, create the indicated
-- number of virtual tokens at the start of the 'SrcSpan', returning the first
-- token to be emitted and a list of pending tokens.
replicateVirtuals :: SrcSpan -> Token -> Int -> (Located Token, [Located Token])
replicateVirtuals sp tok n
  | n <= 0 = (locTok, [])
  | otherwise = go 1 id
  where
    tokEnd = conVirtual sp
    locTok = Located sp tok
    go m acc
      | m == n = (tokEnd, acc [locTok])
      | otherwise = go (m + 1) (acc . (tokEnd :))

-- | Like 'replicateVirtuals', but also takes a possibly-pending token to emit
-- just before the passed token.
replicateVirtuals' ::
  SrcSpan ->
  Maybe (Located Token) ->
  Token ->
  Int ->
  (Located Token, [Located Token])
replicateVirtuals' sp Nothing tok n
  | n <= 0 = (locTok, [])
  | otherwise = go 1 id
  where
    tokEnd = conVirtual sp
    locTok = Located sp tok
    go m acc
      | m == n = (tokEnd, acc [locTok])
      | otherwise = go (m + 1) (acc . (tokEnd :))
replicateVirtuals' sp (Just pending) tok n
  | n <= 0 = (pending, [locTok])
  | otherwise = go 1 id
  where
    tokEnd = conVirtual sp
    locTok = Located sp tok
    go m acc
      | m == n = (tokEnd, acc [pending, locTok])
      | otherwise = go (m + 1) (acc . (tokEnd :))

doStartInlineVerbatim :: AlexAction
doStartInlineVerbatim _ sp _ = do
  setStartVerbatim sp
  pure $ Located sp Tok.StartInlineVerbatim

doEndInlineVerbatim :: AlexAction
doEndInlineVerbatim _ sp _ = do
  modify $ \s -> s {parseStatePendingIndent = Nothing}
  pure $ Located sp Tok.EndInlineVerbatim

-- | Resolve all pending scopes, throwing a lexer error if there are any pending
-- braced group or attribute set scopes
doEOF :: Parser (Located Token)
doEOF = do
  inp <- gets parseStateInput
  let sp = getAlexInputSrcPos inp
  let srcSpan = SrcSpan (getAlexInputSrcName inp) sp sp
  scopes <- gets parseStateScopeStack
  numVirtuals <- getNumVirtuals srcSpan scopes
  let (tok, toks) = replicateVirtuals srcSpan Tok.TokenEOF numVirtuals
  setPendingTokens toks
  pure tok
  where
    getNumVirtuals x = getNumVirtuals' x 0
    getNumVirtuals' :: SrcSpan -> Int -> [Scope] -> Parser Int
    getNumVirtuals' endPos !nv (scope : scopes) = case scopeType scope of
      LayoutScope _ _ -> getNumVirtuals' endPos (nv + 1) scopes
      LevelScope _ -> getNumVirtuals' endPos (nv + 1) scopes
      BraceScope -> throwLexError endPos $ UnmatchedStartBraceGroup (scopePos scope)
      AttrSetScope -> throwLexError endPos $ UnmatchedStartAttrSet (scopePos scope)
    getNumVirtuals' _ !nv [] = pure nv

initParseState :: AlexInput -> ParseState
initParseState ai =
  ParseState
    { parseStateStartCode = 0,
      parseStateInput = ai,
      parseStatePendingTokens = [],
      parseStatePendingIndent = Nothing,
      parseStateLayoutDepth = 0,
      parseStateScopeStack = [],
      parseStateStartVerbatimLoc = Nothing
    }

-- | A possible error that can occur during parsing
data ParseError
  = -- | source span where the error occurred (see 'LexError'), lexer error
    LexerError SrcSpan LexError
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
  | -- | a start of attribute set was not matched by an end of attribute set (span
    -- of start of attribute set)
    UnmatchedStartAttrSet SrcSpan
  | -- | an end of brace group was encountered with no matching start of brace
    -- | group
    UnmatchedEndBraceGroup
  | -- | an end of attribute set was encountered with no matching start of attribute set
    UnmatchedEndAttrSet
  | -- | a start of attribute set was ended by an end of braced group (span of
    -- start of attribute set)
    AttrBraceMismatch SrcSpan
  | -- | a start of braced group was ended by an end of attribute set (span of
    -- start of braced group )
    BraceAttrMismatch SrcSpan
  | -- | a de-indent occurred in a braced group (span of start of braced group)
    DeIndentInBracedGroup SrcSpan
  | -- | a de-indent occurred in an attribute set (span of start of attribute
    -- set)
    DeIndentInAttrSet SrcSpan
  | -- | a de-indent occurred in a verbatim span (span of start of verbatim span)
    DeIndentInVerbatimSpan SrcSpan
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
  1 -> Just (b0, ai {numSurplusBytes = 0})
  2 -> Just (b1, ai {numSurplusBytes = 1})
  _ -> Just (b2, ai {numSurplusBytes = 2})

-- | Get the current source position from the 'AlexInput' state
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
  Left e -> error $ show e
  Right a -> a
