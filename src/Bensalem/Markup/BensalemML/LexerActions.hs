{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Lexer actions
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- The 'AlexAction' type and actions that handle the lexing of tokens in
-- "Bensalem.Markup.BensalemML.Lexer"
module Bensalem.Markup.BensalemML.LexerActions
  ( AlexAction,
    thenCode,
    textTok,
    plainTok,
    doInlineTag,
    doLayoutTag,
    doLevelTag,
    doStartBraceGroup,
    doEndBraceGroup,
    doStartAttrSet,
    doBlanks,
    doVerbatimBlanks,
    doLineComment,
    doEOF,
  )
where

import Bensalem.Markup.BensalemML.ParserDefs
import Bensalem.Markup.BensalemML.Token (Token)
import qualified Bensalem.Markup.BensalemML.Token as Tok
import Control.Monad.State.Strict
  ( gets,
    modify,
  )
import Data.Text (Text)
import qualified Data.Text as T

-- | The type of alex actions, taking in the length of the token, the span it
-- occupies in the source input, and the actual 'Text' of the token
type AlexAction = Int -> SrcSpan -> Text -> Parser (Located Token)

setCode :: Int -> Parser ()
setCode n = modify $ \ps -> ps {parseStateStartCode = n}

thenCode :: AlexAction -> Int -> AlexAction
thenCode f cd n sp t = f n sp t <* setCode cd

plainTok :: Token -> AlexAction
plainTok tok _ sp _ = pure $ Located sp tok

textTok :: (Text -> Token) -> AlexAction
textTok tok _ sp t = pure $ Located sp $ tok t

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

clearPendingIndent :: Parser ()
clearPendingIndent = modify $ \s -> s {parseStatePendingIndent = Nothing}

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
  let (emitTok, toks) = replicateEndImplicitScope' spn pendingIndent tok pendingTokLen
  setPendingTokens toks
  clearPendingIndent
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
doLineComment _ sp t = pure $ Located sp $ Tok.LineComment t'
  where
    -- Drop the initial @;;
    t' = T.drop 3 t

-- | Handle a level element tag. Level elements close the scope of all level
-- elements with depth greater than or equal to that level element.
doLevelTag :: AlexAction
doLevelTag _ sp t =
  case Tok.validEltName tagt of
    Just eltname -> resolveLevelScopes sp level $ Tok.LevelTag level eltname
    Nothing -> throwLexError errSp $ InvalidEltName tagt
  where
    (nums, tagt) = T.span (== '#') t
    -- the lexer guarantees (must guarantee) that this is greater than zero
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
  currInput <- gets $ alexInputText . parseStateInput
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
      let (tok, toks) = replicateEndImplicitScope sp (Tok.Blanks t) numVirtuals
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
          let (tok, toks) = replicateEndImplicitScope sp (Tok.Blanks t) numVirtuals
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
replicateEndImplicitScope :: SrcSpan -> Token -> Int -> (Located Token, [Located Token])
replicateEndImplicitScope sp tok n
  | n <= 0 = (locTok, [])
  | otherwise = go 1 id
  where
    tokEnd = conVirtual sp
    locTok = Located sp tok
    go m acc
      | m == n = (tokEnd, acc [locTok])
      | otherwise = go (m + 1) (acc . (tokEnd :))

-- | Like 'replicateEndImplicitScope', but also takes a possibly-pending token to emit
-- just before the passed token.
replicateEndImplicitScope' ::
  SrcSpan ->
  Maybe (Located Token) ->
  Token ->
  Int ->
  (Located Token, [Located Token])
replicateEndImplicitScope' sp Nothing tok n = replicateEndImplicitScope sp tok n
replicateEndImplicitScope' sp (Just pending) tok n
  | n <= 0 = (pending, [locTok])
  | otherwise = go 1 id
  where
    -- importantly, we want all of the emitted EndImplicitScope tokens to be
    -- located just before the optional pending token, not the passed token.
    tokEnd = conVirtual $ locatedSpan pending
    locTok = Located sp tok
    go m acc
      | m == n = (tokEnd, acc [pending, locTok])
      | otherwise = go (m + 1) (acc . (tokEnd :))

-- | Resolve all pending scopes, throwing a lexer error if there are any pending
-- braced group or attribute set scopes
doEOF :: Parser (Located Token)
doEOF = do
  inp <- gets parseStateInput
  let sp = alexInputSrcPos inp
  let srcSpan = SrcSpan (alexInputSrcName inp) sp sp
  scopes <- gets parseStateScopeStack
  numVirtuals <- getNumVirtuals srcSpan scopes
  let (tok, toks) = replicateEndImplicitScope srcSpan Tok.TokenEOF numVirtuals
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
