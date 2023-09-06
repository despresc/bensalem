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
    setCode,
    textTok,
    plainTok,
    doInlineTag,
    doAttrKey,
    doStartBraceGroup,
    doEndBraceGroup,
    doStartAttrSet,
    doEndAttrSet,
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

setScopeStack :: [Scope] -> Parser ()
setScopeStack s = modify $ \x -> x {parseStateScopeStack = s}

-- | Simple scope resolution
resolveScope :: SrcSpan -> ScopeType -> Parser ()
resolveScope sp st = gets parseStateScopeStack >>= go
  where
    go (scope : scopes)
      | scopeType scope == st = do
          setCode $ scopeAmbientStartCode scope
          setScopeStack scopes
      | otherwise = throwLexError sp $ ScopeMismatch scope st
    go [] = throwLexError sp $ UnmatchedEndScope st

-- | Handle a line comment
doLineComment :: AlexAction
doLineComment _ sp t = pure $ Located sp $ Tok.LineComment t'
  where
    -- Drop the initial @;;
    t' = T.drop 3 t

-- | Handle the start of a braced group. The start of a braced group opens a
-- braced group scope.
doStartBraceGroup ::
  -- | the start code to be restored on exit
  Int ->
  AlexAction
doStartBraceGroup restoreCode _ sp _ = do
  pushScope $ Scope BraceScope restoreCode sp
  pure $ Located sp Tok.StartBraceGroup

-- | Handle the end of a braced group. The end of a braced group resolves all
-- layout and level scopes, then resolves a single braced scope. A lexical error
-- is thrown if no braced scope is found.
doEndBraceGroup :: AlexAction
doEndBraceGroup _ sp _ = do
  resolveScope sp BraceScope
  pure $ Located sp Tok.EndBraceGroup

doStartAttrSet ::
  -- | the start code to be restored on exit
  Int ->
  AlexAction
doStartAttrSet restoreCode _ sp _ = do
  pushScope $ Scope AttrSetScope restoreCode sp
  pure $ Located sp Tok.StartAttrSet

-- | Handle the end of an attribute set. The end of an attribute set resolves
-- only an end of attribute set scope and nothing else.
doEndAttrSet :: AlexAction
doEndAttrSet _ sp _ = do
  resolveScope sp AttrSetScope
  pure $ Located sp Tok.EndAttrSet

doInlineTag :: AlexAction
doInlineTag _ sp t = case Tok.validEltName tagt of
  Just eltname -> pure $ Located sp $ Tok.InlineTag eltname
  Nothing -> throwLexError errSp $ InvalidEltName tagt
  where
    tagt = T.drop 1 t
    errSp = sp {srcSpanStart = spStart {srcCol = srcCol spStart + 1}}
      where
        spStart = srcSpanStart sp

doAttrKey :: AlexAction
doAttrKey _ sp t = case Tok.validAttrKey t of
  Just k -> pure $ Located sp $ Tok.AttrKey k
  Nothing -> throwLexError sp $ InvalidAttrKey t

-- | Throws a lexer error if there are any pending scopes
doEOF :: Parser (Located Token)
doEOF = do
  inp <- gets parseStateInput
  let sp = alexInputSrcPos inp
  let srcSpan = SrcSpan (alexInputSrcName inp) sp sp
  scopes <- gets parseStateScopeStack
  case scopes of
    scope : _ -> throwLexError srcSpan $ UnmatchedBeginScope scope
    [] -> pure ()
  pure $ Located srcSpan Tok.TokenEOF
