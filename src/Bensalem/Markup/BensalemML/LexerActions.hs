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
    doLayoutTag,
    doLevelTag,
    doStartBraceGroup,
    doEndBraceGroup,
    doStartAttrSet,
    doEndAttrSet,
    doBlanks,
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
import Data.Functor (($>))
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

-- | Construct a zero-width 'Tok.EndImplicitScope' at the start of the given 'SrcSpan'
conVirtual :: SrcSpan -> Located Token
conVirtual sp = Located sp' Tok.EndImplicitScope
  where
    sp' = sp {srcSpanEnd = srcSpanStart sp}

data ScopePop a
  = -- | continue with scope popping
    ScopeContinue a
  | -- | consume this last scope and stop
    ScopeStop a
  | -- | preserve this last scope and stop
    ScopeEnd

-- | Resolve scopes using the given scope resolving function, returning a
-- difference list of the results. Note that @gatherScopesDL f act@ has the
-- property that if @f@ never returns 'ScopeEnd' then the returned difference
-- list will always be non-empty. It is also true that that returned difference
-- list will be non-empty when given a non-empty input.
--
-- This function handles the scope start code restoration for you as well.

-- TODO: anywhere this is used is not particularly elegant because we really
-- want a (_ -> NonEmpty a) return value. guaranteeing that is a bit of a pain,
-- though.

-- TODO: if we ever get a "global" scope, then we won't need the action to run
-- at the end, because we can just decide to do something upon encountering the
-- global scope
gatherScopesDL ::
  -- | function to resolve one scope
  (Scope -> Parser (ScopePop a)) ->
  -- | action to run if the end of the scope stack is reached
  Parser () ->
  Parser ([a] -> [a])
gatherScopesDL f globalScopeAct = do
  scopes <- gets parseStateScopeStack
  (dl, scopes') <- go id scopes
  setScopeStack scopes'
  pure dl
  where
    go acc (s : ss) = do
      pa <- f s
      case pa of
        ScopeContinue a -> do
          setCode $ scopeAmbientStartCode s
          go (acc . (a :)) ss
        ScopeStop a -> do
          setCode $ scopeAmbientStartCode s
          pure (acc . (a :), ss)
        ScopeEnd -> pure (acc, s : ss)
    go acc [] = globalScopeAct $> (acc, [])

-- | Unsafely extract the head and tail of a non-empty difference list. You must
-- be able to prove that the input is in fact non-empty! See the documentation
-- for 'gatherScopesDL' for some facts you can use to do this, since this is
-- only ever used on the (derivatives of) that function.
--
-- Obviously this is all kind of a hack, and with some effort (and probably less
-- elegance) the use of this function can be eliminated.
unsafeNonEmptyDL :: ([a] -> [a]) -> (a, [a])
unsafeNonEmptyDL l = case l [] of
  x : xs -> (x, xs)
  [] -> error "Internal error - difference list cannot be empty"

-- | Resolve all active level scopes with depth greater than or equal to the
-- given 'Int', using the passed 'Text' and 'SrcPos' for the location of the
-- emitted virtual tokens. Also takes in the level token itself and returns the
-- appropriate token to be emitted immediately by the lexer.
resolveLevelScopes ::
  -- | the start code to be restored
  Int ->
  SrcSpan ->
  Int ->
  Token ->
  Parser (Located Token)
resolveLevelScopes restoreCode spn n tok = do
  toksDL <- gatherScopesDL willBeClosed (pure ())
  pushScope $ Scope (LevelScope n) restoreCode spn
  let (emitTok, pending) = unsafeNonEmptyDL $ toksDL . (Located spn tok :)
  setPendingTokens pending
  pure emitTok
  where
    willBeClosed scope
      | LevelScope m <- scopeType scope,
        m >= n =
          pure $ ScopeContinue $ conVirtual spn
      | otherwise = pure ScopeEnd

-- | Handle a line comment
doLineComment :: AlexAction
doLineComment _ sp t = pure $ Located sp $ Tok.LineComment t'
  where
    -- Drop the initial @;;
    t' = T.drop 3 t

-- | Handle a level element tag. Level elements close the scope of all level
-- elements with depth greater than or equal to that level element.
doLevelTag ::
  -- | the start code to be restored on exit
  Int ->
  AlexAction
doLevelTag restoreCode _ sp t =
  case Tok.validEltName tagt of
    Just eltname -> resolveLevelScopes restoreCode sp level $ Tok.LevelTag level eltname
    Nothing -> throwLexError errSp $ InvalidEltName tagt
  where
    -- drop the initial @
    t' = T.drop 1 t
    (nums, tagt) = T.span (== '#') t'
    -- the lexer guarantees (must guarantee) that this is greater than zero
    level = T.length nums
    errSp = sp {srcSpanStart = spStart {srcCol = srcCol spStart + level}}
      where
        spStart = srcSpanStart sp

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
  toksDL <- gatherScopesDL willBeClosed globalAct
  -- since willBeClosed never returns ScopeEnd, this is total (see note in
  -- gatherScopesDL documentation)
  let (emitTok, pending) = unsafeNonEmptyDL toksDL
  setPendingTokens pending
  pure emitTok
  where
    tokEnd = conVirtual sp
    tokBrace = Located sp Tok.EndBraceGroup
    willBeClosed scope = case scopeType scope of
      BraceScope -> pure $ ScopeStop tokBrace
      AttrSetScope -> throwLexError sp $ AttrBraceMismatch $ scopePos scope
      LayoutScope _ ambient -> do
        setLayoutDepth ambient
        pure $ ScopeContinue tokEnd
      LevelScope _ -> pure $ ScopeContinue tokEnd
    globalAct = throwLexError sp UnmatchedEndBraceGroup

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
  toksDL <- gatherScopesDL willBeClosed globalAct
  let (emitTok, pending) = unsafeNonEmptyDL toksDL
  setPendingTokens pending
  pure emitTok
  where
    -- I'm unsure if it is possible for us ever to encounter layout or level
    -- scopes when resolving the end of an attribute set, or if the
    -- UnmatchedEndAttrSet error will ever be relevant given our lexing
    -- strategy. possibly it's best to remove some of these.
    willBeClosed scope = case scopeType scope of
      AttrSetScope -> pure $ ScopeStop $ Located sp Tok.EndAttrSet
      BraceScope -> throwLexError sp $ BraceAttrMismatch $ scopePos scope
      LayoutScope _ _ -> throwLexError sp $ LayoutAttrMismatch $ scopePos scope
      LevelScope _ -> throwLexError sp $ LevelAttrMismatch $ scopePos scope
    globalAct = throwLexError sp UnmatchedEndAttrSet

{-

TODO HERE: I've added attr set handling to the existing functions. now I need to

1. write the begin/end attr set scope handling functions

2. add a new lexer state for "immediately after tag", probably, so we can know
   that a '[' is significant and actually starts an attribute set scope.

-}

-- | Handle the start of a layout block, pushing a new 'LayoutScope' onto the
-- stack and setting the new layout depth
doLayoutTag ::
  -- | the start code to be restored on scope exit
  Int ->
  AlexAction
doLayoutTag restoreCode _ sp t = case Tok.validEltName tagt of
  Just eltname -> do
    ambient <- gets parseStateLayoutDepth
    pushScope $ Scope (LayoutScope layoutDepth ambient) restoreCode sp
    setLayoutDepth layoutDepth
    pure $ Located sp $ Tok.LayoutTag eltname
  Nothing -> throwLexError errSp $ InvalidEltName tagt
  where
    layoutDepth = srcCol $ srcSpanStart sp
    -- drop the initial @&
    tagt = T.drop 2 t
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
-- is thrown if a brace scope is encountered during this process.

{-

N.B. We operate under the principle that the blanks that this function handles
will be included inside the first scope that the blanks do not participate in
closing. This makes sense, and is only slightly weird (if at all) in two
scenarios:

1. In the input:

--------
@#lvl1

@##lvl2

@#lvl3
--------

the blanks between lvl2 and lvl3 get lexed as if they belonged to lvl2.

2. At the end of input, trailing blanks will always be parsed as if they
belonged to the global implicit scope, so in particular in the snippet

--------
@#lvl1

@##lvl2

@#lvl3

--------

the trailing blanks get lexed as if they did /not/ belong to any of the elements
lvl1, lvl2, or lvl3.

In both cases this does not matter at all, since we strip all initial and final
whitespace from level elements anyway, but it does mean that there might
sometimes be odd trailing whitespace hanging around at the end of input. This
can be fixed fairly in a variety of ways, but it's very inconsequential at the
moment.

-}

doBlanks :: AlexAction
doBlanks _ sp t = do
  -- if we're about to hit the end of input, then we need to close every level
  -- and layout scope.
  let willBeClosedEOF scope = case scopeType scope of
        LevelScope _ -> pure $ ScopeContinue $ conVirtual sp
        LayoutScope _ _ -> pure $ ScopeContinue $ conVirtual sp
        BraceScope -> throwLexError sp $ UnmatchedStartBraceGroup (scopePos scope)
        AttrSetScope -> throwLexError sp $ UnmatchedStartAttrSet (scopePos scope)
  let incomingColumn = srcCol $ srcSpanEnd sp
  let willBeClosedOther scope = do
        layoutDepth <- gets parseStateLayoutDepth
        if layoutDepth >= incomingColumn
          then case scopeType scope of
            LevelScope _ -> pure $ ScopeContinue $ conVirtual sp
            LayoutScope _ ambientLvl -> do
              setLayoutDepth ambientLvl
              pure $ ScopeContinue $ conVirtual sp
            BraceScope -> throwLexError sp $ DeIndentInBracedGroup $ scopePos scope
            AttrSetScope -> throwLexError sp $ DeIndentInAttrSet $ scopePos scope
          else pure ScopeEnd
  currInput <- gets $ alexInputText . parseStateInput
  let willBeClosed
        | T.null currInput = willBeClosedEOF
        | otherwise = willBeClosedOther
  toksDL <- gatherScopesDL willBeClosed $ pure ()
  let (emitTok, pending) = unsafeNonEmptyDL $ toksDL . (Located sp (Tok.Blanks t) :)
  setPendingTokens pending
  pure emitTok

-- | Resolve all pending scopes, throwing a lexer error if there are any pending
-- braced group or attribute set scopes
doEOF :: Parser (Located Token)
doEOF = do
  inp <- gets parseStateInput
  let sp = alexInputSrcPos inp
  let srcSpan = SrcSpan (alexInputSrcName inp) sp sp
  let willBeClosed scope = case scopeType scope of
        LayoutScope _ _ -> pure $ ScopeContinue $ conVirtual srcSpan
        LevelScope _ -> pure $ ScopeContinue $ conVirtual srcSpan
        BraceScope -> throwLexError srcSpan $ UnmatchedStartBraceGroup (scopePos scope)
        AttrSetScope -> throwLexError srcSpan $ UnmatchedStartAttrSet (scopePos scope)
  toksDL <- gatherScopesDL willBeClosed $ pure ()
  let (emitTok, pending) = unsafeNonEmptyDL $ toksDL . (Located srcSpan Tok.TokenEOF :)
  setPendingTokens pending
  pure emitTok
