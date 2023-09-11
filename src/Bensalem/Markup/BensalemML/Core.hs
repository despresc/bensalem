{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Core
-- Copyright   : 2023 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- Proof-of-concept core that does not do any type checking on the input
-- document and simply operates on the (name-resolved) syntax
module Bensalem.Markup.BensalemML.Core
  ( -- * Core expressions
    Expr (..),
    Attr,
    Arg,
    AttrVal (..),
    toExpr,
    InterpError (..),
  )
where

import Bensalem.Markup.BensalemML.Name (Name, NameVariety (..), nameVariety)
import Bensalem.Markup.BensalemML.ParserDefs
  ( Located,
    SrcSpan,
  )
import Bensalem.Markup.BensalemML.Syntax
  ( Node,
  )
import qualified Bensalem.Markup.BensalemML.Syntax as Syn
import Bensalem.Markup.BensalemML.WiredIn
  ( Arity,
    wiredInArity,
  )
import Data.Sequence (Seq (..))
import Data.Text (Text)

-- TODO: obviously duplicated

-- TODO: technically this is a named argument - should it be a Name? probably
-- should be opaque in any case
type Attr = (Located Text, AttrVal)

data AttrVal
  = AttrSeq !(Seq Expr)
  | AttrSet !(Seq Attr)
  deriving (Eq, Ord, Show)

-- We include the start of the argument

-- TODO: should probably make this its own type, perhaps recording first line of
-- the content or the start of the tag itself as the position of a content
-- argument
type Arg = (ArgLoc, Seq Expr)

data ArgLoc = ArgBraced !SrcSpan | ArgScoped
  deriving (Eq, Ord, Show)

data Expr
  = App !Name !(Seq Attr) !(Seq Arg)
  | -- | unresolved groups
    Group !(Seq Expr)
  | PlainText !Text
  | LineSpace !Int
  | LineEnd
  deriving (Eq, Ord, Show)

-- TODO: replace uses with actual errors
data InterpError = InterpError
  deriving (Eq, Ord, Show)

toExpr :: Seq (Node Name) -> Either InterpError (Seq Expr)
toExpr = toExpr' mempty
  where
    toExpr' !acc (Syn.PlainText t :<| nodes) = toExpr' (acc :|> PlainText t) nodes
    toExpr' !acc (Syn.LineSpace n :<| nodes) = toExpr' (acc :|> LineSpace n) nodes
    toExpr' !acc (Syn.LineEnd :<| nodes) = toExpr' (acc :|> LineEnd) nodes
    toExpr' !acc (Syn.ElementNode n :<| nodes) = do
      (expr, nodes') <- elemToExpr n nodes
      toExpr' (acc :|> expr) nodes'
    toExpr' !acc (Syn.GroupNode g :<| nodes) = do
      g' <- toExpr $ Syn.groupContent g
      let g'' = PlainText "{" :<| (g' :|> PlainText "}")
      toExpr' (acc <> g'') nodes
    toExpr' !acc (Syn.InlineComment _ :<| nodes) = toExpr' acc nodes
    toExpr' !acc Empty = pure acc

elemToExpr ::
  Syn.Element Name ->
  Seq (Node Name) ->
  Either InterpError (Expr, Seq (Node Name))
elemToExpr el nodes = case nameVariety $ Syn.elementName el of
  NameWiredIn wi -> do
    let ar = wiredInArity wi
    (args, nodes') <- grabElemArgs ar (Syn.elementScopeContent el) nodes
    attrs <- attrsToExpr $ Syn.elementAttrs el
    let ex = App (Syn.elementName el) attrs args
    pure (ex, nodes')

isSynBlank :: Node name -> Bool
isSynBlank Syn.LineSpace {} = True
isSynBlank Syn.LineEnd = True
isSynBlank Syn.InlineComment {} = True
isSynBlank Syn.ElementNode {} = False
isSynBlank Syn.GroupNode {} = False
isSynBlank Syn.PlainText {} = False

grabElemArgs ::
  Arity ->
  Syn.ScopeContent Name ->
  Seq (Node Name) ->
  Either InterpError (Seq Arg, Seq (Node Name))
grabElemArgs ar Syn.InlineScopeContent nodes = grabArgs ar nodes
grabElemArgs ar (Syn.LayoutScopeContent inNodes) nodes = do
  args <- grabContentArgs ar inNodes
  pure (args, nodes)
grabElemArgs ar (Syn.LevelScopeContent inNodes) nodes = do
  args <- grabContentArgs ar inNodes
  pure (args, nodes)

grabContentArgs :: Arity -> Seq (Node Name) -> Either InterpError (Seq Arg)
grabContentArgs ar inNodes
  | ar == 0 = do
      verifyBlanks inNodes
      pure mempty
  | otherwise = do
      (firstArgs, inNodes') <- grabArgs (ar - 1) inNodes
      inNodes'' <- toExpr inNodes'
      pure $ firstArgs :|> (ArgScoped, inNodes'')
  where
    verifyBlanks x
      | all isSynBlank x = pure ()
      | otherwise = Left InterpError

grabArgs ::
  Arity ->
  Seq (Node Name) ->
  Either InterpError (Seq Arg, Seq (Node Name))
grabArgs ar = go ar mempty
  where
    go n !acc nodes
      | n <= 0 = pure (acc, nodes)
      | otherwise = do
          (arg, nodes') <- grabArg nodes
          go (n - 1) (acc :|> arg) nodes'
    grabArg (Syn.LineSpace _ :<| nodes) = grabArg nodes
    grabArg (Syn.LineEnd :<| nodes) = grabArg nodes
    grabArg (Syn.GroupNode g :<| nodes) = do
      gcon <- toExpr $ Syn.groupContent g
      pure ((ArgBraced $ Syn.groupStart g, gcon), nodes)
    grabArg _ = Left InterpError

attrsToExpr :: Syn.Attrs Name -> Either InterpError (Seq Attr)
attrsToExpr Syn.NoAttrs = pure mempty
attrsToExpr (Syn.Attrs attrs) = traverse attrToExpr attrs
  where
    attrToExpr (k, v) = do
      v' <- case v of
        Syn.BracedAttrVal ns -> AttrSeq <$> toExpr ns
        Syn.SetAttrVal as -> AttrSet <$> traverse attrToExpr as
      pure (k, v')
