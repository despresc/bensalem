{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Tests of expression conversion
-- Copyright   : 2022 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module Bensalem.Markup.BensalemML.InterpretSpec where

import Bensalem.Markup.BensalemML.Core (toExpr)
import Bensalem.Markup.BensalemML.Interpreter (interpretExprs)
import Bensalem.Markup.BensalemML.Name (resolveNodesByWiredOnly)
import Bensalem.Markup.BensalemML.Parser (parseNodes)
import Test.Common
import Test.Tasty
import qualified Text.Blaze.Html.Renderer.Utf8 as HtmlR
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL

spec :: TestTree
spec =
  testGroup
    "Interpret"
    [ goldParse "riemann" "turns a simple document into html"
    ]
  where
    base = "test/golden/Bensalem/Markup/BensalemML/InterpretSpec/"
    gold = goldWith' base "html" "bsm"
    prepend x f = ((x <> ": ") <>) . f
    parse' t = do
      ns <- selectRight (prepend "parse" show) (parseNodes "") t
      ns' <- selectRight (prepend "wired" show) resolveNodesByWiredOnly ns
      ns'' <- selectRight (prepend "core" show) toExpr ns'
      ns''' <- selectRight (prepend "interpret" show) interpretExprs ns''
      pure $ HtmlR.renderHtml ns'''
    goldParse x y = gold x y parse'
