{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Tests of expression conversion
-- Copyright   : 2022 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module Bensalem.Markup.BensalemML.CoreSpec where

import Bensalem.Markup.BensalemML.Core (toExpr)
import Bensalem.Markup.BensalemML.Name (resolveNodesByWiredOnly)
import Bensalem.Markup.BensalemML.Parser (parseNodes)
import Test.Common
import Test.Tasty

spec :: TestTree
spec =
  testGroup
    "Core"
    [ goldParse "simple-doc" "turns a simple document into core"
    ]
  where
    base = "test/golden/Bensalem/Markup/BensalemML/CoreSpec/"
    gold = goldWith base "bsm"
    parse' t = do
      ns <- selectRight show (parseNodes "") t
      ns' <- selectRight show resolveNodesByWiredOnly ns
      ns'' <- selectRight show toExpr ns'
      pure $ pShowByte ns''
    goldParse x y = gold x y parse'
