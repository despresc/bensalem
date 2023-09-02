{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Tests of parsing up to syntax
-- Copyright   : 2022 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module Bensalem.Markup.BensalemML.SyntaxSpec where

import Bensalem.Markup.BensalemML.Syntax (parseNodes)
import Test.Common
import Test.Tasty

spec :: TestTree
spec =
  testGroup
    "Syntax"
    [ simpleInlines,
      inlineAttrs
    ]
  where
    base = "test/golden/Bensalem/Markup/BensalemML/SyntaxSpec/"
    gold = goldWith base "bsm"
    parse' = fmap pShowByte . parseNodes ""
    simpleInlines =
      gold "simple-inlines" "parses a variety of simple inlines" $
        selectRight show parse'
    inlineAttrs =
      gold "inline-with-attrs" "parses an inline with attributes" $
        selectRight show parse'
