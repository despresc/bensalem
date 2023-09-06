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
    [ goldParse "simple-inlines" "parses a variety of simple inlines",
      goldParse "inline-with-attrs" "parses an inline with attributes",
      goldParse "inline-with-arg" "parses an inline with argument",
      goldParse "inline-full" "parses a full inline"
    ]
  where
    base = "test/golden/Bensalem/Markup/BensalemML/SyntaxSpec/"
    gold = goldWith base "bsm"
    parse' = fmap pShowByte . parseNodes ""
    goldParse x y = gold x y $ selectRight show parse'
