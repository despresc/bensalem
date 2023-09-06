{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Tests of parsing up to syntax
-- Copyright   : 2022 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module Bensalem.Markup.BensalemML.SyntaxSpec where

import Bensalem.Markup.BensalemML.Parser (parseNodes)
import Test.Common
import Test.Tasty

spec :: TestTree
spec =
  testGroup
    "Syntax"
    [ goldParse "simple-inlines" "parses a variety of simple inlines",
      goldParse "inline-with-attrs" "parses an inline with attributes",
      goldParse "inline-with-arg" "parses an inline with argument",
      goldParse "inline-full" "parses a full inline",
      goldParse "layout-simple" "parses a simple layout element",
      goldParse "layout-nested" "parses layout element nested in another",
      goldParse "level-simple" "parses a simple level element",
      goldParse "level-nested" "parses nested level elements",
      goldParse
        "level-blank-1"
        "parses whitespace before level tag that closes a layout and level tag at once"
    ]
  where
    base = "test/golden/Bensalem/Markup/BensalemML/SyntaxSpec/"
    gold = goldWith base "bsm"
    parse' = fmap pShowByte . parseNodes ""
    goldParse x y = gold x y $ selectRight show parse'
