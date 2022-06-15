{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Tests of parsing up to syntax
-- Copyright   : 2022 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module Scriba.Markup.ScribaML.SyntaxSpec where

import Scriba.Markup.ScribaML.Syntax (parseNodes)
import Test.Common
import Test.Tasty

spec :: TestTree
spec =
  testGroup
    "Syntax"
    [ indent1,
      indent2
    ]
  where
    base = "test/golden/Scriba/Markup/ScribaML/SyntaxSpec/"
    gold = goldWith base "scb"
    parse' = fmap pShowByte . parseNodes ""
    indent1 =
      gold "indent1" "indent parsed correctly in layout elements" $
        selectRight show parse'
    indent2 =
      gold "indent2" "indent parsed correctly in level elements" $
        selectRight show parse'
