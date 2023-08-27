-- |
-- Description : Bensalem test suite
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- The entry point to the test suite
module Main where

import qualified Bensalem.Markup.BensalemML.SyntaxSpec as SyntaxSpec
import qualified Bensalem.Markup.BensalemML.TokenSpec as TokenSpec
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ TokenSpec.spec,
      SyntaxSpec.spec
    ]
