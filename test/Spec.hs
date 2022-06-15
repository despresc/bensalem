-- |
-- Description : Scriba test suite
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- The entry point to the test suite
module Main where

import qualified Scriba.Markup.ScribaML.SyntaxSpec as SyntaxSpec
import qualified Scriba.Markup.ScribaML.TokenSpec as TokenSpec
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
