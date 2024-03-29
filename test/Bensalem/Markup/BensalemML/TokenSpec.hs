{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Tokenization tests
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module Bensalem.Markup.BensalemML.TokenSpec where

import Bensalem.Markup.BensalemML.Lexer (lexTokens)
import Bensalem.Markup.BensalemML.ParserDefs
  ( Located (..),
    ParseError,
    SrcPos (..),
    SrcSpan (..),
    evalParser,
    initAlexInput,
  )
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Lazy.Encoding as TLE
import Test.Common
import Test.Tasty

spec :: TestTree
spec =
  testGroup
    "Tokenization"
    [goldenTests]

-- TODO: throw a nice-looking error
lexTokens' :: Text -> Either ParseError LBS.ByteString
lexTokens' inp = go <$> evalParser lexTokens (initAlexInput 2 "<test>" inp)
  where
    go toks = TLE.encodeUtf8 $ mconcat $ showLocTok <$> toks
    showPos (SrcPos n x y) = tlshow (n, x, y)
    showSrcSpan (SrcSpan _ start end) = showPos start <> showPos end
    showLocTok (Located sp tok) = showSrcSpan sp <> " " <> tlshow tok <> "\n"

goldenTests :: TestTree
goldenTests =
  testGroup
    "Golden tests"
    [ simpleGold "simple-tokens" "tokenizes simple tokens properly",
      simpleGold "simple-attrs" "tokenizes some simple attributes",
      simpleGold "level-closing-layout-1" "tokenizes a tricky level-layout interaction",
      simpleGold "level-closing-layout-2" "tokenizes another tricky level-layout interaction",
      simpleGold "verbatim" "tokenizes a verbatim span correctly"
    ]
  where
    base = "./test/golden/Bensalem/Markup/BensalemML/TokenSpec/"
    gold = goldWith base "input"
    simpleGold x y = gold x y $ selectRight show lexTokens'
