{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Tokenization tests
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module Scriba.Markup.ScribaML.TokenSpec where

import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Scriba.Markup.ScribaML.Lexer (lexTokens)
import Scriba.Markup.ScribaML.ParserDefs
  ( Located (..),
    ParseError,
    SrcPos (..),
    SrcSpan (..),
    evalParser,
    initAlexInput,
  )
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)

tlshow :: Show a => a -> TL.Text
tlshow = TL.pack . show

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

selectRight :: (b -> String) -> (a -> Either b c) -> a -> IO c
selectRight shw f a = case f a of
  Left e -> error $ shw e
  Right c -> pure c

goldenTests :: TestTree
goldenTests =
  testGroup
    "Golden tests"
    [tokenGolden]
  where
    base = "./test/golden/Scriba/Markup/ScribaML/TokenSpec/"
    gold name desc act =
      goldenVsString (name <> ": " <> desc) (base <> name <> ".golden") $ do
        t <- T.readFile $ base <> name <> ".input"
        act t
    tokenGolden = gold "token" "tokenized properly" $ selectRight show lexTokens'
