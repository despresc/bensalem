{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Common test definitions
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module Test.Common where

import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Text.Pretty.Simple

-- | Either throw a shown @Left@ or return a @Right@
selectRight :: (b -> String) -> (a -> Either b c) -> a -> IO c
selectRight shw f a = case f a of
  Left e -> error $ shw e
  Right c -> pure c

-- | Show the input as a lazy text value
tlshow :: Show a => a -> TL.Text
tlshow = TL.pack . show

tightIndent :: OutputOptions
tightIndent = defaultOutputOptionsNoColor {outputOptionsIndentAmount = 1}

-- | Show the input as a tightly-indented lazy bytestring
pShowByte :: Show a => a -> LBS.ByteString
pShowByte = TLE.encodeUtf8 . pShowOpt tightIndent

-- | Show the input as a tightly-indented string
pShowStr :: Show a => a -> String
pShowStr = TL.unpack . pShowOpt tightIndent

-- TODO HERE: need to fix the goldWith stuff (since I think that I want custom
-- file extensions for the golden tests)

-- | A common simple scheme for golden tests
goldWith ::
  TestName ->
  TestName ->
  TestName ->
  TestName ->
  (Text -> IO LBS.ByteString) ->
  TestTree
goldWith base suffix name desc act =
  goldenVsString (name <> ": " <> desc) (base <> name <> ".golden") $ do
    t <- T.readFile $ base <> name <> "." <> suffix
    act t
