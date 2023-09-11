{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Interpreter
-- Copyright   : 2023 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- Proof-of-concept interpreter that (currently) only outputs HTML.
module Bensalem.Markup.BensalemML.Interpreter
  ( interpretExpr,
    interpretExprs,
  )
where

import Bensalem.Markup.BensalemML.Core
import Bensalem.Markup.BensalemML.Name (Name, NameVariety (..), nameVariety)
import Bensalem.Markup.BensalemML.WiredIn (WiredIn (..))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.State.Strict
  ( MonadState,
    StateT,
    gets,
    runStateT,
  )
import Data.Sequence (Seq (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Blaze.Html as Html
import qualified Text.Blaze.Html5 as Html5
import qualified Text.Blaze.Html5.Attributes as Html5A

data CompileState = CompileState
  {csHeadingDepth :: !Int}
  deriving (Eq, Ord, Show)

newtype CompileM a = CompileM {unCompileM :: StateT CompileState (Either InterpError) a}
  deriving (Functor, Applicative, Monad, MonadError InterpError, MonadState CompileState)

initCompileState :: CompileState
initCompileState = CompileState {csHeadingDepth = 0}

class ToHtml a where
  compileHtml :: a -> CompileM Html.Markup

runCompileM :: CompileM a -> Either InterpError a
runCompileM = fmap fst . ($ initCompileState) . runStateT . unCompileM

interpretExpr :: Expr -> Either InterpError Html.Markup
interpretExpr = runCompileM . compileHtml

interpretExprs :: Seq Expr -> Either InterpError Html.Markup
interpretExprs = runCompileM . compileHtml

instance ToHtml Expr where
  compileHtml (PlainText t) = compileHtml t
  compileHtml (LineSpace n) = compileHtml $ T.replicate n " "
  compileHtml LineEnd = pure $ Html.text "\n"
  compileHtml (Group g) = do
    let start = Html.text "{"
    let end = Html.text "}"
    g' <- compileHtml g
    pure $ start <> g' <> end
  compileHtml (App nm attrs args) = case nameVariety nm of
    NameWiredIn wi -> handleWiredIn nm wi attrs args

instance ToHtml Text where
  compileHtml = pure . Html.text

instance (ToHtml a) => ToHtml (Seq a) where
  compileHtml = go mempty
    where
      go !acc (x :<| xs) = do
        x' <- compileHtml x
        go (acc <> x') xs
      go !acc Empty = pure acc

-- Only use this if you happen to know the arity of the thing itself is one!
unsafeOneArg :: Seq a -> a
unsafeOneArg (x :<| Empty) = x
unsafeOneArg _ = error "Internal error"

-- TODO: obviously should go in a file somewhere once we get declarations

-- TODO: we already do arity checking in the core pass, but we might want to add
-- another check here
handleWiredIn :: Name -> WiredIn -> Seq Attr -> Seq Arg -> CompileM Html.Markup
handleWiredIn _ _ (_ :<| _) _ = throwError InterpError
handleWiredIn _ wi Empty args = case wi of
  WIsection -> do
    let content = unsafeOneArg args
    content' <- compileHtml $ snd content
    pure $ Html5.section content'
  WIheading -> do
    let content = unsafeOneArg args
    content' <- compileHtml $ snd content
    -- for now we simply render the depth according to section depth, but we
    -- will want to separate these two things if we have, say, untitled sections
    -- like mainMatter. or we have a titleOf property, so we can look up the
    -- section depth of the referenced thing and render the title accordingly.
    n <- gets csHeadingDepth
    pure $ genHeading n $ content'
  WItable -> directTranslation Html5.table
  WItbody -> directTranslation Html5.tbody
  WIrow -> directTranslation Html5.tr
  WIcell -> directTranslation Html5.td
  WIol -> directTranslation Html5.ol
  WIul -> directTranslation Html5.ul
  WIli -> directTranslation Html5.li
  WIp -> directTranslation Html5.p
  WIemph -> directTranslation Html5.em
  WIphysPage -> directTranslation $ Html5.span Html5.! Html5A.class_ "physPage"
  WIplaceRef -> directTranslation $ Html5.span Html5.! Html5A.class_ "placeRef"
  WIpersonRef -> directTranslation $ Html5.span Html5.! Html5A.class_ "personRef"
  WIlocRef -> directTranslation $ Html5.span Html5.! Html5A.class_ "locRef"
  WItdmath -> directTranslation $ \x -> Html5.text "$$" <> x <> Html5.text "$$"
  WItimath -> directTranslation $ \x -> Html5.text "\\(" <> x <> Html5.text "\\)"
  WIcitation -> directTranslation $ Html5.span Html5.! Html5A.class_ "citation"
  where
    directTranslation f = do
      let content = unsafeOneArg args
      content' <- compileHtml $ snd content
      pure $ f content'
    genHeading n
      | n == 0 = Html5.h1
      | n == 1 = Html5.h2
      | n == 2 = Html5.h3
      | n == 3 = Html5.h4
      | n == 4 = Html5.h5
      | n == 5 = Html5.h6
      | otherwise = Html5.p Html5.! Html5A.class_ "deepHead"
