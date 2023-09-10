{-# LANGUAGE BangPatterns #-}

-- |
-- Description : Names and resolution
-- Copyright   : 2023 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- Functions for resolving names in a bensalem document. Currently we only
-- support the direct use of our (temporary) built-ins with no imports, local
-- definitions, or namespaces, and so name resolution is very easy.
module Bensalem.Markup.BensalemML.Name
  ( -- * Names and built-in resolvers
    Name,
    NameVariety (..),
    nameVariety,
    resolveWiredIn,
    resolveByWiredOnly,
    resolveNodesByWiredOnly,
  )
where

import Bensalem.Markup.BensalemML.Syntax
  ( Node,
    SrcName,
    srcNameStr,
  )
import Bensalem.Markup.BensalemML.WiredIn
import Data.Sequence (Seq)

data Name = Name
  { -- | what the name is
    nameVariety :: !NameVariety,
    -- | what the name looked like in source
    nameStr :: !SrcName
  }
  deriving (Eq, Ord, Show)

data NameVariety
  = -- | a wired-in name
    NameWiredIn !WiredIn
  deriving (Eq, Ord, Show)

resolveByWiredOnly :: Node SrcName -> Either SrcName (Node Name)
resolveByWiredOnly = traverse go
  where
    go sn = case resolveWiredIn $ srcNameStr sn of
      Just nv -> Right $ Name (NameWiredIn nv) sn
      Nothing -> Left sn

resolveNodesByWiredOnly :: Seq (Node SrcName) -> Either SrcName (Seq (Node Name))
resolveNodesByWiredOnly = traverse resolveByWiredOnly
