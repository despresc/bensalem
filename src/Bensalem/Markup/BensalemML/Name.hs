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
  )
where

import Bensalem.Markup.BensalemML.Syntax
  ( Node,
    SrcName,
    srcNameStr,
  )
import Bensalem.Markup.BensalemML.WiredIn

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

resolveByWiredOnly :: Node SrcName -> Maybe (Node Name)
resolveByWiredOnly = traverse go
  where
    go sn = case resolveWiredIn $ srcNameStr sn of
      Just nv -> Just $! Name (NameWiredIn nv) sn
      Nothing -> Nothing
