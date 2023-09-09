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
    resolveWiredIn,
    resolveByWiredOnly,
  )
where

import Bensalem.Markup.BensalemML.Syntax
  ( Node,
    SrcName,
    srcNameStr,
  )
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

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

-- | A wired-in name. These represent simple and not particularly fundamental
-- markup constructs to start, and will change in future.

-- TODO: probably should have this be looser and instead solve this with unique
-- identifiers, but this is fine for now

-- For now, these need to match the source names of the entities, aside from the
-- initial WI prefix

data WiredIn
  = -- | section
    WIsection
  | -- | block of TeX math macro definitions
    WItexmacros
  | -- | heading of a section
    WIheading
  | -- | \"classic\" table
    WItable
  | -- | table body
    WItbody
  | -- | table row
    WIrow
  | -- | ordered list
    WIol
  | -- | unordered list
    WIul
  | -- | list item
    WIli
  | -- | paragraph
    WIp
  | -- | emphasized text
    WIemph
  | -- | a marker for a physical page
    WIphysPage
  | -- | a reference to a place
    WIplaceRef
  | -- | a reference to a person
    WIpersonRef
  | -- | a reference to a physical place
    WIlocRef
  | -- | TeX-style display math
    WItdmath
  | -- | TeX-style inline math
    WItimath
  | -- | citation of something
    WIcitation
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Resolve a 'SrcName' to a wired-in name, if possible

-- TODO: this and the temporary resolvers shouldn't live in this module
-- long-term, I would imagine. probably some kind of name resolution module.
-- that would be more feasilbe if we get rid of the 'WiredIn' type like the note
-- on that type says.
resolveWiredIn :: SrcName -> Maybe Name
resolveWiredIn nm = case M.lookup (srcNameStr nm) wiredInNameMap of
  Just nv -> Just $! Name nv nm
  Nothing -> Nothing

wiredInNameMap :: Map Text NameVariety
wiredInNameMap = M.fromList $ go <$> [minBound .. maxBound]
  where
    go :: WiredIn -> (Text, NameVariety)
    go w =
      let !t = T.pack $ List.drop 2 $ show w
          !n = NameWiredIn w
       in (t, n)

resolveByWiredOnly :: Node SrcName -> Maybe (Node Name)
resolveByWiredOnly = traverse resolveWiredIn
