-- |
-- Description : Wired-in name data
-- Copyright   : 2023 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module Bensalem.Markup.BensalemML.WiredIn
  ( WiredIn (..),
    resolveWiredIn,
    wiredInArity,
    Arity,
  )
where

import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

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

-- | the arity of the built-in
type Arity = Int

-- | The arity of a wired-in type. All the built-in types happen to have arity 1
-- at the moment.

-- TODO: obviously we should figure this out from a signature/declaration
-- somewhere and not just have it as a function.
wiredInArity :: WiredIn -> Arity
wiredInArity _ = 1

-- | Resolve a 'SrcName' to a wired-in name, if possible

-- TODO: this and the temporary resolvers shouldn't live in this module
-- long-term, I would imagine. probably some kind of name resolution module.
-- that would be more feasilbe if we get rid of the 'WiredIn' type like the note
-- on that type says.
resolveWiredIn :: Text -> Maybe WiredIn
resolveWiredIn t = case M.lookup t wiredInNameMap of
  Just nv -> Just nv
  Nothing -> Nothing

wiredInNameMap :: Map Text WiredIn
wiredInNameMap = M.fromList $ go <$> [minBound .. maxBound]
  where
    go :: WiredIn -> (Text, WiredIn)
    go w = (T.pack $ List.drop 2 $ show w, w)
