-- |
-- Description : Scriba document syntax
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- Types respresenting the surface syntax of a scriba document. Note that these
-- types do not represent source documents exactly; they discard information
-- like the precise indentation contexts of layout elements or the exact levels
-- of level elements.
module Scriba.Markup.ScribaML.Syntax where

import Data.Text (Text)
import Data.Map.Strict (Map)

-- | A single node in scriba syntax
data Node
  = PlainText !Text
  | LineSpace !Int
  | LineEnd
  | Element
  deriving (Eq, Ord, Show)
