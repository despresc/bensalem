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

import Data.Map.Strict (Map)
import Data.Text (Text)

-- | A single node in scriba syntax
data Node
  = -- | text not containing syntactically-significant characters
    PlainText !Text
  | -- | a run of single space characters
    LineSpace !Int
  | -- | a line ending
    LineEnd
  | -- | an element with attributes and arguments
    Element AttrMap [[Node]]
  deriving (Eq, Ord, Show)

data AttrVal
  = AttrValMarkup [Node]
  | AttrValMap AttrMap
  deriving (Eq, Ord, Show)

newtype AttrMap = AttrMap
  { unAttrMap :: Map Text AttrVal
  }
  deriving (Eq, Ord, Show)
