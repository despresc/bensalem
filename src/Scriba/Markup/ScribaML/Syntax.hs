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

{- A brief description of scriba syntax as it relates to parsing, to be
   incorporated into documentation somewhere, later.

- groups are plain braced arguments {<markup>*}

- elements start with one of the tag tokens

- a backslash element tag can be followed by an optional attribute map and a run
  of optional groups as arguments, each separated by white space. if neither of
  these categories are present, then the tag must be followed by a right brace,
  right bracket, white space, or the start of another element. it may also be
  followed by a comma when in attribute markup.

- the &-type element starts a new layout context

- the indentation of a new layout context is equal to the indentation of the
  next line, if it is indented by more than current context

- the first arguments to a layout element are given by the initial run of groups
  in its layout context. the content of the last argument is any subsequent
  markup.

- a layout context ends at a deindent (a line indented by less than the current
  layout context) or at a right brace or right bracket. the layout context does
  not include trailing blank lines.

- braced groups and attribute maps cannot contain deindents - they must be ended
  by their matching right tokens

- the element starting forms (taking into account the special layout rules for
  the layout elements):
  - \<tag><attrs>?<arg>*
  - &<tag><attrs>?<arg>*<layout content>
  - <# run><tag><attrs>?<arg>*<level content>

- attribute maps look like [<kvPairs>], where kvPairs is a white-space-separated
  list of key-value pairs. there may be special forms for, e.g., boolean
  attributes in future.

- a key-value pair looks like <key> '=' <value>, where <key> is one or more
  alphanumeric strings separated by periods and <value> is:

  - a sub-map [<kvPairs>]

  - a braced group {node}

I had the idea of possibly incorporating anonymous elements, notably an
anonymous layout element, (looking like \. and &. for inline and layout) as
another way to specify element arguments and the values of key-value pairs.
these haven't been added yet, if they ever will be.
-}

-- | A single node in scriba syntax
data Node
  = PlainText !Text
  | LineSpace !Int
  | LineEnd
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
