module Schemas
 (
  -- * Schemas
    Field(..)
  , Schema(..)
  -- ** functions for working with schemas
  , isSubtypeOf
  , coerce
  , finite
  -- * Typed schemas
  , TypedSchema
  , TypedSchemaFlex
  , HasSchema(..)
  , theSchema
  , extractSchema
  -- ** Construction
  , prim
  , enum
  , readShow
  , list
  , stringMap
  , Key(..)
  -- *** Applicative record definition
  , record
  , Alt
  , RecordField
  , RecordFields
  , field
  , fieldWith
  , optField
  , optFieldWith
  , fieldName
  , extractFields
  -- *** Unions
  , union
  , union'
  , UnionTag(..)
  , alt
  -- * Encoding
  , encode
  , decode
  , encodeTo
  , decodeFrom
  , encodeWith
  , decodeWith
  , encodeToWith
  , decodeFromWith
  , DecodeError(..)
  -- * working with recursive schemas
  , finiteValue
  , finiteEncode
  -- * Reexports
  , Profunctor(..)
  )
  where

import Control.Alternative.Free
import Data.Profunctor
import Schemas.Internal
