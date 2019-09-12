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
  , list
  , stringMap
  , Key(..)
  -- *** Applicative record definition
  , record
  , Alt
  , RecordField
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
  )
  where

import Control.Alternative.Free
import Schemas.Internal
