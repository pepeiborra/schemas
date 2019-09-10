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
  , tempty
  , enum
  , stringMap
  , Key(..)
  -- *** Applicative record definition
  , record
  , field
  , optField
  , fieldName
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

import Schemas.Internal
