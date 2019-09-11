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
  , enum
  , list
  , stringMap
  , Key(..)
  -- *** Applicative record definition
  , record
  , RecordField
  , field
  , fieldWith
  , optField
  , optFieldWith
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
