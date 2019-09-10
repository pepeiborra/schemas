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
  , encodeWith
  , decodeWith
  , DecodeError(..)
  -- * working with recursive schemas
  , finiteValue
  , finiteEncode
  )
  where

import Schemas.Internal
