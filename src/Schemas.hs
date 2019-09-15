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
  , readShow
  , list
  , stringMap
  , viaJSON
  , viaIso
  , Key(..)
  -- *** Applicative record definition
  , record
  , Alt
  , RecordField
  , RecordFields
  , field
  , fieldWith
  , fieldWith'
  , optField
  , optFieldWith
  , optFieldEither
  , optFieldEitherWith
  , optFieldGeneral
  , fieldName
  , extractFields
  , liftMaybe
  , liftEither
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
