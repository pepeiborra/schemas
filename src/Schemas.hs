{-# LANGUAGE PatternSynonyms #-}
module Schemas
 (
  -- * Schemas
    Field(..)
  , Schema(.., Empty, Union)
  , _Empty
  , _Union
  -- ** functions for working with schemas
  , Mismatch(..)
  , Trace
  , isSubtypeOf
  , versions
  , coerce
  , finite
  , validate
  , validatorsFor
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
  , vector
  , Key(..)
  , stringMap
  , viaJSON
  , viaIso
  , oneOf
  -- *** Applicative record definition
  , record
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
  , fieldNameL
  , overFieldNames
  , extractFields
  , liftJust
  , liftRight
  , liftPrism
  -- *** Unions
  , union
  , union'
  , UnionTag(..)
  , alt
  , altWith
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

import Data.Profunctor
import Schemas.Class
import Schemas.Internal
import Schemas.Untyped
