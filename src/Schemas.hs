{-# LANGUAGE PatternSynonyms #-}

-- | As a simple example of a schema, let's consider a simple record type:
--
-- @
--  import Schemas
--  import Schemas.SOP
--
--  data Person = Person
--    { name      :: String
--    , age       :: Int
--    , addresses :: [String]
--    }
--
--  personSchema :: TypedSchema Person
--  personSchema = record $ Person
--    \<$\> fieldWith string "name" name
--    \<*\> fieldWith int    "age"  age
--    \<*\> fieldWith (list string) "addresses" addresses
-- @
--
-- Or, by relying on the @HasSchema@ type class:
--
-- @
--  personSchema :: TypedSchema Person
--  personSchema = record $ Person
--    \<$\> field "name" name
--    \<*\> field "age"  age
--    \<*\> field "addresses" addresses
-- @
--
-- Or, if the type is SOP generic:
--
-- @
-- personSchema = gSchema defOptions
-- @

module Schemas
 (
  -- * Typed schemas
    TypedSchemaFlex
  , TypedSchema
  , HasSchema(..)
  -- ** Construction
  , enum
  , readShow
  , list
  , string
  , vector
  , Key(..)
  , stringMap
  , viaJSON
  , viaIso
  -- *** Applicative record definition
  , record
  , RecordFields
  , RecordField
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
  -- *** Partial schemas
  , liftJust
  , liftRight
  , liftPrism
  -- *** Unions
  , union
  , union'
  , alt
  , altWith
  , UnionTag
  , oneOf
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

  -- * Untyped schemas
  , Schema(.., Empty, Union)
  ,  Field(..)
  , _Empty
  , _Union
  -- ** Extraction
  , extractSchema
  , theSchema
  -- ** Functions
  , Mismatch(..)
  , Trace
  , isSubtypeOf
  , versions
  , coerce
  , finite
  , validate
  , validatorsFor
  -- * Reexports
  , Profunctor(..)
  )
  where

import Data.Profunctor
import Schemas.Class
import Schemas.Internal
import Schemas.Untyped

