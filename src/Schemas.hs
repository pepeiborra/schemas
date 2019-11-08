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
  , emptySchema
  , pureSchema
  , enum
  , readShow
  , list
  , string
  , vector
  , Key(..)
  , stringMap
  , viaJSON
  , viaIso
  , named
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
  -- *** Discriminated Unions
  , union
  , alt
  , altWith
  , UnionAlt
  -- *** Undiscriminated unions
  , Typed.oneOf
  , eitherSchema
  , liftPrism
  -- ** Encoding
  , encode
  , decode
  , encodeTo
  , decodeFrom
  , encodeWith
  , decodeWith
  , encodeToWith
  , decodeFromWith
  , DecodeError

  -- ** Results
  , Result
  , runResult
  , runDelay
  , IterAlt
  , retractAlt
  , Attempt
  , runAttempt

  -- * Untyped schemas
  , Schema(.., Unit, Union)
  , Field(..)
  , _Unit
  , _Union
  -- ** Extraction
  , extractSchema
  , theSchema
  -- ** Functions
  , Mismatch(..)
  , Trace
  , isSubtypeOf
  , coerce
  , validate
  , validatorsFor
  -- * Reexports
  , Profunctor(..)
  )
  where

import Data.Profunctor
import Schemas.Class
import Schemas.Internal as Typed
import Schemas.Untyped
import Schemas.Attempt
