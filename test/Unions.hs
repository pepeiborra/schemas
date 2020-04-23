{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists  #-}
module Unions where

import Data.Generics.Labels ()
import Schemas
import GHC.Generics (Generic)

data Some a = Some a | None
  deriving (Eq, Generic, Show)

schemaSomeNone, schemaNoneSome :: HasSchema a => TypedSchema (Some a)
schemaSomeNone = oneOf [alt #_Some, alt #_None]
schemaNoneSome = oneOf [alt #_None, alt #_Some]


data Three a b = One a | Two b | Three
  deriving (Eq, Generic, Show)

schemaThree, schemaThree' :: TypedSchema a -> TypedSchema b -> TypedSchema (Three a b)
schemaThree  a b = oneOf [altWith a #_One, altWith b #_Two, alt #_Three]
schemaThree' a b = oneOf [alt #_Three, altWith a #_One, altWith b #_Two]
