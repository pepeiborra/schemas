{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Schemas.Class where

import           Control.Lens         hiding (_Empty, Empty, enum, (<.>))
import           Data.Aeson           (Value)
import           Data.Biapplicative
import           Data.Hashable
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.HashSet         (HashSet)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Maybe
import           Data.Scientific
import           Data.Text            (Text, pack, unpack)
import           Data.Vector          (Vector)
import           Numeric.Natural
import           Schemas.Attempt
import           Schemas.Internal
import           Schemas.Untyped

-- HasSchema class and instances
-- -----------------------------------------------------------------------------------

class HasSchema a where
  schema :: TypedSchema a

instance HasSchema () where
  schema = TPure ()

instance HasSchema Bool where
  schema = viaJSON "Boolean"

instance HasSchema Double where
  schema = viaJSON "Number"

instance HasSchema Scientific where
  schema = viaJSON "Number"

instance HasSchema Int where
  schema = viaJSON "Integer"

instance HasSchema Integer where
  schema = viaJSON "Integer"

instance HasSchema Natural where
  schema = viaJSON "Integer"

instance {-# OVERLAPPING #-} HasSchema String where
  schema = string

instance HasSchema Text where
  schema = viaJSON "String"

instance {-# OVERLAPPABLE #-} HasSchema a => HasSchema [a] where
  schema = list schema

instance (HasSchema a) => HasSchema (Vector a) where
  schema = vector schema

instance (Eq a, Hashable a, HasSchema a) => HasSchema (HashSet a) where
  schema = list schema

instance  (HasSchema a) => HasSchema (NonEmpty a) where
  schema = list schema

instance HasSchema a => HasSchema (Identity a) where
  schema = dimap runIdentity Identity schema

deriving instance HasSchema SchemaName

instance HasSchema Schema where
  schema = named "Schema" $ union
    [ ("StringMap", alt $ prism' StringMap (\case StringMap x -> Just x ; _ -> Nothing))
    , ("Array", alt     $ prism' Array (\case Array x -> Just x ; _ -> Nothing))
    , ("Enum", alt      $ prism' Enum (\case Enum x -> Just x ; _ -> Nothing))
    , ("Record", alt    $ prism' Record (\case Record x -> Just x ; _ -> Nothing))
    , ("Prim", alt      $ prism' Prim (\case Prim x -> Just x ; _ -> Nothing))
    , ("Union",altWith unionSchema _Union)
    , ("OneOf", alt     $ prism' OneOf (\case OneOf x -> Just x ; _ -> Nothing))
    , ("Named", altWith namedSchema $ prism' (uncurry Named) (\case Named s sc -> Just (s,sc) ; _ -> Nothing))
    , ("Empty", alt     $ prism' (const Empty) (\case Empty -> Just () ; _ -> Nothing))
    ]
    where
      namedSchema = record $ (,) <$> field "name" fst <*> field "schema" snd
      unionSchema = list (record $ (,) <$> field "constructor" fst <*> field "schema" snd)

instance HasSchema Field where
  schema = record $
        Field <$> field "schema" fieldSchema
              <*> fmap (fromMaybe True) (optField "isRequired" (\x -> if isRequired x then Nothing else Just False))


instance HasSchema Value where
  schema = viaJSON "JSON"

instance (HasSchema a, HasSchema b) => HasSchema (a,b) where
  schema = record $ (,) <$> field "$1" fst <*> field "$2" snd

instance (HasSchema a, HasSchema b, HasSchema c) => HasSchema (a,b,c) where
  schema = record $ (,,) <$> field "$1" (view _1) <*> field "$2" (view _2) <*> field "$3" (view _3)

instance (HasSchema a, HasSchema b, HasSchema c, HasSchema d) => HasSchema (a,b,c,d) where
  schema =
    record
      $   (,,,)
      <$> field "$1" (view _1)
      <*> field "$2" (view _2)
      <*> field "$3" (view _3)
      <*> field "$4" (view _4)

instance (HasSchema a, HasSchema b, HasSchema c, HasSchema d, HasSchema e) => HasSchema (a,b,c,d,e) where
  schema =
    record
      $   (,,,,)
      <$> field "$1" (view _1)
      <*> field "$2" (view _2)
      <*> field "$3" (view _3)
      <*> field "$4" (view _4)
      <*> field "$5" (view _5)

instance (HasSchema a, HasSchema b) => HasSchema (Either a b) where
  schema = union [("Left", alt _Left), ("Right", alt _Right)
                 ,("left", alt _Left), ("right", alt _Right)]

instance (Eq key, Hashable key, HasSchema a, Key key) => HasSchema (HashMap key a) where
  schema = dimap toKeyed fromKeyed $ stringMap schema
    where
      fromKeyed :: HashMap Text a -> HashMap key a
      fromKeyed = Map.fromList . map (first fromKey) . Map.toList
      toKeyed :: HashMap key a -> HashMap Text a
      toKeyed = Map.fromList . map (first toKey) . Map.toList

class Key a where
  fromKey :: Text -> a
  toKey :: a -> Text

instance Key Text where
  fromKey = id
  toKey = id

instance Key String where
  fromKey = unpack
  toKey   = pack

-- HasSchema aware combinators
-- -----------------------------------------------------------------------------------
-- | Extract the default 'Schema' for a type
theSchema :: forall a . HasSchema a => Schema
theSchema = case extractSchema (schema @a) of x :| _ -> x

validatorsFor :: forall a . HasSchema a => Validators
validatorsFor = extractValidators (schema @a)

-- | encode using the default schema
encode :: HasSchema a => (a -> Value)
encode = encodeWith schema

-- | Attempt to encode to the target schema using the default schema.
--   First encodes using the default schema, then computes a coercion
--   applying 'isSubtypeOf', and then applies the coercion to the encoded data.
encodeTo :: HasSchema a => Schema -> Attempt E (a -> Value)
encodeTo = encodeToWith schema

-- | Decode using the default schema.
decode :: HasSchema a => Value -> Result a
decode = decodeWith schema

-- | Apply `isSubtypeOf` to construct a coercion from the source schema to the default schema,
--   apply the coercion to the data, and attempt to decode using the default schema.
decodeFrom :: HasSchema a => Schema -> Result (Value -> Result a)
decodeFrom = decodeFromWith schema

-- | Coerce from 'sub' to 'sup'Returns 'Nothing' if 'sub' is not a subtype of 'sup'
coerce :: forall sub sup . (HasSchema sub, HasSchema sup) => Value -> Maybe Value
coerce = case isSubtypeOf (validatorsFor @sub) (theSchema @sub) (theSchema @sup) of
  Right cast -> Just . cast
  _          -> const Nothing

-- | @field name get@ introduces a field with the default schema for the type
field :: HasSchema a => Text -> (from -> a) -> RecordFields from a
field = fieldWith schema

-- | @optField name get@ introduces an optional field with the default schema for the type
optField :: forall a from. HasSchema a => Text -> (from -> Maybe a) -> RecordFields from (Maybe a)
optField n get = optFieldWith (lmap get $ liftJust (schema @a)) n

-- | @optFieldEither name get@ introduces an optional field with the default schema for the type
optFieldEither
    :: forall a from e
     . HasSchema a
    => Text
    -> (from -> Either e a)
    -> e
    -> RecordFields from (Either e a)
optFieldEither n f e = optFieldEitherWith (lmap f (liftRight schema)) n e

-- | @alt name prism@ introduces a discriminated union alternative with the default schema
alt :: HasSchema a => Prism' from a -> UnionAlt from
alt = altWith schema
