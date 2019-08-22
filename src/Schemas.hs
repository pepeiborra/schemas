{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
module Schemas
  (
  -- * Schemas
    Constructor(..)
  , Field(..)
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
  , empty
  , enum
  , record
  , RecordSchema
  , RecordField(..)
  , optional
  , required
  , union
  , union'
  , Alt(..)
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
  )where

import           Control.Lens         hiding (Empty, enum)
import           Control.Monad
import           Data.Aeson           (Value)
import qualified Data.Aeson           as A
import           Data.Aeson.Lens
import           Data.Barbie
import           Data.Biapplicative
import           Data.Either
import           Data.Generics.Labels ()
import qualified Data.HashMap.Strict  as Map
import           Data.List            (find)
import           Data.List.NonEmpty   (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty   as NE
import           Data.Maybe
import           Data.Scientific
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Tuple
import           Data.Vector          (Vector)
import qualified Data.Vector          as V
import           GHC.Exts             (fromList)
import           GHC.Generics         (Generic)
import           Prelude hiding (lookup)

-- Schemas
-- --------------------------------------------------------------------------------

data Field f = Field
  { name     :: f Text
  , schema   :: f Schema
  , isRequired :: f Bool
  }
  deriving (Generic)
  deriving anyclass (FunctorB, ProductB, TraversableB)

deriving instance Eq (Field Identity)
deriving instance Show (Field Identity)

instance HasSchema (Field Identity) where
  schema = record $ Field (required "name") (required "schema") (required "required")

data Constructor f = Constructor
  { name   :: f Text
  , schema :: f (Maybe Schema)
  }
  deriving (Generic)
  deriving anyclass (FunctorB, ProductB, TraversableB)

deriving instance Eq (Constructor Identity)
deriving instance Show (Constructor Identity)

instance HasSchema (Constructor Identity) where
  schema = record $ Constructor (required "name") (optional "schema")

data Schema
  = Empty
  | Bool
  | Number
  | String
  | Array Schema
  | Enum   (NonEmpty Text)
  | Record (NonEmpty (Field Identity))
  | Union  (NonEmpty (Constructor Identity))
  deriving (Eq, Generic, Show)

-- Typed schemas
-- --------------------------------------------------------------------------------

-- | TypedSchema is designed to be used with higher-kinded types, Barbie style
--   Its main addition over 'Schema' is converting from a JSON 'Value'
data TypedSchemaFlex from a where
  TBool :: (Bool -> a) -> (from -> Bool) -> TypedSchemaFlex from a
  TNumber :: (Scientific -> a) -> (from -> Scientific) -> TypedSchemaFlex from a
  TString :: (Text -> a) ->  (from -> Text) -> TypedSchemaFlex from a
  TEnum   :: (NonEmpty (Text, a)) -> (from -> Text) -> TypedSchemaFlex from a
  TArray :: TypedSchema b -> (Vector b -> a) -> (from -> Vector b) -> TypedSchemaFlex from a
  PureSchema :: a -> TypedSchemaFlex from a
  RecordSchema :: (ProductB f, TraversableB f) =>
                  RecordSchema f -> (f Identity -> a) -> (from -> f Identity) -> TypedSchemaFlex from a
  UnionSchema :: (NonEmpty (Text, TypedSchemaFlex from a)) -> (from -> Text) -> TypedSchemaFlex from a

type TypedSchema a = TypedSchemaFlex a a
type RecordSchema f = f RecordField

class HasSchema a where
  schema :: TypedSchema a

instance HasSchema () where
  schema = empty

instance HasSchema Bool where
  schema = TBool id id

instance HasSchema Double where
  schema = TNumber toRealFloat fromFloatDigits

instance HasSchema Scientific where
  schema = TNumber id id

instance HasSchema Int where
  schema = TNumber floor fromIntegral

instance {-# OVERLAPPING #-} HasSchema String where
  schema = TString T.unpack T.pack

instance HasSchema Text where
  schema = TString id id

instance {-# OVERLAPPABLE #-} HasSchema a => HasSchema [a] where
  schema = TArray schema V.toList V.fromList

instance HasSchema a => HasSchema (Vector a) where
  schema = TArray schema id id

instance  HasSchema a => HasSchema (NonEmpty a) where
  schema = TArray schema (NE.fromList . V.toList) (V.fromList . NE.toList)

instance HasSchema Schema where
  schema = union'
    [ alt "Bool" #_Bool
    , alt "Number" #_Number
    , alt "String" #_String
    , alt "Array" #_Array
    , alt "Enum" #_Enum
    , alt "Record" #_Record
    , alt "Union" #_Union
    , alt "Empty" #_Empty
    ]

enum :: Eq a => (a -> Text) -> (NonEmpty a) -> TypedSchema a
enum showF opts = TEnum alts (fromMaybe (error "invalid alt") . flip lookup altMap)
 where
  altMap = fmap swap $ alts --TODO fast lookup
  alts   = opts <&> \x -> (showF x, x)

empty :: TypedSchema ()
empty = PureSchema ()


instance Functor (TypedSchemaFlex from) where
  fmap = rmap

instance Profunctor TypedSchemaFlex where
  dimap _ f (PureSchema a               ) = PureSchema (f a)
  dimap g f (TBool   tof fromf          ) = TBool (f . tof) (fromf . g)
  dimap g f (TNumber tof fromf          ) = TNumber (f . tof) (fromf . g)
  dimap g f (TString tof fromf          ) = TString (f . tof) (fromf . g)
  dimap g f (TEnum        opts     fromf) = TEnum (second f <$> opts) (fromf . g)
  dimap g f (TArray       sc   tof fromf) = TArray sc (f . tof) (fromf . g)
  dimap g f (RecordSchema sc tof fromf  ) = RecordSchema sc (f . tof) (fromf . g)
  dimap g f (UnionSchema tags       getTag) = UnionSchema (second (dimap g f) <$> tags) (getTag . g)

-- --------------------------------------------------------------------------------
-- Typed Records

-- | Annotates a typed schema with a field name

record :: (ProductB f, TraversableB f) => RecordSchema f -> TypedSchema (f Identity)
record sc = RecordSchema sc id id

data RecordField a where
  Required :: Text -> TypedSchema a -> RecordField a
  Optional  :: Text -> TypedSchema a -> RecordField (Maybe a)

required :: HasSchema a => Text -> RecordField a
required n = Required n schema

optional :: HasSchema a => Text -> RecordField (Maybe a)
optional n = Optional n schema

-- --------------------------------------------------------------------------------
-- Typed Unions

union :: (NonEmpty (Text, TypedSchema a, a -> Bool)) -> TypedSchema a
union args = UnionSchema constructors fromF
 where
  constructors = args <&> \(c, sc, _) -> (c, sc)
  fromF x = maybe (error $ "invalid constructor") (view _1)
    $ find (\(_, _, p) -> p x) args

data Alt from where
  Alt :: Text -> Prism' from b -> TypedSchema b -> Alt from

alt :: HasSchema a => Text -> Prism' from a -> Alt from
alt n p = Alt n p schema

union' :: (NonEmpty (Alt from)) -> TypedSchema from
union' args = union $ args <&> \(Alt c p sc) ->
    withPrism p $ \t f ->
      (c, dimap (either (error "impossible") id . f) t sc, isRight . f)

-- --------------------------------------------------------------------------------
-- Finite schemas

-- | Ensure that a 'Schema' is finite by enforcing a max depth.
--   The result is guaranteed to be a subtype of the input.
finite :: Int -> Schema -> Schema
finite = go
 where
  go :: Int -> Schema -> Schema
  go 0 (Record _) = Empty
  go 0 (Array _) = Empty
  go 0 (Union _) = Empty
  go d (Record opts) =
    Record $ fmap (\(Field n sc opt) -> Field n (go (d - 1) <$> sc) opt) opts
  go d (Union opts) = Union (over (traverse . #schema . mapped . mapped) (go (d-1)) opts)
  go d (Array sc  ) = Array (go (d-1) sc)
  go _ other        = other

-- | Ensure that a 'Value' is finite by enforcing a max depth in a schema preserving way
finiteValue :: Int -> Schema -> Value -> Value
finiteValue d sc
  | Just cast <- finite d sc `isSubtypeOf` sc = cast
  | otherwise = error "bug in isSubtypeOf"

-- --------------------------------------------------------------------------------
-- Schema extraction from a TypedSchema

-- | Extract an untyped schema that can be serialized
extractSchema :: TypedSchema a -> Schema
extractSchema TBool{} = Bool
extractSchema PureSchema{} = Empty
extractSchema TNumber{} = Number
extractSchema TString{} = String
extractSchema (TEnum opts  _) = Enum (fst <$> opts)
extractSchema (TArray sc _ _)  = Array (extractSchema sc)
extractSchema (RecordSchema rs _ _) = maybe Empty Record $ nonEmpty $ bfoldMap ((:[]) . f) rs
  where
    f (Required n sc) = Field (pure n) (pure $ extractSchema sc) (pure True)
    f (Optional n sc) = Field (pure n) (pure $ extractSchema sc) (pure False)
extractSchema (UnionSchema scs _getTag) = Union $ scs <&> \(n, sc) ->
  Constructor
    (pure n)
    (pure $ case extractSchema sc of
      Empty -> Nothing
      x     -> Just x
    )

theSchema :: forall a . HasSchema a => Schema
theSchema = extractSchema (schema @a)

-- ---------------------------------------------------------------------------------------
-- Encoding

-- | Given a value and its typed schema, produce a JSON record using the 'RecordField's
encodeWith :: TypedSchema a -> a -> Value
encodeWith (TBool _ fromf) b = A.Bool (fromf b)
encodeWith (TNumber _ fromf) b = A.Number (fromf b)
encodeWith (TString _ fromf) b = A.String (fromf b)
encodeWith (TEnum _ fromf) b = A.String (fromf b)
encodeWith (PureSchema _) _  = A.object []
encodeWith (TArray sc _ fromf) b = A.Array (encodeWith sc <$> fromf b)
encodeWith (RecordSchema rs _ fromf) b =
  A.Object
    $ fromList
    $ bfoldMap (maybe [] (: []) . getConst)
    $ bzipWith f rs (fromf b)
 where
  f :: RecordField a -> Identity a -> Const (Maybe (Text, Value)) a
  f (Required n sc) x  = Const $ Just (n, encodeWith sc $ runIdentity x)
  f (Optional  n sc) x = Const $ (n, ) . encodeWith sc <$> runIdentity x
encodeWith (UnionSchema opts fromF) x =
  case lookup tag opts of
    Nothing -> error $ "Unknown tag: " <> show tag
    Just sc -> A.object [ tag A..= encodeWith sc x ]
  where tag = fromF x

-- | encode using the default schema
encode :: HasSchema a => a -> Value
encode = encodeWith schema

-- | Encode a value into a finite representation by enforcing a max depth
finiteEncode :: forall a. HasSchema a => Int -> a -> Value
finiteEncode d = finiteValue d (theSchema @a) . encode

-- --------------------------------------------------------------------------
-- Decoding

data DecodeError
  = InvalidRecordField { name :: Text, context :: [Text]}
  | MissingRecordField { name :: Text, context :: [Text]}
  | InvalidEnumValue { given :: Text, options :: NonEmpty Text, context :: [Text]}
  | InvalidConstructor { name :: Text, context :: [Text]}
  | InvalidUnionType { contents :: Value, context :: [Text]}
  | SchemaMismatch {context :: [Text]}
  deriving (Eq, Show)

-- | Given a JSON 'Value' and a typed schema, extract a Haskell value
decodeWith :: TypedSchema a -> Value -> Either DecodeError a
decodeWith = go []
  where
    go :: [Text] -> TypedSchema a -> Value -> Either DecodeError a
    go _tx (TBool tof _) (A.Bool x) = pure $ tof x
    go _tx (TNumber tof _) (A.Number x) = pure $ tof x
    go _tx (TString tof _) (A.String x) = pure $ tof x
    go ctx (TEnum opts _) (A.String x) = maybe (Left $ InvalidEnumValue x (fst <$> opts) ctx) pure $ lookup x opts
    go ctx (TArray sc tof _) (A.Array x) = tof <$> traverse (go ("[]" : ctx) sc) x
    go _tx (PureSchema a) _ = pure a
    go ctx (RecordSchema rsc tof _) (A.Object fields) = tof <$> btraverse f rsc
      where
        f :: RecordField a -> Either DecodeError (Identity a)
        f (Required n sc) = case Map.lookup n fields of
          Just v  -> pure <$> go (n : ctx) sc v
          Nothing -> Left $ MissingRecordField n ctx
        f (Optional n sc) = case Map.lookup n fields of
          Just v  -> pure . pure <$> go (n : ctx) sc v
          Nothing -> pure $ pure Nothing
    go ctx (UnionSchema opts _) it@(A.Object x) = case Map.toList x of
      [(n, v)] -> case lookup n opts of
        Just sc -> go (n : ctx) sc v
        Nothing -> Left $ InvalidConstructor n ctx
      _ -> Left $ InvalidUnionType it ctx
    go ctx _ _ = Left $ SchemaMismatch ctx

decode :: HasSchema a => Value -> Either DecodeError a
decode = decodeWith schema

-- ------------------------------------------------------------------------------------------------------
-- Subtype relation

-- | @sub isSubtypeOf sup@ returns a witness that @sub@ is a subtype of @sup@, i.e. a cast function @sub -> sup@
--
-- > Array Bool `isSubtypeOf` Bool
--   Just <function>
-- > Record [("a", Bool)] `isSubtypeOf` Record [("a", Number)]
--   Nothing
isSubtypeOf :: Schema -> Schema -> Maybe (Value -> Value)
isSubtypeOf sub sup = go sup sub
 where
  go (Array _)  Empty = pure $ const (A.Array [])
  go (Union _)  Empty = pure $ const (A.Object $ fromList [])
  go (Record _) Empty = pure $ const (A.Object $ fromList [])
  go (Array a) (Array b) = do
    f <- go a b
    pure $ over (_Array . traverse) f
  go a (Array b) | a == b = Just (A.Array . fromList . (:[]))
  go (Enum opts) (Enum opts') | all (`elem` opts') opts = Just id
  go (Union opts) (Union opts') = do
    ff <- forM opts $ \(Constructor (Identity n) (Identity sc)) -> do
      Constructor _ (Identity sc') <- find ((== Identity n) . view #name) opts'
      case (sc,sc') of
        (Nothing, Nothing) -> return id
        (Just sc, Just sc') -> do
          f <- go sc sc'
          return $ over (_Object . ix n) f
        _ -> Nothing
    return (foldr (.) id ff)
  go (Record opts) (Record opts') = do
    forM_ opts $ \(Field n _ (Identity req)) ->
      guard $ not req || isJust (find ((== n) . view #name) opts')
    ff <- forM opts' $ \(Field (Identity n') (Identity sc') (Identity req')) -> do
      case find ((== Identity n') . view #name) opts of
        Nothing -> do
          Just $ over (_Object) (Map.delete n')
        Just (Field _ (Identity sc) (Identity req)) -> do
          guard (not req || req')
          f <- go sc sc'
          Just $ over (_Object . ix n') f
    return (foldr (.) id ff)
  go a b | a == b = pure id
  go a b          = a `seq` b `seq` Nothing

-- | Returns 'Nothing' if 'sub' is not a subtype of 'sup'
coerce :: forall sub sup . (HasSchema sub, HasSchema sup) => Value -> Maybe Value
coerce = case isSubtypeOf (theSchema @sub) (theSchema @sup) of
  Just cast -> Just . cast
  Nothing -> const Nothing

-- ----------------------------------------------
-- Utils

-- | Generalized lookup for Foldables
lookup :: (Eq a, Foldable f) => a -> f (a,b) -> Maybe b
lookup a = fmap snd . find ((== a) . fst)

-- The Schema schema is recursive and cannot be serialized unless we use finiteEncode
-- >>> import qualified Data.ByteString.Lazy.Char8 as B
-- >>> B.putStrLn $ A.encode $ finiteEncode 10 (theSchema @Schema)
-- {"Union":[{"name":"Bool"},{"name":"Number"},{"name":"String"},{"schema":{"Union":[{"name":"Bool"},{"name":"Number"},{"name":"String"},{"schema":{"Union":[{"name":"Bool"},{"name":"Number"},{"name":"String"},{"schema":{"Union":[]},"name":"Array"},{"schema":{"Array":{}},"name":"Enum"},{"schema":{"Array":{}},"name":"Record"},{"schema":{"Array":{}},"name":"Union"},{"name":"Empty"}]},"name":"Array"},{"schema":{"Array":{"String":{}}},"name":"Enum"},{"schema":{"Array":{"Record":[{"required":true,"schema":{},"name":"name"},{"required":true,"schema":{},"name":"schema"},{"required":true,"schema":{},"name":"required"}]}},"name":"Record"},{"schema":{"Array":{"Record":[{"required":true,"schema":{},"name":"name"},{"required":false,"schema":{},"name":"schema"}]}},"name":"Union"},{"name":"Empty"}]},"name":"Array"},{"schema":{"Array":{"String":{}}},"name":"Enum"},{"schema":{"Array":{"Record":[{"required":true,"schema":{"String":{}},"name":"name"},{"required":true,"schema":{"Union":[{"name":"Bool"},{"name":"Number"},{"name":"String"},{"schema":{},"name":"Array"},{"schema":{},"name":"Enum"},{"schema":{},"name":"Record"},{"schema":{},"name":"Union"},{"name":"Empty"}]},"name":"schema"},{"required":true,"schema":{"Bool":{}},"name":"required"}]}},"name":"Record"},{"schema":{"Array":{"Record":[{"required":true,"schema":{"String":{}},"name":"name"},{"required":false,"schema":{"Union":[{"name":"Bool"},{"name":"Number"},{"name":"String"},{"schema":{},"name":"Array"},{"schema":{},"name":"Enum"},{"schema":{},"name":"Record"},{"schema":{},"name":"Union"},{"name":"Empty"}]},"name":"schema"}]}},"name":"Union"},{"name":"Empty"}]}

-- Deserializing a value V of a recursive schema S is not supported,
-- because S is not a subtype of the truncated schema finite(S)

-- >>> isJust $ finite 10 (theSchema @Schema) `isSubtypeOf` theSchema @Schema
-- True
-- >>> isJust $ theSchema @Schema `isSubtypeOf` finite 10 (theSchema @Schema)
-- False

