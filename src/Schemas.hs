{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
module Schemas
  (
  -- * Schemas
    TypedSchema
  , TypedSchemaFlex
  -- * Construction
  , empty
  , bool
  , double
  , int
  , number
  , string
  , text
  , list
  , array
  , enum
  , record
  , RecordSchema
  , RecordField(..)
  , union
  , union'
  , Alt(..)
  -- * Functions
  , finite
  , theSchema
  , pack
  , unpack
  , UnpackError(..)
  , isSubtype
  )where

import           Control.Lens          hiding (enum)
import           Control.Monad
import           Data.Aeson            (FromJSON, ToJSON, Value)
import qualified Data.Aeson            as A
import           Data.Aeson.Lens
import           Data.Barbie
import           Data.Biapplicative
import           Data.Either
import qualified Data.HashMap.Strict   as Map
import           Data.List             (find)
import           Data.Maybe
import           Data.Scientific
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Tuple
import           Data.Vector           (Vector)
import qualified Data.Vector           as V
import           GHC.Exts
import           GHC.Generics

data ValueType
  = Bool
  | Number
  | String
  | Array ValueType
  | Enum   [Text]
  | Record [(Text, ValueType, Bool)]   -- ^ name, type, required
  | Union  [(Text, ValueType)]
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

-- | TypedSchema is designed to be used with higher-kinded types, Barbie style
--   Its main addition over 'ValueType' is converting from a JSON 'Value'
data TypedSchemaFlex from a where
  TBool :: (Bool -> a) -> (from -> Bool) -> TypedSchemaFlex from a
  TNumber :: (Scientific -> a) -> (from -> Scientific) -> TypedSchemaFlex from a
  TString :: (Text -> a) ->  (from -> Text) -> TypedSchemaFlex from a
  TEnum   :: [(Text, a)] -> (from -> Text) -> TypedSchemaFlex from a
  TArray :: TypedSchema b -> (Vector b -> a) -> (from -> Vector b) -> TypedSchemaFlex from a
  PureSchema :: a -> TypedSchemaFlex from a
  RecordSchema :: (ProductB f, TraversableB f) =>
                  RecordSchema f -> (f Identity -> a) -> (from -> f Identity) -> TypedSchemaFlex from a
  UnionSchema :: [(Text, TypedSchemaFlex from a)] -> (from -> Text) -> TypedSchemaFlex from a

type TypedSchema a = TypedSchemaFlex a a
type RecordSchema f = f RecordField

bool :: TypedSchema Bool
bool = TBool id id

double :: TypedSchema Double
double = TNumber toRealFloat fromFloatDigits

number :: TypedSchema Scientific
number = TNumber id id

int :: TypedSchema Int
int = TNumber floor fromIntegral

string :: TypedSchema String
string = TString T.unpack T.pack

text :: TypedSchema Text
text = TString id id

list :: TypedSchema a -> TypedSchema [a]
list sc = TArray sc V.toList V.fromList

array :: TypedSchema a -> TypedSchema (Vector a)
array sc = TArray sc id id

record :: (ProductB f, TraversableB f) => RecordSchema f -> TypedSchema (f Identity)
record sc = RecordSchema sc id id

enum :: Eq a => (a -> Text) -> [a] -> TypedSchema a
enum showF opts = TEnum alts (fromMaybe (error "invalid alt") . flip lookup altMap)
 where
  altMap = map swap $ alts --TODO fast lookup
  alts   = [ (showF x, x) | x <- opts ]

empty :: TypedSchema ()
empty = PureSchema ()

union :: [(Text, TypedSchema a, a -> Bool)] -> TypedSchema a
union args = UnionSchema constructors fromF
 where
  constructors = [ (c, sc) | (c, sc, _) <- args ]
  fromF x = fromMaybe (error $ "invalid constructor")
    $ listToMaybe [ c | (c, _, p) <- args, p x ]

data Alt from where
  Alt :: Text -> Prism' from b -> TypedSchema b -> Alt from

union' :: [Alt from] -> TypedSchema from
union' args = union
  [ withPrism p $ \t f ->
      (c, dimap (either (error "impossible") id . f) t sc, isRight . f)
  | Alt c p sc <- args]

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

-- | Annotates a typed schema with a field name
data RecordField a where
  Required :: Text -> TypedSchema a -> RecordField a
  Optional  :: Text -> TypedSchema a -> RecordField (Maybe a)

-- | Ensures a 'ValueType' is finite by enforcing a max depth
finite :: Int -> ValueType -> ValueType
finite = go
 where
  go :: Int -> ValueType -> ValueType
  go 0 (Record _) = Record []
  go d (Record opts) =
    Record $ map (\(n, sc, opt) -> (n, go (d - 1) sc, opt)) opts
  go d (Union opts) = Union (second (go d) <$> opts)
  go d (Array sc  ) = Array (go d sc)
  go _ other        = other

-- | Folds over a record of 'RecordField' things to reconstruct an untyped schema
theSchema :: TypedSchemaFlex from a -> ValueType
theSchema TBool{} = Bool
theSchema PureSchema{} = Record []
theSchema TNumber{} = Number
theSchema TString{} = String
theSchema (TEnum opts  _) = Enum (fst <$> opts)
theSchema (TArray sc _ _)  = Array (theSchema sc)
theSchema (RecordSchema rs _ _) = Record $ bfoldMap ((:[]) . f) rs
  where
    f (Required n sc)  = (n, theSchema sc, True)
    f (Optional  n sc) = (n, theSchema sc, False)
theSchema (UnionSchema scs _getTag) = Union $ second theSchema <$> scs

-- | Given a value and its typed schema, produce a JSON record using the 'RecordField's
pack :: TypedSchemaFlex from a -> from -> Value
pack (TBool _ fromf) b = A.Bool (fromf b)
pack (TNumber _ fromf) b = A.Number (fromf b)
pack (TString _ fromf) b = A.String (fromf b)
pack (TEnum _ fromf) b = A.String (fromf b)
pack (PureSchema _) _  = A.object []
pack (TArray sc _ fromf) b = A.Array (pack sc <$> fromf b)
pack (RecordSchema rs _ fromf) b =
  A.Object
    $ fromList
    $ bfoldMap (maybe [] (: []) . getConst)
    $ bzipWith f rs (fromf b)
 where
  f :: RecordField a -> Identity a -> Const (Maybe (Text, Value)) a
  f (Required n sc) x  = Const $ Just (n, pack sc $ runIdentity x)
  f (Optional  n sc) x = Const $ (n, ) . pack sc <$> runIdentity x
pack (UnionSchema opts fromF) x =
  case lookup tag opts of
    Nothing -> error $ "Unknown tag: " <> show tag
    Just sc -> A.object [ tag A..= pack sc x ]
  where tag = fromF x

data UnpackError
  = InvalidRecordField { name :: Text, context :: [Text]}
  | MissingRecordField { name :: Text, context :: [Text]}
  | InvalidEnumValue { given :: Text, options, context :: [Text]}
  | InvalidConstructor { name :: Text, context :: [Text]}
  | InvalidUnionType { contents :: Value, context :: [Text]}
  | SchemaMismatch {context :: [Text]}
  deriving (Eq, Show)

-- | Given a JSON 'Value' and a typed schema, extract a Haskell value
unpack :: TypedSchema a -> Value -> Either UnpackError a
unpack = go []
  where
    go :: [Text] -> TypedSchema a -> Value -> Either UnpackError a
    go _tx (TBool tof _) (A.Bool x) = pure $ tof x
    go _tx (TNumber tof _) (A.Number x) = pure $ tof x
    go _tx (TString tof _) (A.String x) = pure $ tof x
    go ctx (TEnum opts _) (A.String x) = maybe (Left $ InvalidEnumValue x (fst <$> opts) ctx) pure $ lookup x opts
    go ctx (TArray sc tof _) (A.Array x) = tof <$> traverse (go ("[]" : ctx) sc) x
    go _tx (PureSchema a) _ = pure a
    go ctx (RecordSchema rsc tof _) (A.Object fields) = tof <$> btraverse f rsc
      where
        f :: RecordField a -> Either UnpackError (Identity a)
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

-- | `isSubtype sub sup` returns a witness that sub is a subtype of sup, i.e. a cast function
isSubtype :: ValueType -> ValueType -> Maybe (Value -> Value)
isSubtype sub sup = go sup sub
 where
  go (Array a) (Array b) = do
    f <- isSubtype a b
    pure $ over (_Array . traverse) f
  go (Union opts) (Union opts') = do
    ff <- forM opts $ \(n,sc) -> do
      sc' <- lookup n opts'
      f <- isSubtype sc sc'
      return $ over (_Object . ix n) f
    return (foldr (.) id ff)
  go (Record opts) (Record opts') = do
    ff <- forM opts $ \(n, sc, req) -> do
      case find (\(n', _, _) -> n == n') opts' of
        Nothing -> Just $ over (_Object) (Map.delete n)
        Just (_, sc', req') -> do
          guard (req || not req')
          f <- isSubtype sc sc'
          Just $ over (_Object . ix n) f
    return (foldr (.) id ff)
  go a b | a == b = pure id
  go _ _          = Nothing
