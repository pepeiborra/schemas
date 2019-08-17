{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE GADTs              #-}
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
  , theSchema
  , pack
  , unpack
  , isSubtype
  )where

import           Control.Prism
import           Data.Aeson            (FromJSON, ToJSON, Value)
import qualified Data.Aeson            as A
import           Data.Barbie
import           Data.Biapplicative
import           Data.Either
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Profunctor
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
  | Record [(Text, ValueType, Bool)]
  | Union  [(Text, ValueType)]
  deriving (Generic, Show, ToJSON, FromJSON)

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

-- list :: TypedSchemaFlex from a -> TypedSchemaFlex from [a]
list :: TypedSchemaFlex b b -> TypedSchemaFlex [b] [b]
list sc = TArray sc V.toList V.fromList

-- list :: TypedSchemaFlex from a -> TypedSchemaFlex from [a]
array :: TypedSchemaFlex b b -> TypedSchemaFlex (Vector b) (Vector b)
array sc = TArray sc id id

record :: (ProductB f, TraversableB f) => RecordSchema f -> TypedSchema (f Identity)
record sc = RecordSchema sc id id

enum :: Eq a => (a -> Text) -> [a] -> TypedSchema a
enum showF opts = TEnum alts (fromMaybe (error "invalid alt") . flip lookup altMap)
 where
  altMap = map swap $ alts --TODO fast lookup
  alts   = [ (showF x, x) | x <- opts ]

empty :: TypedSchemaFlex () ()
empty = PureSchema ()

union :: [(Text, TypedSchema a, a -> Bool)] -> TypedSchema a
union args = UnionSchema constructors fromF
 where
  constructors = [ (c, sc) | (c, sc, _) <- args ]
  fromF x = fromMaybe (error $ "invalid constructor")
    $ listToMaybe [ c | (c, _, p) <- args, p x ]

data Alt from where
  Alt :: Text -> Prism' from b -> TypedSchemaFlex b b -> Alt from

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
  Mandatory :: Text -> TypedSchemaFlex a a -> RecordField a
  Optional  :: Text -> TypedSchemaFlex a a -> RecordField (Maybe a)

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
    f (Mandatory n sc) = (n, theSchema sc, True)
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
  f (Mandatory n sc) x = Const $ Just (n, pack sc $ runIdentity x)
  f (Optional  n sc) x = Const $ (n, ) . pack sc <$> runIdentity x
pack (UnionSchema opts fromF) x =
  case lookup tag opts of
    Nothing -> error $ "Unknown tag: " <> show tag
    Just sc  -> pack sc x
  where tag = fromF x

-- | Given a JSON 'Value' and a typed schema, verify whether it "fits" and if so extract a Haskell value
unpack :: Value -> TypedSchemaFlex from a -> Maybe a
unpack = undefined

-- | One cannot write a function that will recover a typed schema from an untyped one
promote :: ValueType -> Maybe (TypedSchemaFlex from a)
promote = const Nothing

-- | Returns a witness of subtyping, i.e. a cast function for JSON values
isSubtype :: ValueType -> ValueType -> Maybe (Value -> Value)
isSubtype = undefined
