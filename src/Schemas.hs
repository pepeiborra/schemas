{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}
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

import           Control.Alternative.Free
import           Control.Applicative      (Alternative (..))
import           Control.Lens             hiding (Empty, enum)
import           Control.Monad
import           Data.Aeson               (Value)
import qualified Data.Aeson               as A
import           Data.Aeson.Lens
import           Data.Biapplicative
import           Data.Either
import           Data.Functor.Compose
import           Data.Generics.Labels     ()
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as Map
import           Data.List                (find)
import           Data.List.NonEmpty       (NonEmpty (..))
import qualified Data.List.NonEmpty       as NE
import           Data.Maybe
import           Data.Scientific
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Tuple
import           Data.Vector              (Vector)
import qualified Data.Vector              as V
import           GHC.Exts                 (fromList)
import           GHC.Generics             (Generic)
import           Numeric.Natural
import           Prelude                  hiding (lookup)

-- Schemas
-- --------------------------------------------------------------------------------

data Schema
  = Empty
  | Bool
  | Number
  | String
  | Array Schema
  | StringMap Schema
  | Enum   (NonEmpty Text)
  | Record (HashMap Text Field)
  | Union  (HashMap Text Schema)
  | Or Schema Schema
  deriving (Eq, Generic, Show)

instance Monoid Schema where mempty = Empty
instance Semigroup Schema where
  Empty <> x = x
  x <> Empty = x
  a <> b = Or a b

data Field = Field
  { fieldSchema :: Schema
  , isRequired  :: Maybe Bool -- ^ defaults to True
  }
  deriving (Eq, Show)

isRequiredField :: Field -> Bool
isRequiredField Field{isRequired = Just x} = x
isRequiredField _                          = True

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
  TMap   :: TypedSchema b -> (HashMap Text b -> a) -> (from -> HashMap Text b) -> TypedSchemaFlex from a
  TOr    :: TypedSchemaFlex from a -> TypedSchemaFlex from a -> TypedSchemaFlex from a
  TEmpty :: a -> TypedSchemaFlex from a
  RecApSchema :: Alt (RecordFieldF from') a' -> (a' -> a) -> (from -> from') -> TypedSchemaFlex from a
  UnionSchema :: (NonEmpty (Text, TypedSchemaFlex from a)) -> (from -> Text) -> TypedSchemaFlex from a

enum :: Eq a => (a -> Text) -> (NonEmpty a) -> TypedSchema a
enum showF opts = TEnum alts (fromMaybe (error "invalid alt") . flip lookup altMap)
 where
  altMap = fmap swap $ alts --TODO fast lookup
  alts   = opts <&> \x -> (showF x, x)

tempty :: TypedSchema ()
tempty = TEmpty ()

instance Functor (TypedSchemaFlex from) where
  fmap = rmap

instance Profunctor TypedSchemaFlex where
  dimap _ f (TEmpty a             ) = TEmpty (f a)
  dimap g f (TOr a b) = TOr (dimap g f a) (dimap g f b)
  dimap g f (TBool   tof  fromf       ) = TBool (f . tof) (fromf . g)
  dimap g f (TNumber tof  fromf       ) = TNumber (f . tof) (fromf . g)
  dimap g f (TString tof  fromf       ) = TString (f . tof) (fromf . g)
  dimap g f (TEnum   opts fromf       ) = TEnum (second f <$> opts) (fromf . g)
  dimap g f (TArray       sc tof fromf) = TArray sc (f . tof) (fromf . g)
  dimap g f (TMap         sc tof fromf) = TMap sc (f . tof) (fromf . g)
  dimap g f (RecApSchema  sc tof fromf) = RecApSchema sc (f . tof) (fromf . g)
  dimap g f (UnionSchema tags getTag) =
    UnionSchema (second (dimap g f) <$> tags) (getTag . g)

instance Monoid a => Monoid (TypedSchemaFlex f a) where
  mempty = TEmpty mempty

instance Semigroup a => Semigroup (TypedSchemaFlex f a) where
  TEmpty a <> TEmpty b = TEmpty (a <> b)
  TEmpty{} <> x = x
  x <> TEmpty{} = x
  a <> b = TOr a b

type TypedSchema a = TypedSchemaFlex a a

-- --------------------------------------------------------------------------------
-- Applicative records

data RecordFieldF from a where
  RequiredAp :: Text -> (from -> a) -> TypedSchema a -> RecordFieldF from a
  OptionalAp :: Text -> (from -> Maybe a) -> TypedSchema a -> RecordFieldF from (Maybe a)

fieldName :: RecordFieldF from a -> Text
fieldName (RequiredAp x _ _) = x
fieldName (OptionalAp x _ _) = x

-- | Define a record schema using applicative syntax
record :: Alt (RecordFieldF a) a -> TypedSchema a
record sc = RecApSchema sc id id

field :: HasSchema a => Text -> (from -> a) -> Alt (RecordFieldF from) a
field n get = liftAlt (RequiredAp n get schema)

optField :: HasSchema a => Text -> (from -> Maybe a) -> Alt (RecordFieldF from) (Maybe a)
optField n get = liftAlt (OptionalAp n get schema)

-- --------------------------------------------------------------------------------
-- Typed Unions

union :: (NonEmpty (Text, TypedSchema a, a -> Bool)) -> TypedSchema a
union args = UnionSchema constructors fromF
 where
  constructors = args <&> \(c, sc, _) -> (c, sc)
  fromF x = maybe (error $ "invalid constructor") (view _1)
    $ find (\(_, _, p) -> p x) args

data UnionTag from where
  UnionTag :: Text -> Prism' from b -> TypedSchema b -> UnionTag from

alt :: HasSchema a => Text -> Prism' from a -> UnionTag from
alt n p = UnionTag n p schema

union' :: (NonEmpty (UnionTag from)) -> TypedSchema from
union' args = union $ args <&> \(UnionTag c p sc) ->
    withPrism p $ \t f ->
      (c, dimap (either (error "impossible") id . f) t sc, isRight . f)

-- HasSchema class and instances
-- -----------------------------------------------------------------------------------

class HasSchema a where
  schema :: TypedSchema a

instance HasSchema () where
  schema = tempty

instance HasSchema Bool where
  schema = TBool id id

instance HasSchema Double where
  schema = TNumber toRealFloat fromFloatDigits

instance HasSchema Scientific where
  schema = TNumber id id

instance HasSchema Int where
  schema = TNumber floor fromIntegral

instance HasSchema Natural where
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

instance HasSchema Field where
  schema = record $ Field <$> field "schema" fieldSchema <*> optField "field'" isRequired

instance HasSchema a => HasSchema (Identity a) where
  schema = dimap runIdentity Identity schema

instance HasSchema Schema where
  schema = union'
    [ alt "Bool" #_Bool
    , alt "Number" #_Number
    , alt "String" #_String
    , alt "StringMap" #_StringMap
    , alt "Array" #_Array
    , alt "Enum" #_Enum
    , alt "Record" #_Record
    , alt "Union" #_Union
    , alt "Empty" #_Empty
    , alt "Or" #_Or
    ]

instance (HasSchema a, HasSchema b) => HasSchema (a,b) where
  schema = record $ (,) <$> field "fst" fst <*> field "snd" snd

instance HasSchema a => HasSchema (HashMap Text a) where
  schema = TMap schema id id

-- --------------------------------------------------------------------------------
-- Finite schemas

-- | Ensure that a 'Schema' is finite by enforcing a max depth.
--   The result is guaranteed to be a supertype of the input.
finite :: Natural -> Schema -> Schema
finite = go
 where
  go :: Natural -> Schema -> Schema
  go 0 _ = Empty
  go d (Record    opts) = Record $ fromList $ mapMaybe
    (\(fieldname, Field sc isOptional) -> case go (max 0 (pred d)) sc of
      Empty -> Nothing
      sc'   -> Just (fieldname, Field sc' isOptional)
    )
    (Map.toList opts)
  go d (Union     opts) = Union (fmap (go (max 0 (pred d))) opts)
  go d (Array     sc  ) = Array (go (max 0 (pred d)) sc)
  go d (StringMap sc  ) = StringMap (go (max 0 (pred d)) sc)
  go d (Or a b        ) = Or (finite (d - 1) a) (finite (d - 1) b)
  go _ other            = other

-- | Ensure that a 'Value' is finite by enforcing a max depth in a schema preserving way
finiteValue :: Natural -> Schema -> Value -> Value
finiteValue d sc
  | Just cast <- sc `isSubtypeOf` finite d sc = cast
  | otherwise = error "bug in isSubtypeOf"

-- --------------------------------------------------------------------------------
-- Schema extraction from a TypedSchema

-- | Extract an untyped schema that can be serialized
extractSchema :: TypedSchemaFlex from a -> Schema
extractSchema TBool{}          = Bool
extractSchema (TOr a b) = Or (extractSchema a) (extractSchema b)
extractSchema TEmpty{}     = Empty
extractSchema TNumber{}        = Number
extractSchema TString{}        = String
extractSchema (TEnum opts  _)  = Enum (fst <$> opts)
extractSchema (TArray sc _ _)  = Array $ extractSchema sc
extractSchema (TMap sc _ _)    = StringMap $ extractSchema sc
extractSchema (RecApSchema rs _ _) = foldMap (Record . fromList) $ runAlt_ ((:[]) . (:[]) . extractField) rs
  where
    extractField :: RecordFieldF from a -> (Text, Field)
    extractField (RequiredAp n _ sc) = (n,) . (`Field` Nothing) $ extractSchema sc
    extractField (OptionalAp n _ sc) = (n,) . (`Field` Just False) $ extractSchema sc
extractSchema (UnionSchema scs _getTag) =
  Union . Map.fromList . NE.toList $ fmap (\(n, sc) -> (n, extractSchema sc)) scs

theSchema :: forall a . HasSchema a => Schema
theSchema = extractSchema (schema @a)

-- ---------------------------------------------------------------------------------------
-- Encoding

-- | Given a value and its typed schema, produce a JSON record using the 'RecordField's
encodeWith :: TypedSchemaFlex from a -> from -> Value
encodeWith (TBool _ fromf) b = A.Bool (fromf b)
encodeWith (TOr a b) x = encodeAlternatives [encodeWith a x, encodeWith b x]
encodeWith (TNumber _ fromf) b = A.Number (fromf b)
encodeWith (TString _ fromf) b = A.String (fromf b)
encodeWith (TEnum _ fromf) b = A.String (fromf b)
encodeWith (TEmpty _) _  = A.object []
encodeWith (TArray sc _ fromf) b = A.Array (encodeWith sc <$> fromf b)
encodeWith (TMap   sc _ fromf) b = A.Object(encodeWith sc <$> fromf b)
encodeWith (RecApSchema rec _ fromf) x = encodeAlternatives $ fmap (A.Object . fromList) fields
  where
    fields = runAlt_ (maybe [[]] ((:[]) . (:[])) . extractFieldAp (fromf x)) rec

    extractFieldAp b (RequiredAp n get sc) = Just (n, encodeWith sc  $  get b)
    extractFieldAp b (OptionalAp n get sc) = (n,) . encodeWith sc <$> get b

encodeWith (UnionSchema [(_,sc)] _) x = encodeWith sc x
encodeWith (UnionSchema opts fromF) x =
  case lookup tag opts of
    Nothing           -> error $ "Unknown tag: " <> show tag
    Just TEmpty{} -> A.String tag
    Just sc           -> A.object [ tag A..= encodeWith sc x ]
  where tag = fromF x

encodeAlternatives :: [Value] -> Value
encodeAlternatives [] = error "empty"
encodeAlternatives [x] = x
encodeAlternatives (x:xx) = A.object ["L" A..= x, "R" A..= encodeAlternatives xx]

-- | encode using the default schema
encode :: HasSchema a => a -> Value
encode = encodeWith schema

-- | Encode a value into a finite representation by enforcing a max depth
finiteEncode :: forall a. HasSchema a => Natural -> a -> Value
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
  | InvalidAlt {context :: [Text], path :: Path}
  deriving (Eq, Show)

-- | Given a JSON 'Value' and a typed schema, extract a Haskell value
decodeWith :: TypedSchema a -> Value -> Either DecodeError a
decodeWith = go []
 where
  go :: [Text] -> TypedSchema a -> Value -> Either DecodeError a
  go _tx (TBool   tof _) (A.Bool   x) = pure $ tof x
  go _tx (TNumber tof _) (A.Number x) = pure $ tof x
  go _tx (TString tof _) (A.String x) = pure $ tof x
  go ctx (TEnum opts _) (A.String x) =
    maybe (Left $ InvalidEnumValue x (fst <$> opts) ctx) pure $ lookup x opts
  go ctx (TArray sc tof _) (A.Array x) =
    tof <$> traverse (go ("[]" : ctx) sc) x
  go ctx (TMap sc tof _) (A.Object x) = tof <$> traverse (go ("[]" : ctx) sc) x
  go _tx (TEmpty a) _ = pure a
  go ctx (RecApSchema rec tof _) o@A.Object{}
    | (A.Object fields, encodedPath) <- decodeAlternatives o = tof <$> fromMaybe
      (Left $ InvalidAlt ctx encodedPath)
      (selectPath encodedPath (getCompose $ runAlt (Compose . (: []) . f fields) rec))
   where
    f :: A.Object -> RecordFieldF from a -> Either DecodeError a
    f fields (RequiredAp n _ sc) = doRequiredField ctx n sc fields
    f fields (OptionalAp n _ sc) = doOptionalField ctx n sc fields
  go _tx (UnionSchema opts _) (A.String n)
    | Just (TEmpty a) <- lookup n opts = pure a
  go ctx (UnionSchema opts _) it@(A.Object x) = case Map.toList x of
    [(n, v)] -> case lookup n opts of
      Just sc -> go (n : ctx) sc v
      Nothing -> Left $ InvalidConstructor n ctx
    _ -> Left $ InvalidUnionType it ctx
  go ctx (TOr a b) (A.Object x) = do
    let l = Map.lookup "L" x <&> go ("L":ctx) a
    let r = Map.lookup "R" x <&> go ("R":ctx) b
    fromMaybe (Left $ SchemaMismatch ctx) $ l <|> r
  go ctx _ _ = Left $ SchemaMismatch ctx

  doRequiredField
    :: [Text]
    -> Text
    -> TypedSchema b
    -> HashMap Text Value
    -> Either DecodeError b
  doRequiredField ctx n sc fields = case Map.lookup n fields of
    Just v  -> go (n : ctx) sc v
    Nothing -> case sc of
      TArray _ tof' _ -> pure $ tof' []
      _               -> Left $ MissingRecordField n ctx

  doOptionalField
    :: [Text]
    -> Text
    -> TypedSchema b
    -> HashMap Text Value
    -> Either DecodeError (Maybe b)
  doOptionalField ctx n sc fields = case Map.lookup n fields of
    Just v  -> pure <$> go (n : ctx) sc v
    Nothing -> pure Nothing

decode :: HasSchema a => Value -> Either DecodeError a
decode = decodeWith schema

type Path = [Bool]

decodeAlternatives :: Value -> (Value, Path)
decodeAlternatives (A.Object x)
  | Just v <- Map.lookup "L" x = (v, [True])
  | Just v <- Map.lookup "R" x = (False :) <$> decodeAlternatives v
  | otherwise                  = (A.Object x, [])
decodeAlternatives x = (x,[])

selectPath :: Path -> [a] -> Maybe a
selectPath (True : _) (x : _)  = Just x
selectPath (False:rest) (_:xx) = selectPath rest xx
selectPath [] [x]              = Just x
selectPath _ _                 = Nothing

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
  nil = A.Object $ fromList []
  go Empty         _         = pure $ const nil
  go (Array     _) Empty     = pure $ const (A.Array [])
  go (Union     _) Empty     = pure $ const nil
  go (Record    _) Empty     = pure $ const nil
  go (StringMap _) Empty     = pure $ const nil
  go Or{}          Empty     = pure $ const nil
  go (Array a)     (Array b) = do
    f <- go a b
    pure $ over (_Array . traverse) f
  go (StringMap a) (StringMap b) = do
    f <- go a b
    pure $ over (_Object . traverse) f
  go a (Array b) | a == b = Just (A.Array . fromList . (: []))
  go (Enum opts) (Enum opts') | all (`elem` opts') opts = Just id
  go (Union opts) (Union opts') = do
    ff <- forM (Map.toList opts) $ \(n, sc) -> do
      sc' <- Map.lookup n opts'
      f   <- go sc sc'
      return $ over (_Object . ix n) f
    return (foldr (.) id ff)
  go (Record opts) (Record opts') = do
    forM_ (Map.toList opts) $ \(n, f@(Field _ _)) ->
      guard $ not (isRequiredField f) || Map.member n opts'
    ff <- forM (Map.toList opts') $ \(n', f'@(Field sc' _)) -> do
      case Map.lookup n' opts of
        Nothing -> do
          Just $ over (_Object) (Map.delete n')
        Just f@(Field sc _) -> do
          guard (not (isRequiredField f) || isRequiredField f')
          witness <- go sc sc'
          Just $ over (_Object . ix n') witness
    return (foldr (.) id ff)
  -- go (Or a b) c = over (_Object . ix "L") <$> go a c
  --             <|> over (_Object . ix "R") <$> go b c
  go a (Or b c) =
    (go a b <&> \f -> fromMaybe (error "bad input") . preview (_Object . ix "L" . to f)) <|>
    (go a c <&> \f -> fromMaybe (error "bad input") . preview (_Object . ix "R" . to f))
  go (Or a b) c = (go a c <&> ((A.object . (:[]) . ("L" A..=)) .))
              <|> (go b c <&> ((A.object . (:[]) . ("R" A..=)) .))
  go a b | a == b = pure id
  go _ _          = Nothing

-- | Returns 'Nothing' if 'sub' is not a subtype of 'sup'
coerce :: forall sub sup . (HasSchema sub, HasSchema sup) => Value -> Maybe Value
coerce = case isSubtypeOf (theSchema @sub) (theSchema @sup) of
  Just cast -> Just . cast
  Nothing   -> const Nothing

-- ----------------------------------------------
-- Utils

-- | Generalized lookup for Foldables
lookup :: (Eq a, Foldable f) => a -> f (a,b) -> Maybe b
lookup a = fmap snd . find ((== a) . fst)

runAlt_ :: (Alternative g, Monoid m) => (forall a. f a -> g m) -> Alt f b -> g m
runAlt_ f = fmap getConst . getCompose . runAlt (Compose . fmap Const . f)

-- >>> data Person = Person {married :: Bool, age :: Int}
-- >>> runAlt_  (\x -> [[fieldName x]]) (Person <$> (field "married" married <|> field "foo" married) <*> (field "age" age <|> pure 0))
-- [["married","age"],["married"],["foo","age"],["foo"]]

-- ----------------------------------------------
-- Examples

-- The Schema schema is recursive and cannot be serialized unless we use finiteEncode
-- >>> import Text.Pretty.Simple
-- >>> pPrintNoColor $ finite 2 (theSchema @Schema)
-- Union
--     ( fromList
--         [
--             ( "String"
--             , Empty
--             )
--         ,
--             ( "Empty"
--             , Empty
--             )
--         ,
--             ( "Union"
--             , StringMap Empty
--             )
--         ,
--             ( "StringMap"
--             , Union
--                 ( fromList
--                     [
--                         ( "String"
--                         , Empty
--                         )
--                     ,
--                         ( "Empty"
--                         , Empty
--                         )
--                     ,
--                         ( "Union"
--                         , Empty
--                         )
--                     ,
--                         ( "StringMap"
--                         , Empty
--                         )
--                     ,
--                         ( "Array"
--                         , Empty
--                         )
--                     ,
--                         ( "Record"
--                         , Empty
--                         )
--                     ,
--                         ( "Enum"
--                         , Empty
--                         )
--                     ,
--                         ( "Number"
--                         , Empty
--                         )
--                     ,
--                         ( "Bool"
--                         , Empty
--                         )
--                     ]
--                 )
--             )
--         ,
--             ( "Array"
--             , Union
--                 ( fromList
--                     [
--                         ( "String"
--                         , Empty
--                         )
--                     ,
--                         ( "Empty"
--                         , Empty
--                         )
--                     ,
--                         ( "Union"
--                         , Empty
--                         )
--                     ,
--                         ( "StringMap"
--                         , Empty
--                         )
--                     ,
--                         ( "Array"
--                         , Empty
--                         )
--                     ,
--                         ( "Record"
--                         , Empty
--                         )
--                     ,
--                         ( "Enum"
--                         , Empty
--                         )
--                     ,
--                         ( "Number"
--                         , Empty
--                         )
--                     ,
--                         ( "Bool"
--                         , Empty
--                         )
--                     ]
--                 )
--             )
--         ,
--             ( "Record"
--             , StringMap Empty
--             )
--         ,
--             ( "Enum"
--             , Array String
--             )
--         ,
--             ( "Number"
--             , Empty
--             )
--         ,
--             ( "Bool"
--             , Empty
--             )
--         ]
--     )

-- >>> import Data.Aeson.Encode.Pretty
-- >>> import qualified Data.ByteString.Lazy.Char8 as B
-- >>> B.putStrLn $ encodePretty $ finiteEncode 4 (theSchema @Schema)
-- {
--     "Union": {
--         "String": "Empty",
--         "Empty": "Empty",
--         "Union": {
--             "StringMap": {
--                 "Union": {}
--             }
--         },
--         "StringMap": {
--             "Union": {
--                 "String": {},
--                 "Empty": {},
--                 "Union": {},
--                 "StringMap": {},
--                 "Array": {},
--                 "Record": {},
--                 "Enum": {},
--                 "Number": {},
--                 "Bool": {}
--             }
--         },
--         "Array": {
--             "Union": {
--                 "String": {},
--                 "Empty": {},
--                 "Union": {},
--                 "StringMap": {},
--                 "Array": {},
--                 "Record": {},
--                 "Enum": {},
--                 "Number": {},
--                 "Bool": {}
--             }
--         },
--         "Record": {
--             "StringMap": {
--                 "Record": {}
--             }
--         },
--         "Enum": {
--             "Array": "String"
--         },
--         "Number": "Empty",
--         "Bool": "Empty"
--     }
-- }

-- Deserializing a value V of a recursive schema S is not supported,
-- because S is not a subtype of the truncated schema finite(S)

-- >>> isJust $ finite 10 (theSchema @Schema) `isSubtypeOf` theSchema @Schema
-- True
-- >>> isJust $ theSchema @Schema `isSubtypeOf` finite 10 (theSchema @Schema)
-- False

