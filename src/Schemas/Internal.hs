{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -Wno-name-shadowing    #-}
module Schemas.Internal where

import           Control.Alternative.Free
import           Control.Applicative        (Alternative (..))
import           Control.Exception
import           Control.Lens               hiding (Empty, enum, (<.>))
import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.Aeson                 (Value)
import qualified Data.Aeson                 as A
import           Data.Aeson.Lens
import           Data.Biapplicative
import           Data.Either
import           Data.Foldable              (asum)
import           Data.Functor.Compose
import           Data.Generics.Labels       ()
import           Data.Hashable
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.HashSet               (HashSet)
import           Data.List                  (find)
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NE
import           Data.Maybe
import           Data.Scientific
import           Data.Text                  (Text, pack, unpack)
import           Data.Tuple
import           Data.Typeable              (Typeable)
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           GHC.Exts                   (IsList (..))
import           GHC.Generics               (Generic)
import           Numeric.Natural
import           Prelude                    hiding (lookup)
import           Text.Read


-- Schemas
-- --------------------------------------------------------------------------------

data Schema
  = Empty
  | Array Schema
  | StringMap Schema
  | Enum   (NonEmpty Text)
  | Record (HashMap Text Field)
  | AllOf (NonEmpty Schema)  -- ^ encoding and decoding work for all alternatives
  | OneOf (NonEmpty Schema)  -- ^ Decoding works for all alternatives, encoding only for one
  | Prim
  deriving (Eq, Generic, Show)

instance Monoid Schema where mempty = Empty
instance Semigroup Schema where
  Empty <> x    = x
  x <> Empty    = x
  AllOf aa <> b = AllOf (aa <> [b])
  b <> AllOf aa = AllOf ([b] <> aa)
  a <> b        = AllOf [a,b]

data Field = Field
  { fieldSchema :: Schema
  , isRequired  :: Bool -- ^ defaults to True
  }
  deriving (Generic, Eq, Show)

-- Is there more than one choice here ? Maybe this should be configuration
emptyValue :: Value
emptyValue = A.object []

pattern Union :: NonEmpty (Text, Schema) -> Schema
pattern Union alts <- (preview _Union -> Just alts) where
  Union alts = review _Union alts

_Union :: Prism' Schema (NonEmpty (Text, Schema))
_Union = prism' build match
  where
    build = OneOf . fmap (\(n,sc) -> Record [(n, Field sc True)])

    match (OneOf scc) = traverse viewAlt scc
    match _           = Nothing

    viewAlt :: Schema -> Maybe (Text, Schema)
    viewAlt (Record [(n,Field sc True)]) = Just (n, sc)
    viewAlt _                            = Nothing

-- Typed schemas
-- --------------------------------------------------------------------------------

-- | @TypedSchemaFlex enc dec@ is a schema for encoding to @enc@ and decoding to @dec@.
--   Usually we want @enc@ and @dec@ to be the same type but this flexibility comes in handy
--   when composing typed schemas.
data TypedSchemaFlex from a where
  TEnum   :: (NonEmpty (Text, a)) -> (from -> Text) -> TypedSchemaFlex from a
  TArray :: TypedSchema b -> (Vector b -> a) -> (from -> Vector b) -> TypedSchemaFlex from a
  TMap   :: TypedSchema b -> (HashMap Text b -> a) -> (from -> HashMap Text b) -> TypedSchemaFlex from a
  -- used to support renamings and other structural changes
  TAllOf :: NonEmpty (TypedSchemaFlex from a) -> TypedSchemaFlex from a
  -- used to support unions, etc
  TOneOf :: NonEmpty (TypedSchemaFlex from a) -> TypedSchemaFlex from a
  TEmpty :: a -> TypedSchemaFlex from a
  TPrim  :: (Value -> A.Result a) -> (from -> Value) -> TypedSchemaFlex from a
  -- TTry is used to implement 'optField' on top of 'optFieldWith'
  -- it could be exposed to provide some form of error handling, but currently is not
  TTry   :: TypedSchemaFlex a b -> (a' -> Maybe a) -> TypedSchemaFlex a' b
  RecordSchema :: RecordFields from a -> TypedSchemaFlex from a

enum :: Eq a => (a -> Text) -> (NonEmpty a) -> TypedSchema a
enum showF opts = TEnum alts (fromMaybe (error "invalid alt") . flip lookup altMap)
 where
  altMap = fmap swap $ alts --TODO fast lookup
  alts   = opts <&> \x -> (showF x, x)

stringMap :: TypedSchema a -> TypedSchema (HashMap Text a)
stringMap sc = TMap sc id id

list :: IsList l => TypedSchema (Item l) -> TypedSchema l
list schema = TArray schema (fromList . V.toList) (V.fromList . toList)

vector :: TypedSchema a -> TypedSchema (Vector a)
vector sc = TArray sc id id

viaJSON :: (A.FromJSON a, A.ToJSON a) => TypedSchema a
viaJSON = TPrim A.fromJSON A.toJSON

viaIso :: Iso' a b -> TypedSchema a -> TypedSchema b
viaIso iso sc = withIso iso $ \from to -> dimap to from sc

readShow :: (Read a, Show a) => TypedSchema a
readShow = dimap show read schema

instance Functor (TypedSchemaFlex from) where
  fmap = rmap

instance Profunctor TypedSchemaFlex where
    dimap _ f (TEmpty a                 ) = TEmpty (f a)
    dimap g f (TTry        sc        try) = TTry (rmap f sc) (try . g)
    dimap g f (TAllOf      scc          ) = TAllOf (dimap g f <$> scc)
    dimap g f (TOneOf      scc          ) = TOneOf (dimap g f <$> scc)
    dimap g f (TEnum     opts      fromf) = TEnum (second f <$> opts) (fromf . g)
    dimap g f (TArray      sc  tof fromf) = TArray sc (f . tof) (fromf . g)
    dimap g f (TMap        sc  tof fromf) = TMap sc (f . tof) (fromf . g)
    dimap g f (TPrim           tof fromf) = TPrim (fmap f . tof) (fromf . g)
    dimap g f (RecordSchema sc) = RecordSchema (dimap g f sc)

instance Monoid a => Monoid (TypedSchemaFlex f a) where
  mempty = TEmpty mempty

instance Semigroup a => Semigroup (TypedSchemaFlex f a) where
  TEmpty a <> TEmpty b = TEmpty (a <> b)
  TEmpty{} <> x = x
  x <> TEmpty{} = x
  TAllOf aa <> b = TAllOf (aa <> [b])
  a <> TAllOf bb = TAllOf ([a] <> bb)
  a <> b = TAllOf [a,b]

type TypedSchema a = TypedSchemaFlex a a

-- --------------------------------------------------------------------------------
-- Applicative records

data RecordField from a where
  RequiredAp :: { fieldName :: Text
                , fieldTypedSchema :: TypedSchemaFlex from a
                } -> RecordField from a
  OptionalAp :: { fieldName :: Text
                , fieldTypedSchema :: TypedSchemaFlex from a
                , fieldDefValue :: a
                } -> RecordField from a

fieldNameL :: Lens' (RecordField from a) Text
fieldNameL f (RequiredAp n sc) = (`RequiredAp` sc) <$> f n
fieldNameL f OptionalAp{..} = (\fieldName -> OptionalAp{..}) <$> f fieldName

instance Profunctor RecordField where
  dimap f g (RequiredAp name sc)     = RequiredAp name (dimap f g sc)
  dimap f g (OptionalAp name sc def) = OptionalAp name (dimap f g sc) (g def)

newtype RecordFields from a = RecordFields {getRecordFields :: Alt (RecordField from) a}
  deriving newtype (Alternative, Applicative, Functor, Monoid, Semigroup)

instance Profunctor RecordFields where
  dimap f g = RecordFields . hoistAlt (lmap f) . fmap g . getRecordFields

overFieldNames :: (Text -> Text) -> RecordFields from a -> RecordFields from a
overFieldNames f = RecordFields . hoistAlt ((over fieldNameL f)) . getRecordFields

-- | Define a record schema using applicative syntax
record :: RecordFields from a -> TypedSchemaFlex from a
record = RecordSchema

field :: HasSchema a => Text -> (from -> a) -> RecordFields from a
field = fieldWith schema

fieldWith :: TypedSchema a -> Text -> (from -> a) -> RecordFields from a
fieldWith schema n get = fieldWith' (lmap get schema) n

fieldWith' :: TypedSchemaFlex from a -> Text -> RecordFields from a
fieldWith' schema n = RecordFields $ liftAlt (RequiredAp n schema)

data TryFailed = TryFailed
 deriving (Exception, Show, Typeable)

optField :: forall a from. HasSchema a => Text -> (from -> Maybe a) -> RecordFields from (Maybe a)
optField n get = optFieldWith (lmap get $ liftMaybe (schema @a)) n

-- | Project a schema through a Prism. The resulting schema is empty if the Prism doesn't fit
liftPrism :: Prism s t a b -> TypedSchemaFlex a b -> TypedSchemaFlex s t
liftPrism p sc = withPrism p $ \t f -> rmap t (TTry sc (either (const Nothing) Just . f))

-- | Use this to build schemas for 'optFieldWith'. The resulting schema is empty for the Nothing case
liftMaybe :: TypedSchemaFlex a b -> TypedSchemaFlex (Maybe a) (Maybe b)
liftMaybe = liftPrism _Just

-- | Use this to build schemas for 'optFieldEitherWith'. The resulting schema is empty for the Left case
liftEither :: TypedSchemaFlex a b -> TypedSchemaFlex (Either c a) (Either c b)
liftEither = liftPrism _Right

-- | A generalized version of 'optField'. Does not handle infinite/circular data.
optFieldWith
    :: forall a from
     . TypedSchemaFlex from (Maybe a)
    -> Text
    -> RecordFields from (Maybe a)
optFieldWith schema n = RecordFields $ liftAlt (OptionalAp n schema Nothing)

optFieldGeneral
    :: forall a from
     . TypedSchemaFlex from a
    -> Text
    -> a
    -> RecordFields from a
optFieldGeneral schema n def = RecordFields $ liftAlt (OptionalAp n schema def)

optFieldEither
    :: forall a from e
     . HasSchema a
    => Text
    -> (from -> Either e a)
    -> e
    -> RecordFields from (Either e a)
optFieldEither n from e = optFieldGeneral (lmap from $ liftEither schema) n (Left e)

-- | A generalized version of 'optFieldEither'. Does not handle infinite/circular data
optFieldEitherWith
    :: TypedSchemaFlex from (Either e a) -> Text -> e -> RecordFields from (Either e a)
optFieldEitherWith schema n e = optFieldGeneral schema n (Left e)

-- | Extract all the field groups (from alternatives) in the record
extractFields :: RecordFields from a -> [[(Text, Field)]]
extractFields = extractFieldsHelper extractField
  where
    extractField :: RecordField from a -> (Text, Field)
    extractField (RequiredAp n sc) = (n,) . (`Field` True) $ extractSchema sc
    extractField (OptionalAp n sc _) = (n,) . (`Field` False) $ extractSchema sc

extractFieldsHelper :: (forall a . RecordField from a -> b) -> RecordFields from a -> [[b]]
extractFieldsHelper f = runAlt_ (\x -> (f x : []) : []) . getRecordFields

-- --------------------------------------------------------------------------------
-- Typed Unions

union :: (NonEmpty (Text, TypedSchema a)) -> TypedSchema a
union args = TOneOf (mk <$> args)
 where
  mk (name, sc) = RecordSchema $ fieldWith' sc name

data UnionTag from where
  UnionTag :: Text -> Prism' from b -> TypedSchema b -> UnionTag from

alt :: HasSchema a => Text -> Prism' from a -> UnionTag from
alt = altWith schema

altWith :: TypedSchema a -> Text -> Prism' from a -> UnionTag from
altWith sc n p = UnionTag n p sc

union' :: (NonEmpty (UnionTag from)) -> TypedSchema from
union' args = union $ args <&> \(UnionTag c p sc) -> (c, liftPrism p sc)

-- HasSchema class and instances
-- -----------------------------------------------------------------------------------

class HasSchema a where
  schema :: TypedSchema a

instance HasSchema () where
  schema = mempty

instance HasSchema Bool where
  schema = viaJSON

instance HasSchema Double where
  schema = viaJSON

instance HasSchema Scientific where
  schema = viaJSON

instance HasSchema Int where
  schema = viaJSON

instance HasSchema Integer where
  schema = viaJSON

instance HasSchema Natural where
  schema = viaJSON

instance {-# OVERLAPPING #-} HasSchema String where
  schema = viaJSON

instance HasSchema Text where
  schema = viaJSON

instance {-# OVERLAPPABLE #-} HasSchema a => HasSchema [a] where
  schema = list schema

instance HasSchema a => HasSchema (Vector a) where
  schema = TArray schema id id

instance (Eq a, Hashable a, HasSchema a) => HasSchema (HashSet a) where
  schema = list schema

instance  HasSchema a => HasSchema (NonEmpty a) where
  schema = list schema

instance HasSchema Field where
  schema = record $ Field <$> field "schema" fieldSchema <*> fmap
    (fromMaybe True)
    (optField "isRequired" (\x -> if isRequired x then Nothing else Just False))

instance HasSchema a => HasSchema (Identity a) where
  schema = dimap runIdentity Identity schema

instance HasSchema Schema where
  schema = union'
    [ alt "StringMap" #_StringMap
    , alt "Array"     #_Array
    , alt "Enum"      #_Enum
    , alt "Record"    #_Record
    , alt "Empty"     #_Empty
    , alt "AllOf"     #_AllOf
    , alt "Prim"      #_Prim
    , altWith unionSchema "Union" _Union
    , alt "OneOf"     #_OneOf
    ]
    where
      unionSchema = list (record $ (,) <$> field "constructor" fst <*> field "schema" snd)

instance HasSchema Value where
  schema = viaJSON

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
  schema = union' [alt "Left" #_Left, alt "Right" #_Right]

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
  go d (Array     sc  ) = Array (go (max 0 (pred d)) sc)
  go d (StringMap sc  ) = StringMap (go (max 0 (pred d)) sc)
  go d (AllOf     opts) = let d' = max 0 (pred d) in AllOf (finite d' <$> opts)
  go d (OneOf     opts) = let d' = max 0 (pred d) in OneOf (finite d' <$> opts)
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
extractSchema TPrim{}          = Prim
extractSchema (TTry sc _)      = extractSchema sc
extractSchema (TOneOf scc)     = OneOf $ extractSchema <$> scc
extractSchema (TAllOf scc)     = AllOf $ extractSchema <$> scc
extractSchema TEmpty{}         = Empty
extractSchema (TEnum opts  _)  = Enum (fst <$> opts)
extractSchema (TArray sc _ _)  = Array $ extractSchema sc
extractSchema (TMap sc _ _)    = StringMap $ extractSchema sc
extractSchema (RecordSchema rs) = foldMap (Record . fromList) $ extractFields rs

theSchema :: forall a . HasSchema a => Schema
theSchema = extractSchema (schema @a)

-- ---------------------------------------------------------------------------------------
-- Encoding to JSON

-- | Given a value and its typed schema, produce a JSON record using the 'RecordField's
encodeWith :: TypedSchemaFlex from a -> from -> Value
  -- TODO Better error messages to help debug partial schemas ?
encodeWith sc = either (throw . head) id . runExcept . go sc where
  go :: TypedSchemaFlex from a -> from -> Except [SomeException] Value
  go (TAllOf scc) x = encodeAlternatives . NE.toList =<< traverse (`go` x) scc
  go (TOneOf scc) x = asum (fmap (`go` x) scc)
  go (TTry   sc  try   ) x = go sc =<< maybe (throwE [toException TryFailed]) pure (try x)
  go (TEnum  _   fromf ) b = pure $ A.String (fromf b)
  go (TPrim  _   fromf ) b = pure $ fromf b
  go (TEmpty _         ) _ = pure emptyValue
  go (TArray sc _ fromf) b = A.Array <$> go sc `traverse` fromf b
  go (TMap   sc _ fromf) b = A.Object <$> go sc `traverse` fromf b
  go (RecordSchema rec ) x = do
    fields' <- traverse((`catchE` \_ -> pure Nothing) . fmap Just . sequence) fields
    encodeAlternatives $ fmap (A.Object . fromList . catMaybes) (catMaybes fields')
   where
    fields = extractFieldsHelper (extractField x) rec

    extractField b RequiredAp {..} =  Just . (fieldName,) <$> go fieldTypedSchema b
    extractField b OptionalAp {..} = (Just . (fieldName,) <$> go fieldTypedSchema b) `catchE` \_ -> pure Nothing

encodeAlternatives :: [Value] -> Except [SomeException] Value
encodeAlternatives [] = throwE []
encodeAlternatives [x] = pure x
encodeAlternatives xx  = pure $ A.object $ fromList [ ("#" <> pack (show i), x) | (i,x) <- zip [(1::Int)..] xx ]

-- | encode using the default schema
encode :: HasSchema a => a -> Value
encode = encodeWith schema

encodeToWith :: TypedSchema a -> Schema -> Maybe (a -> Value)
encodeToWith sc target = case extractSchema sc `isSubtypeOf` target of
  Just cast -> Just $ cast . encodeWith sc
  Nothing   -> Nothing

encodeTo :: HasSchema a => Schema -> Maybe (a -> Value)
encodeTo = encodeToWith schema

-- | Encode a value into a finite representation by enforcing a max depth
finiteEncode :: forall a. HasSchema a => Natural -> a -> Value
finiteEncode d = finiteValue d (theSchema @a) . encode

-- --------------------------------------------------------------------------
-- Decoding

type Trace = [Text]

data DecodeError
  = VE ValidationError
  | TriedAndFailed
  deriving (Eq, Show)

-- | Runs a schema as a function @enc -> dec@. Loops for infinite/circular data
runSchema :: TypedSchemaFlex enc dec -> enc -> Either [DecodeError] dec
runSchema sc = runExcept . go sc
    where
        go :: forall from a. TypedSchemaFlex from a -> from -> Except [DecodeError] a
        go (TEmpty a       ) _    = pure a
        go (TTry sc try) from = maybe (throwE [TriedAndFailed]) (go sc) (try from)
        go (TPrim toF fromF) from = case toF (fromF from) of
            A.Success a -> pure a
            A.Error   e -> failWith (PrimError e)
        go (TEnum opts fromF) from = case lookup enumValue opts of
            Just x  -> pure x
            Nothing -> failWith $ InvalidEnumValue enumValue (fst <$> opts)
            where enumValue = fromF from
        go (TMap   _sc toF fromF) from = pure $ toF (fromF from)
        go (TArray _sc toF fromF) from = pure $ toF (fromF from)
        go (TAllOf scc          ) from = msum $ (`go` from) <$> scc
        go (TOneOf scc          ) from = msum $ (`go` from) <$> scc
        go (RecordSchema fields ) from = runAlt f (getRecordFields fields)
            where
                f :: RecordField from b -> Except [DecodeError] b
                f RequiredAp{..} = go fieldTypedSchema from
                f OptionalAp{..} = go fieldTypedSchema from

        failWith e = throwE [VE e]

-- | Given a JSON 'Value' and a typed schema, extract a Haskell value
decodeWith :: TypedSchemaFlex from a -> Value -> Either [(Trace, DecodeError)] a
-- TODO merge runSchema and decodeWith ?
-- TODO change decode type to reflect partiality due to TTry?
decodeWith sc = runExcept . go [] sc
 where
  failWith ctx e = throwE [(reverse ctx, VE e)]

  go
    :: [Text]
    -> TypedSchemaFlex from a
    -> Value
    -> Except [(Trace, DecodeError)] a
  go ctx (TEnum opts _) (A.String x) =
    maybe (failWith ctx (InvalidEnumValue x (fst <$> opts))) pure
      $ lookup x opts
  go ctx (TArray sc tof _) (A.Array x) =
    tof <$> traverse (go ("[]" : ctx) sc) x
  go ctx (TMap sc tof _) (A.Object x) = tof <$> traverse (go ("[]" : ctx) sc) x
  go _tx (TEmpty a) _ = pure a
  go ctx (RecordSchema rec) o@A.Object{} = do
    let alts = decodeAlternatives o
    asum $ concatMap
      (\(A.Object fields, _encodedPath) ->
          getCompose $ runAlt (Compose . (: []) . f fields) (getRecordFields rec)
      )
      alts
   where
    f :: A.Object -> RecordField from a -> Except [(Trace, DecodeError)] a
    f fields (RequiredAp n sc) = case Map.lookup n fields of
      Just v  -> go (n : ctx) sc v
      Nothing -> case sc of
        TArray _ tof' _ -> pure $ tof' []
        _               -> failWith ctx (MissingRecordField n)
    f fields OptionalAp {..} = case Map.lookup fieldName fields of
      Just v  -> go (fieldName : ctx) fieldTypedSchema v
      Nothing -> pure fieldDefValue

  -- The TAllOf case is probably wrong. I suspect TAllOf should decode very much like TOneOf
  go ctx (TAllOf scc) v = asum $ map
    (\(v', i) -> go
      (pack (show i) : ctx)
      (fromMaybe (error "TAllOf") $ selectPath i (NE.toList scc))
      v'
    )
    (decodeAlternatives v)
  go ctx (TOneOf scc ) x = asum [ go ctx sc x | sc <- NE.toList scc ]

  go ctx (TPrim tof _) x = case tof x of
    A.Error   e -> failWith ctx (PrimError e)
    A.Success a -> pure a
  go ctx (TTry sc _try) x = go ctx sc x
  go ctx _              _ = failWith ctx SchemaMismatch

decode :: HasSchema a => Value -> Either [(Trace, DecodeError)] a
decode = decodeWith schema

decodeFromWith :: TypedSchema a -> Schema -> Maybe (Value -> Either [(Trace, DecodeError)] a)
decodeFromWith sc source = case source `isSubtypeOf` extractSchema sc of
  Just cast -> Just $ decodeWith sc . cast
  Nothing   -> Nothing

decodeFrom :: HasSchema a => Schema -> Maybe (Value -> Either [(Trace, DecodeError)] a)
decodeFrom = decodeFromWith schema

type Path = Int

decodeAlternatives :: Value -> [(Value, Path)]
decodeAlternatives obj@(A.Object x) =
  case
      [ (v, n) | (unpack -> '#' : (readMaybe -> Just n), v) <- Map.toList x ]
    of
      []    -> [(obj, 0)]
      other -> other
decodeAlternatives x = [(x,0)]

selectPath :: Path -> [a] -> Maybe a
selectPath 0 (x : _)  = Just x
selectPath n (_ : xx) = selectPath (pred n) xx
selectPath _ _        = Nothing

-- ------------------------------------------------------------------------------------------------------
-- Versions

-- | Flattens alternatives. Returns a schema without 'AllOf' constructors
versions :: Schema -> NonEmpty Schema
versions (AllOf scc) = join $ traverse versions scc
versions (OneOf scc) = OneOf <$> traverse versions scc
versions (Record fields) = Record <$> ((traverse . #fieldSchema) versions fields)
versions (Array sc) = Array <$> versions sc
versions (StringMap sc) = StringMap <$> versions sc
versions x = [x]

-- ------------------------------------------------------------------------------------------------------
-- Validation

data ValidationError
  = MissingRecordField { name :: Text}
  | InvalidEnumValue   { given :: Text, options :: NonEmpty Text}
  | InvalidConstructor { name :: Text}
  | InvalidUnionValue { contents :: Value}
  | SchemaMismatch
  | EmptyAllOf
  | PrimError {viaJSONError :: String}
  | InvalidChoice{choiceNumber :: Int}
  deriving (Eq, Show)

-- | Structural validation of a JSON value against a schema
--   Ignores extraneous fields in records
validate :: Schema -> Value -> [(Trace, ValidationError)]
validate sc v = either (fmap (first reverse)) (\() -> [])
  $ runExcept (go [] sc v) where
  go :: Trace -> Schema -> Value -> Except [(Trace, ValidationError)] ()
  go _   Prim           _             = pure ()
  go ctx (StringMap sc) (A.Object xx) = ifor_ xx $ \i -> go (i : ctx) sc
  go ctx (Array sc) (A.Array xx) =
    ifor_ xx $ \i -> go (pack ("[" <> show i <> "]") : ctx) sc
  go ctx (Enum opts) (A.String s) =
    if s `elem` opts then pure () else throwE [(ctx, InvalidEnumValue s opts)]
  go ctx (Record ff) (A.Object xx) = ifor_ ff $ \n (Field sc opt) ->
    case (opt, Map.lookup n xx) of
      (_   , Just y ) -> go (n : ctx) sc y
      (True, Nothing) -> pure ()
      _               -> throwE [(ctx, MissingRecordField n)]
  go ctx (Union constructors) v@(A.Object xx) = case toList xx of
    [(n, v)] | Just sc <- lookup n constructors -> go (n : ctx) sc v
             | otherwise -> throwE [(ctx, InvalidConstructor n)]
    _ -> throwE [(ctx, InvalidUnionValue v)]
  go ctx (OneOf scc) v = case decodeAlternatives v of
    [(v, 0)] -> msum $ fmap (\sc -> go ctx sc v) scc
    alts     -> msum $ fmap
      (\(v, n) ->
        fromMaybe (throwE [(ctx, InvalidChoice n)]) $ selectPath n $ fmap
          (\sc -> go (pack (show n) : ctx) sc v)
          (toList scc)
      )
      alts
  go ctx (AllOf scc) v = go ctx (OneOf scc) v
  go ctx _           _ = throwE [(ctx, SchemaMismatch)]

-- ------------------------------------------------------------------------------------------------------
-- Subtype relation

-- | @sub `isSubtypeOf` sup@ returns a witness that @sub@ is a subtype of @sup@, i.e. a cast function @sub -> sup@
--
-- > Array Bool `isSubtypeOf` Bool
--   Just <function>
-- > Record [("a", Bool)] `isSubtypeOf` Record [("a", Number)]
--   Nothing
isSubtypeOf :: Schema -> Schema -> Maybe (Value -> Value)
isSubtypeOf sub sup = go sup sub
 where
        -- TODO go: fix confusing order of arguments
  go Empty         _         = pure $ const emptyValue
  go (Array     _) Empty     = pure $ const (A.Array [])
  go (Record    _) Empty     = pure $ const emptyValue
  go (StringMap _) Empty     = pure $ const emptyValue
  go OneOf{}       Empty     = pure $ const emptyValue
  go (Array a)     (Array b) = do
    f <- go a b
    pure $ over (_Array . traverse) f
  go (StringMap a) (StringMap b) = do
    f <- go a b
    pure $ over (_Object . traverse) f
  go a (Array b) | a == b = Just (A.Array . fromList . (: []))
  go (Enum opts) (Enum opts') | all (`elem` opts') opts = Just id
  go (Union opts) (Union opts') = do
    ff <- forM opts' $ \(n, sc) -> do
      sc' <- lookup n (toList opts)
      f   <- go sc sc'
      return $ over (_Object . ix n) f
    return (foldr (.) id ff)
  go (Record opts) (Record opts') = do
    forM_ (Map.toList opts) $ \(n, f@(Field _ _)) ->
      guard $ not (isRequired f) || Map.member n opts'
    ff <- forM (Map.toList opts') $ \(n', f'@(Field sc' _)) -> do
      case Map.lookup n' opts of
        Nothing -> do
          Just $ over (_Object) (Map.delete n')
        Just f@(Field sc _) -> do
          guard (not (isRequired f) || isRequired f')
          witness <- go sc sc'
          Just $ over (_Object . ix n') witness
    return (foldr (.) id ff)
  go (AllOf sup) sub = do
    (i, c) <- msum $ imap (\i sup' -> (i,) <$> go sup' sub) sup
    return $ \v -> A.object [("#" <> pack (show i), c v)]
  go sup (AllOf scc) = asum
    [ go sup b <&> \f ->
        fromMaybe
            (  error
            $  "failed to upcast an AllOf value due to missing entry: "
            <> field
            )
          . preview (_Object . ix (pack field) . to f)
    | (i, b) <- zip [(1 :: Int) ..] (NE.toList scc)
    , let field = "#" <> show i
    ]
  go sup (OneOf [sub]) = go sup sub
  go sup (OneOf sub  ) = do
    alts <- traverse (\sc -> (sc, ) <$> go sup sc) sub
    return $ \v -> head $ mapMaybe
      (\(sc, f) -> if null (validate sc v) then Just (f v) else Nothing)
      (toList alts)
  go (OneOf sup) sub = asum $ fmap (`go` sub) sup
  go a b | a == b  = pure id
  go _ _           = Nothing

-- | Coerce from 'sub' to 'sup'Returns 'Nothing' if 'sub' is not a subtype of 'sup'
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

(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
f <.> g = fmap f . g

infixr 9 <.>

