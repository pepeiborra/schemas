{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# OPTIONS -Wno-name-shadowing    #-}
module Schemas.Internal where

import           Control.Alternative.Free
import           Control.Applicative        (Alternative (..))
import           Control.Exception
import           Control.Lens               hiding (Empty, enum)
import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.Aeson                 (Value)
import qualified Data.Aeson                 as A
import           Data.Aeson.Lens
import           Data.Biapplicative
import           Data.Either
import           Data.Functor.Compose
import           Data.Generics.Labels       ()
import           Data.Hashable
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as Set
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
import           GHC.Exts                   (fromList)
import           GHC.Generics               (Generic)
import           Numeric.Natural
import           Prelude                    hiding (lookup)

-- Schemas
-- --------------------------------------------------------------------------------

data Schema
  = Empty
  | Array Schema
  | StringMap Schema
  | Enum   (NonEmpty Text)
  | Record (HashMap Text Field)
  | Union  (HashMap Text Schema)
  | Or Schema Schema
  | Prim
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

-- | @TypedSchemaFlex enc dec@ is a schema for encoding to @enc@ and decoding to @dec@.
--   Usually we want @enc@ and @dec@ to be the same type but this flexibility comes in handy
--   when composing typed schemas.
data TypedSchemaFlex from a where
  TEnum   :: (NonEmpty (Text, a)) -> (from -> Text) -> TypedSchemaFlex from a
  TArray :: TypedSchema b -> (Vector b -> a) -> (from -> Vector b) -> TypedSchemaFlex from a
  TMap   :: TypedSchema b -> (HashMap Text b -> a) -> (from -> HashMap Text b) -> TypedSchemaFlex from a
  TOr    :: TypedSchemaFlex from a -> TypedSchemaFlex from a -> TypedSchemaFlex from a
  TEmpty :: a -> TypedSchemaFlex from a
  TPrim  :: (Value -> A.Result a) -> (from -> Value) -> TypedSchemaFlex from a
  -- TTry is used to implement 'optField' on top of 'optFieldWith'
  -- it could be exposed to provide some form of error handling, but currently is not
  TTry   :: TypedSchemaFlex a b -> (a' -> Either SomeException a) -> TypedSchemaFlex a' b
  RecordSchema :: RecordFields from a -> TypedSchemaFlex from a
  UnionSchema :: (NonEmpty (Text, TypedSchemaFlex from a)) -> (from -> Text) -> TypedSchemaFlex from a

enum :: Eq a => (a -> Text) -> (NonEmpty a) -> TypedSchema a
enum showF opts = TEnum alts (fromMaybe (error "invalid alt") . flip lookup altMap)
 where
  altMap = fmap swap $ alts --TODO fast lookup
  alts   = opts <&> \x -> (showF x, x)

stringMap :: TypedSchema a -> TypedSchema (HashMap Text a)
stringMap sc = TMap sc id id

list :: TypedSchema a -> TypedSchema [a]
list schema = TArray schema V.toList V.fromList

viaJSON :: (A.FromJSON a, A.ToJSON a) => TypedSchema a
viaJSON = TPrim A.fromJSON A.toJSON

viaIso :: Iso' a b -> TypedSchema a -> TypedSchema b
viaIso iso sc = withIso iso $ \from to -> dimap to from sc

readShow :: (Read a, Show a) => TypedSchema a
readShow = dimap show read schema

instance Functor (TypedSchemaFlex from) where
  fmap = rmap

instance Profunctor TypedSchemaFlex where
    dimap _ f (TEmpty a                ) = TEmpty (f a)
    dimap g f (TTry sc try) = TTry (rmap f sc) (try . g)
    dimap g f (TOr       a    b        ) = TOr (dimap g f a) (dimap g f b)
    dimap g f (TEnum     opts fromf    ) = TEnum (second f <$> opts) (fromf . g)
    dimap g f (TArray      sc tof fromf) = TArray sc (f . tof) (fromf . g)
    dimap g f (TMap        sc tof fromf) = TMap sc (f . tof) (fromf . g)
    dimap g f (TPrim          tof fromf) = TPrim (fmap f . tof) (fromf . g)
    dimap g f (RecordSchema sc) = RecordSchema (dimap g f sc)
    dimap g f (UnionSchema tags getTag ) = UnionSchema (second (dimap g f) <$> tags) (getTag . g)

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

data RecordField from a where
  RequiredAp :: { fieldName :: Text
                , fieldTypedSchema :: TypedSchemaFlex from a
                } -> RecordField from a
  OptionalAp :: { fieldName :: Text
                , fieldTypedSchema :: TypedSchemaFlex from a
                , fieldOpt :: from -> Bool
                , fieldDefValue :: a
                } -> RecordField from a

instance Profunctor RecordField where
  dimap f g (RequiredAp name sc) = RequiredAp name (dimap f g sc)
  dimap f g (OptionalAp name sc opt def) = OptionalAp name (dimap f g sc) (opt . f) (g def)

newtype RecordFields from a = RecordFields {getRecordFields :: Alt (RecordField from) a}
  deriving newtype (Alternative, Applicative, Functor, Monoid, Semigroup)

instance Profunctor RecordFields where
  dimap f g = RecordFields . hoistAlt (lmap f) . fmap g . getRecordFields

-- | Define a record schema using applicative syntax
record :: RecordFields from a -> TypedSchemaFlex from a
record = RecordSchema

field :: HasSchema a => Text -> (from -> a) -> RecordFields from a
field = fieldWith schema

fieldWith :: TypedSchema a -> Text -> (from -> a) -> RecordFields from a
fieldWith schema n get = fieldWith' (lmap get schema) n

fieldWith' :: TypedSchemaFlex from a -> Text -> RecordFields from a
fieldWith' schema n = RecordFields $ liftAlt (RequiredAp n schema)

data OptFieldE = OptFieldE
 deriving (Exception, Show, Typeable)

optField :: forall a from. HasSchema a => Text -> (from -> Maybe a) -> RecordFields from (Maybe a)
optField n get = optFieldGeneral (lmap get $ liftMaybe (schema @a)) n (isJust . get) Nothing

-- | Use this to build schemas for 'optFieldWith'. The resulting schema fails cleanly for the Nothing case
liftMaybe :: TypedSchema a -> TypedSchema (Maybe a)
liftMaybe = liftMaybe' OptFieldE

liftMaybe' :: Exception e => e -> TypedSchema a -> TypedSchema (Maybe a)
liftMaybe' e schema = rmap Just (TTry schema (maybe (Left $ toException e) Right))

-- | A generalized version of 'optField'. Does not handle infinite/circular data.
optFieldWith
    :: forall a from
     . TypedSchemaFlex from (Maybe a)
    -> Text
    -> RecordFields from (Maybe a)
optFieldWith schema n = optFieldGeneral schema n (isJust . either (const Nothing) id . runSchema schema) Nothing

optFieldGeneral :: TypedSchemaFlex from a -> Text -> (from -> Bool) -> a -> RecordFields from a
optFieldGeneral schema n opt def = RecordFields $ liftAlt (OptionalAp n schema opt def)

optFieldEither
    :: forall a from e
     . HasSchema a
    => Text
    -> (from -> Either e a)
    -> e
    -> RecordFields from (Either e a)
optFieldEither n from e =
  optFieldGeneral (lmap from $ liftEither schema) n (isRight . from) (Left e)

-- | Use this to build schemas for 'optFieldEitherWith'. The resulting schema fails cleanly for the Left case
liftEither :: TypedSchema a -> TypedSchema (Either b a)
liftEither = liftEither' OptFieldE

liftEither' :: Exception e => e -> TypedSchema a -> TypedSchema (Either b a)
liftEither' e schema = rmap Right (TTry schema (either (const $ Left $ toException e) Right))

-- | A generalized version of 'optFieldEither'. Does not handle infinite/circular data
optFieldEitherWith
    :: TypedSchemaFlex from (Either e a) -> Text -> e -> RecordFields from (Either e a)
optFieldEitherWith schema n e = optFieldGeneral schema n (either (const False) isRight . runSchema schema) (Left e)

-- | Extract all the field groups (from alternatives) in the record
extractFields :: RecordFields from a -> [[(Text, Field)]]
extractFields = runAlt_ ((:[]) . (:[]) . extractField) . getRecordFields
  where
    extractField :: RecordField from a -> (Text, Field)
    extractField (RequiredAp n sc) = (n,) . (`Field` Nothing) $ extractSchema sc
    extractField (OptionalAp n sc _ _) = (n,) . (`Field` Just False) $ extractSchema sc

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
  schema = TArray schema V.toList V.fromList

instance HasSchema a => HasSchema (Vector a) where
  schema = TArray schema id id

instance (Eq a, Hashable a, HasSchema a) => HasSchema (HashSet a) where
  schema = dimap Set.toList Set.fromList schema

instance  HasSchema a => HasSchema (NonEmpty a) where
  schema = TArray schema (NE.fromList . V.toList) (V.fromList . NE.toList)

instance HasSchema Field where
  schema = record $ Field <$> field "schema" fieldSchema <*> optField "field'" isRequired

instance HasSchema a => HasSchema (Identity a) where
  schema = dimap runIdentity Identity schema

instance HasSchema Schema where
  schema = union'
    [ alt "StringMap" #_StringMap
    , alt "Array" #_Array
    , alt "Enum" #_Enum
    , alt "Record" #_Record
    , alt "Union" #_Union
    , alt "Empty" #_Empty
    , alt "Or" #_Or
    , alt "Prim" #_Prim
    ]

instance HasSchema Value where
  schema = viaJSON

instance (HasSchema a, HasSchema b) => HasSchema (a,b) where
  schema = record $ (,) <$> field "$1" fst <*> field "$2" snd

instance (HasSchema a, HasSchema b, HasSchema c) => HasSchema (a,b,c) where
  schema = record $ (,,) <$> field "$1" (view _1) <*> field "$2" (view _2) <*> field "$3" (view _3)

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
  go d (Union     opts) = Union (fmap (go (max 0 (pred d))) opts)
  go d (Array     sc  ) = Array (go (max 0 (pred d)) sc)
  go d (StringMap sc  ) = StringMap (go (max 0 (pred d)) sc)
  go d (Or a b        ) = let d' = max 0 (pred d) in Or (finite d' a) (finite d' b)
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
extractSchema (TOr a b)        = Or (extractSchema a) (extractSchema b)
extractSchema TEmpty{}         = Empty
extractSchema (TEnum opts  _)  = Enum (fst <$> opts)
extractSchema (TArray sc _ _)  = Array $ extractSchema sc
extractSchema (TMap sc _ _)    = StringMap $ extractSchema sc
extractSchema (RecordSchema rs) = foldMap (Record . fromList) $ extractFields rs
extractSchema (UnionSchema scs _getTag) =
  Union . Map.fromList . NE.toList $ fmap (\(n, sc) -> (n, extractSchema sc)) scs

theSchema :: forall a . HasSchema a => Schema
theSchema = extractSchema (schema @a)

-- ---------------------------------------------------------------------------------------
-- Encoding

-- | Given a value and its typed schema, produce a JSON record using the 'RecordField's
encodeWith :: TypedSchemaFlex from a -> from -> Value
encodeWith (TOr a b) x = encodeAlternatives [encodeWith a x, encodeWith b x]
 -- TODO change return type of encodeWith to reflect partiality in the presence of TTry
encodeWith (TTry sc try) x = encodeWith sc (either throw id $ try x)
encodeWith (TEnum   _ fromf        ) b = A.String (fromf b)
encodeWith (TPrim   _ fromf        ) b = fromf b
encodeWith (TEmpty _               ) _ = A.object []
encodeWith (TArray      sc  _ fromf) b = A.Array (encodeWith sc <$> fromf b)
encodeWith (TMap        sc  _ fromf) b = A.Object (encodeWith sc <$> fromf b)
encodeWith (RecordSchema rec) x = encodeAlternatives $ fmap (A.Object . fromList) fields
            where
                fields = runAlt_ (maybe [[]] ((: []) . (: [])) . extractFieldAp x) (getRecordFields rec)

                extractFieldAp b RequiredAp{..} = Just (fieldName, encodeWith fieldTypedSchema b)
                extractFieldAp b OptionalAp{..} = if fieldOpt b
                  then Just (fieldName, encodeWith fieldTypedSchema b)
                  else Nothing

encodeWith (UnionSchema [(_, sc)] _    ) x = encodeWith sc x
encodeWith (UnionSchema opts      fromF) x = case lookup tag opts of
            Nothing       -> error $ "Unknown tag: " <> show tag
            Just TEmpty{} -> A.String tag
            Just sc       -> A.object [tag A..= encodeWith sc x]
            where tag = fromF x

encodeAlternatives :: [Value] -> Value
encodeAlternatives [] = error "empty"
encodeAlternatives [x] = x
encodeAlternatives (x:xx) = A.object ["L" A..= x, "R" A..= encodeAlternatives xx]

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
  = InvalidRecordField { name :: Text}
  | MissingRecordField { name :: Text}
  | InvalidEnumValue { given :: Text, options :: NonEmpty Text}
  | InvalidConstructor { name :: Text}
  | InvalidUnionType { contents :: Value}
  | SchemaMismatch
  | InvalidAlt {path :: Path}
  | PrimError {viaJSONError :: String}
  | TriedAndFailed
  deriving (Eq, Show)

-- | Runs a schema as a function @enc -> dec@. Loops for infinite/circular data
runSchema :: TypedSchemaFlex enc dec -> enc -> Either [DecodeError] dec
runSchema sc = runExcept . go sc
    where
        go :: forall from a. TypedSchemaFlex from a -> from -> Except [DecodeError] a
        go (TEmpty a       ) _    = pure a
        go (TTry sc try) from = either (const $ failWith TriedAndFailed) (go sc) (try from)
        go (TPrim toF fromF) from = case toF (fromF from) of
            A.Success a -> pure a
            A.Error   e -> failWith (PrimError e)
        go (TEnum opts fromF) from = case lookup enumValue opts of
            Just x  -> pure x
            Nothing -> failWith $ InvalidEnumValue enumValue (fst <$> opts)
            where enumValue = fromF from
        go (TMap   _sc toF fromF) from = pure $ toF (fromF from)
        go (TArray _sc toF fromF) from = pure $ toF (fromF from)
        go (TOr a b             ) from = go a from <|> go b from
        go (RecordSchema fields ) from = runAlt f (getRecordFields fields)
            where
                f :: RecordField from b -> Except [DecodeError] b
                f RequiredAp{..} = go fieldTypedSchema from
                f OptionalAp{..} = go fieldTypedSchema from <|> pure fieldDefValue
        go (UnionSchema opts tag) from = case lookup theTag opts of
            Just sc -> go sc from
            Nothing -> failWith (InvalidConstructor theTag)
            where theTag = (tag from)

        failWith = throwE . (:[])

-- | Given a JSON 'Value' and a typed schema, extract a Haskell value
decodeWith :: TypedSchemaFlex from a -> Value -> Either (Trace, DecodeError) a
-- TODO merge runSchema and decodeWith ?
decodeWith = go []
    where
        go :: [Text] -> TypedSchemaFlex from a -> Value -> Either (Trace, DecodeError) a
        go ctx (TEnum opts _) (A.String x) =
            maybe (Left (ctx, InvalidEnumValue x (fst <$> opts))) pure $ lookup x opts
        go ctx (TArray sc tof _) (A.Array  x) = tof <$> traverse (go ("[]" : ctx) sc) x
        go ctx (TMap   sc tof _) (A.Object x) = tof <$> traverse (go ("[]" : ctx) sc) x
        go _tx (TEmpty a       ) _            = pure a
        go ctx (RecordSchema rec) o@A.Object{}
            | (A.Object fields, encodedPath) <- decodeAlternatives o = fromMaybe
                (Left (ctx, InvalidAlt encodedPath))
                (selectPath encodedPath (getCompose $ runAlt (Compose . (: []) . f fields) (getRecordFields rec)))
            where
                f :: A.Object -> RecordField from a -> Either (Trace, DecodeError) a
                f fields (RequiredAp n sc) = case Map.lookup n fields of
                    Just v  -> go (n : ctx) sc v
                    Nothing -> case sc of
                        TArray _ tof' _ -> pure $ tof' []
                        _               -> Left (ctx, MissingRecordField n)
                f fields OptionalAp{..} = case Map.lookup fieldName fields of
                    Just v  -> go (fieldName : ctx) fieldTypedSchema v
                    Nothing -> pure fieldDefValue

        go _tx (UnionSchema opts _) (A.String n) | Just (TEmpty a) <- lookup n opts = pure a
        go ctx (UnionSchema opts _) it@(A.Object x) = case Map.toList x of
            [(n, v)] -> case lookup n opts of
                Just sc -> go (n : ctx) sc v
                Nothing -> Left (ctx, InvalidConstructor n)
            _ -> Left (ctx, InvalidUnionType it)
        go ctx (TOr a b) (A.Object x) = do
            let l = Map.lookup "L" x <&> go ("L" : ctx) a
            let r = Map.lookup "R" x <&> go ("R" : ctx) b
            fromMaybe (Left (ctx, SchemaMismatch)) $ l <|> r
        go ctx (TPrim tof _) x = case tof x of
            A.Error   e -> Left (ctx, PrimError e)
            A.Success a -> pure a
        go ctx (TTry sc _try) x = go ctx sc x
        go ctx _ _ = Left (reverse ctx, SchemaMismatch)

decode :: HasSchema a => Value -> Either (Trace, DecodeError) a
decode = decodeWith schema

decodeFromWith :: TypedSchema a -> Schema -> Maybe (Value -> Either (Trace, DecodeError) a)
decodeFromWith sc source = case source `isSubtypeOf` extractSchema sc of
  Just cast -> Just $ decodeWith sc . cast
  Nothing   -> Nothing

decodeFrom :: HasSchema a => Schema -> Maybe (Value -> Either (Trace, DecodeError) a)
decodeFrom = decodeFromWith schema

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
        go a (Array b) | a == b                               = Just (A.Array . fromList . (: []))
        go (Enum opts) (Enum opts') | all (`elem` opts') opts = Just id
        go (Union opts) (Union opts')                         = do
            ff <- forM (Map.toList opts) $ \(n, sc) -> do
                sc' <- Map.lookup n opts'
                f   <- go sc sc'
                return $ over (_Object . ix n) f
            return (foldr (.) id ff)
        go (Record opts) (Record opts') = do
            forM_ (Map.toList opts)
                $ \(n, f@(Field _ _)) -> guard $ not (isRequiredField f) || Map.member n opts'
            ff <- forM (Map.toList opts') $ \(n', f'@(Field sc' _)) -> do
                case Map.lookup n' opts of
                    Nothing -> do
                        Just $ over (_Object) (Map.delete n')
                    Just f@(Field sc _) -> do
                        guard (not (isRequiredField f) || isRequiredField f')
                        witness <- go sc sc'
                        Just $ over (_Object . ix n') witness
            return (foldr (.) id ff)
        go a (Or b c) =
            (go a b <&> \f -> fromMaybe (error "cannot upcast an alternative: missing L value")
                    . preview (_Object . ix "L" . to f)
                )
                <|> (go a c <&> \f ->
                        fromMaybe (error "cannot upcast an alternative: missing R value")
                            . preview (_Object . ix "R" . to f)
                    )
        go (Or a b) c =
            (go a c <&> ((A.object . (: []) . ("L" A..=)) .))
                <|> (go b c <&> ((A.object . (: []) . ("R" A..=)) .))
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

