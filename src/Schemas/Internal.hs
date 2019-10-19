{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# OPTIONS -Wno-name-shadowing    #-}
module Schemas.Internal where

import           Control.Alternative.Free
import           Control.Applicative        (Alternative (..))
import           Control.Exception
import           Control.Lens               hiding (Empty, enum, (<.>), allOf)
import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.Aeson                 (Value)
import qualified Data.Aeson                 as A
import           Data.Biapplicative
import           Data.Coerce
import           Data.Dynamic
import           Data.Either
import           Data.Foldable              (asum)
import           Data.Function              (fix)
import           Data.Functor.Compose
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import qualified Data.HashSet               as Set
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NE
import           Data.Maybe
import           Data.Semigroup
import           Data.Text                  (Text, pack)
import           Data.Tuple
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           GHC.Exts                   (IsList (..))
import           Prelude                    hiding (lookup)
import           Schemas.Untyped

-- import Debug.Pretty.Simple

-- Typed schemas
-- --------------------------------------------------------------------------------

-- | @TypedSchemaFlex v enc dec@ is a schema for encoding to @enc@ and decoding to @dec@.
--   Usually we want @enc@ and @dec@ to be the same type but this flexibility comes in handy
--   for composition.
--
--   * introduction forms: 'record', 'enum', 'schema'
--   * operations: 'encodeToWith', 'decodeFrom', 'extractSchema'
--   * composition: 'dimap', 'union', 'stringMap', 'liftPrism'
--
data TypedSchemaFlexMu v from a where
  TVar   :: v -> TypedSchemaFlexMu v from a
  TMu    :: (v -> TypedSchemaFlexMu v from a) -> TypedSchemaFlexMu v from a
  TEnum  :: (NonEmpty (Text, a)) -> (from -> Text) -> TypedSchemaFlexMu v from a
  TArray :: Typeable b => TypedSchemaFlexMu v b b -> (Vector b -> a) -> (from -> Vector b) -> TypedSchemaFlexMu v from a
  TMap   :: (Typeable b) => TypedSchemaFlexMu v b b -> (HashMap Text b -> a) -> (from -> HashMap Text b) -> TypedSchemaFlexMu v from a
  -- | Encoding and decoding support all alternatives
  TAllOf :: NonEmpty (TypedSchemaFlexMu v from a) -> TypedSchemaFlexMu v from a
  -- | Decoding from all alternatives, but encoding only to one
  TOneOf :: NonEmpty (TypedSchemaFlexMu v from a) -> TypedSchemaFlexMu v from a
  TEmpty :: a -> TypedSchemaFlexMu v from a
  TPrim  :: Text -> (Value -> A.Result a) -> (from -> Value) -> TypedSchemaFlexMu v from a
  -- TTry _ is used to implement 'optField' on top of 'optFieldWith'
  -- It's also crucial for implementing unions on top of TOneOf
  -- it could be exposed to provide some form of error handling, but currently is not
  TTry     :: Typeable a => Text -> TypedSchemaFlexMu v a b -> (a' -> Maybe a) -> TypedSchemaFlexMu v a' b
  RecordSchema :: RecordFieldsMu v from a -> TypedSchemaFlexMu v from a

type TypedSchemaFlex from a = forall v. TypedSchemaFlexMu v from a

type TypedSchemaMu v a = TypedSchemaFlexMu v a a
type TypedSchema a = TypedSchemaFlex a a

mu :: (TypedSchemaFlexMu v from a -> TypedSchemaFlexMu v from a) -> TypedSchemaFlexMu v from a
mu f = (TMu (\x -> (f (TVar x))))

-- mapMu :: (v -> v') -> (v' -> v) -> TypedSchemaFlexMu v a b -> TypedSchemaFlexMu v' a b
-- mapMu f _  (TVar v) = TVar (f v)
-- mapMu f f' (TMu g) = TMu (mapMu f f' . g . f')

-- | @enum values mapping@ construct a schema for a non empty set of values with a 'Text' mapping
enum :: Eq a => (a -> Text) -> (NonEmpty a) -> TypedSchemaMu v a
enum showF opts = TEnum alts (fromMaybe (error "invalid alt") . flip lookup altMap)
 where
  altMap = fmap swap $ alts --TODO fast lookup
  alts   = opts <&> \x -> (showF x, x)

-- | @stringMap sc@ is the schema for a stringmap where the values have schema @sc@
stringMap :: Typeable a => TypedSchemaMu v a -> TypedSchemaMu v (HashMap Text a)
stringMap sc = TMap sc id id

-- | @list sc@ is the schema for a list of values with schema @sc@
list :: (IsList l, Typeable (Item l)) => TypedSchemaMu v (Item l) -> TypedSchemaMu v l
list (schema) = TArray schema (fromList . V.toList) (V.fromList . toList)

-- | @vector sc@ is the schema for a vector of values with schema @sc@
vector :: Typeable a => TypedSchemaMu v a -> TypedSchemaMu v (Vector a)
vector sc = TArray sc id id

-- | @viaJson label@ constructs a schema reusing existing 'aeson' instances. The resulting schema
--  is opaque and cannot be subtyped and/or versioned, so this constructor should be used sparingly.
--  The @label@ is used to describe the extracted 'Schema'.
viaJSON :: (A.FromJSON a, A.ToJSON a) => Text -> TypedSchemaMu v a
viaJSON n = TPrim n A.fromJSON A.toJSON

-- | Apply an isomorphism to a schema
viaIso :: Iso' a b -> TypedSchemaMu v a -> TypedSchemaMu v b
viaIso iso sc = withIso iso $ \from to -> dimap to from sc

-- | The schema of String values
string :: TypedSchemaMu v String
string = viaJSON "String"

-- | A schema for types that can be parsed and pretty-printed. The resulting schema is opaque and cannot
-- be subtyped/versioned, so this constructor is best used for primitive value
readShow :: (Read a, Show a) => TypedSchemaMu v a
readShow = dimap show read string

-- | The schema of undiscriminated unions. Prefer using 'union' where possible
oneOf :: NonEmpty (TypedSchemaFlexMu v from a) -> TypedSchemaFlexMu v from a
oneOf [x] = x
oneOf x = TOneOf $ sconcat $ fmap f x where
  f (TOneOf xx) = xx
  f (x) = [x]

instance Profunctor (TypedSchemaFlexMu v) where
  dimap _ _ (TVar v) = TVar v
  dimap g f (TMu  h) = TMu (dimap g f . h)
  dimap _ f (TEmpty a                 ) = TEmpty (f a)
  dimap g f (TTry n       sc       try) = TTry n (rmap f sc) (try . g)
  dimap g f (TAllOf      scc          ) = TAllOf (dimap g f <$> scc)
  dimap g f (TOneOf      scc          ) = TOneOf (dimap g f <$> scc)
  dimap g f (TEnum     opts      fromf) = TEnum (second f <$> opts) (fromf . g)
  dimap g f (TArray      sc  tof fromf) = TArray sc (f . tof) (fromf . g)
  dimap g f (TMap        sc  tof fromf) = TMap sc (f . tof) (fromf . g)
  dimap g f (TPrim       n   tof fromf) = TPrim n (fmap f . tof) (fromf . g)
  dimap g f (RecordSchema           sc) = RecordSchema (dimap g f sc)

instance Monoid a => Monoid (TypedSchemaFlexMu v f a) where
  mempty = TEmpty mempty

instance Semigroup (TypedSchemaFlexMu v f a) where
  -- | Allows defining multiple schemas for the same thing, effectively implementing versioning.
  TEmpty a  <> TEmpty _  = TEmpty a
  TEmpty{}  <> x         = x
  x         <> TEmpty{}  = x
  TAllOf aa <> b         = allOf (aa <> [b])
  a         <> TAllOf bb = allOf ([a] <> bb)
  a         <> b         = allOf [a, b]

allOf :: NonEmpty (TypedSchemaFlexMu v from a) -> TypedSchemaFlexMu v from a
allOf [x] = x
allOf x   = TAllOf $ sconcat $ fmap f x   where
  f (TAllOf xx) = xx
  f x           = [x]

-- --------------------------------------------------------------------------------
-- Applicative records

data RecordField v from a where
  RequiredAp :: { fieldName :: Text  -- ^ Name of the field
                , fieldTypedSchema :: TypedSchemaFlexMu v from a
                } -> RecordField v from a
  OptionalAp :: { fieldName :: Text
                , fieldTypedSchema :: TypedSchemaFlexMu v from a
                , fieldDefValue :: a
                } -> RecordField v from a

-- | Lens for the 'fieldName' attribute
fieldNameL :: Lens' (RecordField v from a) Text
fieldNameL f (RequiredAp n sc) = (`RequiredAp` sc) <$> f n
fieldNameL f OptionalAp{..} = (\fieldName -> OptionalAp{..}) <$> f fieldName

instance Profunctor (RecordField v) where
  dimap f g (RequiredAp name sc)     = RequiredAp name (dimap f g sc)
  dimap f g (OptionalAp name sc def) = OptionalAp name (dimap f g sc) (g def)

-- | An 'Alternative' profunctor for defining record schemas with versioning
--
-- @
--  schemaPerson = Person
--             \<$\> (field "name" name \<|\> field "full name" name)
--             \<*\> (field "age" age \<|\> pure -1)
-- @
newtype RecordFieldsMu v from a = RecordFieldsMu {getRecordFields :: Alt (RecordField v from) a}
  deriving newtype (Functor, Applicative, Alternative, Monoid, Semigroup)

newtype RecordFields from a = RecordFields (forall v . Alt (RecordField v from) a)

getRecordFieldsMu :: RecordFields from a -> RecordFieldsMu v from a
getRecordFieldsMu (RecordFields rr) = RecordFieldsMu rr

instance Functor (RecordFields from) where
  fmap f (RecordFields x) = RecordFields (fmap f x)

instance Applicative (RecordFields from) where
  pure x = RecordFields (pure x)
  RecordFields f <*> RecordFields v = RecordFields (f <*> v)

instance Alternative (RecordFields from) where
  empty = RecordFields empty
  RecordFields a <|> RecordFields b = RecordFields (a <|> b)

instance Monoid (RecordFields from a) where mempty = RecordFields mempty

instance Semigroup (RecordFields from a) where
  RecordFields a <> RecordFields b = RecordFields (a <> b)

instance Profunctor (RecordFieldsMu v) where
  dimap f g (RecordFieldsMu rr) = RecordFieldsMu $ hoistAlt (lmap f) $ fmap g rr

-- | Map a function over all the field names
overFieldNames :: (Text -> Text) -> RecordFieldsMu v from a -> RecordFieldsMu v from a
overFieldNames f (RecordFieldsMu rr) = RecordFieldsMu (hoistAlt ((over fieldNameL f)) rr)

-- | Wrap an applicative record schema
record :: RecordFieldsMu v from a -> TypedSchemaFlexMu v from a
record (x) = (RecordSchema x)

-- | @fieldWith sc n get@ introduces a field
fieldWith :: TypedSchemaMu v a -> Text -> (from -> a) -> RecordFieldsMu v from a
fieldWith schema n get = fieldWith' (lmap get schema) n

-- | Generalised version of 'fieldWith'
fieldWith' :: TypedSchemaFlexMu v from a -> Text -> RecordFieldsMu v from a
fieldWith' (schema) n = RecordFieldsMu $ liftAlt (RequiredAp n schema)

-- | Project a schema through a Prism. Returns a partial schema.
--   When encoding/decoding a value that doesn't fit the prism,
--   an optional field will be omitted, and a required field will cause
--   this alternative to be aborted.
liftPrism :: Typeable a => Text -> Prism s t a b -> TypedSchemaFlexMu v a b -> TypedSchemaFlexMu v s t
liftPrism n p sc = withPrism p $ \t f -> rmap t (TTry n sc (either (const Nothing) Just . f))

-- | @liftJust = liftPrism _Just@
liftJust :: Typeable a => TypedSchemaFlexMu v a b -> TypedSchemaFlexMu v (Maybe a) (Maybe b)
liftJust = liftPrism "Just" _Just

-- | @liftRight = liftPrism _Right@
liftRight :: Typeable a => TypedSchemaFlexMu v a b -> TypedSchemaFlexMu v (Either c a) (Either c b)
liftRight = liftPrism "Right" _Right

-- | A generalized version of 'optField'. Does not handle infinite/circular data.
optFieldWith
    :: forall a from v
     . TypedSchemaFlexMu v from (Maybe a)
    -> Text
    -> RecordFieldsMu v from (Maybe a)
optFieldWith (schema) n = RecordFieldsMu $ liftAlt (OptionalAp n schema Nothing)

-- | The most general introduction form for optional alts
optFieldGeneral
    :: forall a from v
     . TypedSchemaFlexMu v from a
    -> Text
    -> a
    -> RecordFieldsMu v from a
optFieldGeneral (schema) n def = RecordFieldsMu $ liftAlt (OptionalAp n schema def)

-- | A generalized version of 'optFieldEither'. Does not handle infinite/circular data
optFieldEitherWith
    :: TypedSchemaFlexMu v from (Either e a) -> Text -> e -> RecordFieldsMu v from (Either e a)
optFieldEitherWith schema n e = optFieldGeneral schema n (Left e)

-- | Extract all the field groups (from alternatives) in the record
extractFields :: RecordFields from a -> [ [(Text, Field)] ]
extractFields = extractFieldsMu . getRecordFieldsMu

extractFields' :: (forall v. RecordFieldsMu v from a) -> [ [(Text, Field)] ]
extractFields' x = extractFieldsMu x

extractFieldsMu :: RecordFieldsMu (NonEmpty Schema) from a -> [ [(Text, Field)] ]
extractFieldsMu = runAlt_ (\x -> (:[]) <$> extractField x) . getRecordFields where

  extractField :: RecordField (NonEmpty Schema) from a -> [(Text, Field)]
  extractField (RequiredAp n sc) = (n,) . (`Field` True) <$> NE.toList (extractSchemaMu sc)
  extractField (OptionalAp n sc _) = (n,) . (`Field` False) <$> NE.toList (extractSchemaMu sc)

newtype NonDet a = NonDet { nonDet :: [a] }
  deriving newtype (Applicative, Alternative, Foldable, Functor, Monad)

instance Traversable NonDet where traverse f (NonDet a) = NonDet <$> traverse f a

extractFieldsHelper :: Alternative f => (forall a . RecordField v from a -> f b) -> RecordFieldsMu v from a -> f [b]
extractFieldsHelper f = runAlt_ (\x -> (:[]) <$> f x) . getRecordFields

-- --------------------------------------------------------------------------------
-- Typed Unions

-- | The schema of discriminated unions
--
-- @
--   import Schemas
--   import "generic-lens" Data.Generics.Labels ()
--   import GHC.Generics
--
--   data Education = Degree Text | PhD Text | NoEducation
--
--   schemaEducation = union'
--     [ alt \"NoEducation\" #_NoEducation
--     , alt \"Degree\"      #_Degree
--     , alt \"PhD\"         #_PhD
--     ]
--   @

-- | Given a non empty set of tagged partial schemas, constructs the schema that applies
--   them in order and selects the first successful match.
union :: (NonEmpty (Text, TypedSchemaMu v a)) -> TypedSchemaMu v a
union args = TOneOf (mk <$> args)
 where
  mk (name, sc) = RecordSchema $ fieldWith' sc name

-- | Existential wrapper for convenient definition of discriminated unions
data UnionTag v from where
  UnionTag :: Typeable b => Text -> Prism' from b -> TypedSchemaMu v b -> UnionTag v from

-- | @altWith name prism schema@ introduces a discriminated union alternative
altWith :: Typeable a => TypedSchemaMu v a -> Text -> Prism' from a -> UnionTag v from
altWith sc n p = UnionTag n p sc

-- | Given a non empty set of constructors, construct the schema that selects the first
--   matching constructor
union' :: NonEmpty (UnionTag v from) -> TypedSchemaMu v from
union' args = union $ args <&> \(UnionTag c p sc) -> (c, liftPrism c p sc)

-- --------------------------------------------------------------------------------
-- Schema extraction from a TypedSchema

-- | Extract an untyped schema that can be serialized
extractSchema :: TypedSchemaFlex from a -> NonEmpty Schema
extractSchema = extractSchemaMu

extractSchemaMu :: TypedSchemaFlexMu (NonEmpty Schema) from a -> NonEmpty Schema
extractSchemaMu (TVar v)         = v
extractSchemaMu (TMu  f)         = fix (extractSchemaMu . f)
extractSchemaMu (TPrim n _ _)    = pure $ Prim n
extractSchemaMu (TTry _ sc _)    = extractSchemaMu sc
extractSchemaMu (TOneOf scc)     = pure $ OneOf $ extractSchemaMu =<< scc
extractSchemaMu (TAllOf scc)     = extractSchemaMu =<< scc
extractSchemaMu TEmpty{}         = pure Empty
extractSchemaMu (TEnum opts  _)  = pure $ Enum (fst <$> opts)
extractSchemaMu (TArray sc _ _)  = Array <$> extractSchemaMu sc
extractSchemaMu (TMap sc _ _)    = StringMap <$> extractSchemaMu sc
extractSchemaMu (RecordSchema rs) = fromList $ foldMap (pure . Record . fromList) (extractFieldsMu rs)

-- | Returns all the primitive validators embedded in this typed schema
extractValidators :: TypedSchemaFlex from a -> Validators
extractValidators = go where
  go :: TypedSchemaFlexMu v from a -> Validators
  go (TPrim n parse _) =
    [ ( n
      , (\x -> case parse x of
          A.Success _ -> Nothing
          A.Error   e -> Just (pack e)
        )
      )
    ]
  go (TOneOf scc) = foldMap go scc
  go (TAllOf scc) = foldMap go scc
  go (TArray sc _ _) = go sc
  go (TMap sc _ _) = go sc
  go (TTry _ sc _) = go sc
  go (RecordSchema rs) = mconcat
    $ mconcat $ nonDet (extractFieldsHelper (pure . go . fieldTypedSchema) rs)
  go _ = []

-- ---------------------------------------------------------------------------------------
-- Encoding to JSON

type E = [(Trace, Mismatch)]

-- | Given a value and its typed schema, produce a JSON record using the 'RecordField's
encodeWith :: (Typeable from, Typeable a) => TypedSchemaFlex from a -> from -> Value
encodeWith sc =
  fromRight (error "Internal error") $ encodeToWith sc (NE.head $ extractSchema sc)

type EncodeInst a = Schema -> Except E (a -> Except E Value)

mapEncodeInst :: (b -> a) -> EncodeInst a -> EncodeInst b
mapEncodeInst f enc sc = fmap (lmap f) (enc sc)

encodeToWith :: (Typeable from) => TypedSchemaFlex from a -> Schema -> Either E (from -> Value)
encodeToWith sc target =
  (\m -> either (throw . AllAlternativesFailed) id . runExcept . m) <$> runExcept (go [] sc target)
 where
  failWith ctx m = throwE [(reverse ctx, m)]

  go :: Typeable from
    => Trace
    -> TypedSchemaFlexMu (EncodeInst Dynamic) from a
    -> Schema
    -> Except E (from -> Except E Value)
  -- go _ sc t | pTraceShow (sc, t) False = undefined
  go _tx (TVar v) sc = mapEncodeInst toDyn v sc
  go ctx (TMu  f) sc = fmap (lmap toDyn) $ fix (mapEncodeInst (fromJust . fromDynamic) . go ctx . f) sc
  go _tx TEmpty{} Array{}     = pure $ pure . const (A.Array [])
  go _tx TEmpty{} Record{}    = pure $ pure . const (A.Object [])
  go _tx TEmpty{} StringMap{} = pure $ pure . const (A.Object [])
  go _tx TEmpty{} OneOf{}     = pure $ pure . const emptyValue
  go ctx (TPrim n _ fromf) (Prim n')
    | n == n'   = pure $ pure . fromf
    | otherwise = failWith ctx (PrimMismatch n n')
  go ctx (TArray sc _ fromf) (Array t) = do
    f <- go ("[]" : ctx) sc t
    return $ A.Array <.> traverse f . fromf
  go ctx (TMap sc _ fromf) (StringMap t) = do
    f <- go ("Map" : ctx) sc t
    return $ A.Object <.> traverse f . fromf
  go ctx (TEnum opts fromf) (Enum optsTarget) = do
    case NE.nonEmpty $ NE.filter (`notElem` optsTarget) (fst <$> opts) of
      Nothing -> pure $ pure . A.String . fromf
      Just xx -> failWith ctx $ MissingEnumChoices xx
  go ctx (TAllOf scc) t = asum $ imap (\i sc -> go (tag i : ctx) sc t) scc
  go ctx (TOneOf scc) t = do
    alts <- itraverse (\i sc -> go (tag i : ctx) sc t) scc
    return $ \x -> asum $ fmap ($ x) alts
  go ctx sc              (OneOf tt) = asum $ fmap (go ctx sc) tt
  go ctx (TTry n sc try) t          = do
    f <- go (n : ctx) sc t
    return $ \x -> f =<< maybe (failWith ctx (TryFailed n)) pure (try x)
  go ctx (RecordSchema rec) (Record target) = do
    let alternatives = nonDet $ runAlt_ extractField (getRecordFields rec)
    let targetFields = Set.fromList (Map.keys target)
    let complete = filter ((targetFields ==) . Set.fromList . fmap fst) alternatives
    case complete of
      [] -> failWith ctx NoMatches
      alts -> pure $ \x -> asum $ fmap
        (\alt ->
          A.Object
            .   fromList
            .   (mapMaybe (sequenceOf _2))
            <$> traverse (\(fn, f) -> (fn, ) <$> f x) alt
        )
        alts
   where
    extractField :: Typeable from => RecordField (EncodeInst Dynamic) from a -> NonDet [(Text, from -> Except E (Maybe Value))]
    extractField RequiredAp {..} =
      case Map.lookup fieldName target of
        Nothing -> return []
        Just targetField -> do
          case runExcept $ go (fieldName : ctx) fieldTypedSchema (fieldSchema targetField) of
            Left _ -> empty
            Right f -> do
              let decoder x = Just <$> f x `catchE` \mm -> failWith ctx (InvalidRecordField fieldName mm)
              return [(fieldName, decoder)]

    extractField OptionalAp {..} =
      case Map.lookup fieldName target of
        Nothing -> return []
        Just targetField -> do
          guard $ not (isRequired targetField)
          case runExcept $ go (fieldName : ctx) fieldTypedSchema (fieldSchema targetField) of
            Left _ -> empty
            Right f -> do
              let decoder x = (Just <$> f x) `catchE` \_ -> pure Nothing
              return [(fieldName, decoder)]
  go ctx sc (Array t) = do
    f <- go ctx sc t
    return $ A.Array . fromList . (: []) <.> f
  go _tx _     Empty = pure $ pure . const emptyValue
  go ctx other src   = failWith ctx (SchemaMismatch {-(NE.head $ extractSchema other)-} src src)

-- --------------------------------------------------------------------------
-- Decoding

type D = [(Trace, DecodeError)]

data DecodeError
  = DecodeError Mismatch
  | UnusedFields [[Text]]
  | TriedAndFailed
  deriving (Eq, Show)

-- | Runs a schema as a function @enc -> dec@. Loops for infinite/circular data
runSchema :: TypedSchemaFlex enc dec -> enc -> Either [DecodeError] dec
runSchema sc = runExcept . go sc
    where
        go :: forall from a v. TypedSchemaFlexMu v from a -> from -> Except [DecodeError] a
        go (TEmpty a       ) _    = pure a
        go (TTry _ sc try) from = maybe (throwE [TriedAndFailed]) (go sc) (try from)
        go (TPrim n toF fromF) from = case toF (fromF from) of
            A.Success a -> pure a
            A.Error   e -> failWith (PrimError n (pack e))
        go (TEnum opts fromF) from = case lookup enumValue opts of
            Just x  -> pure x
            Nothing -> failWith $ InvalidEnumValue enumValue (fst <$> opts)
            where enumValue = fromF from
        go (TMap   _sc toF fromF) from = pure $ toF (fromF from)
        go (TArray _sc toF fromF) from = pure $ toF (fromF from)
        go (TAllOf scc          ) from = msum $ (`go` from) <$> scc
        go (TOneOf scc          ) from = msum $ (`go` from) <$> scc
        go (RecordSchema alts   ) from = runAlt f (getRecordFields alts)
            where
                f :: RecordField v from b -> Except [DecodeError] b
                f RequiredAp{..} = go fieldTypedSchema from
                f OptionalAp{..} = go fieldTypedSchema from

        failWith e = throwE [DecodeError e]

-- | Given a JSON 'Value' and a typed schema, extract a Haskell value
decodeWith :: TypedSchemaFlex from a -> Value -> Either D a
decodeWith sc v = decoder >>= ($ v)
 where
   decoder = decodeFromWith sc (NE.head $ extractSchema sc)

decodeFromWith :: TypedSchemaFlex from a -> Schema -> Either D (Value -> Either D a)
-- TODO merge runSchema and decodeFromWith ?
decodeFromWith sc source = (runExcept .) <$> runExcept (go [] sc source)
  where
    failWith ctx e = throwE [(reverse ctx, DecodeError e)]

    go :: Trace -> TypedSchemaFlexMu v from a -> Schema -> Except D (Value -> Except D a)
    go _tx (TEmpty a) _ = pure $ const $ pure a
    go ctx (TEnum optsTarget _) s@(Enum optsSource) =
      case NE.nonEmpty $ NE.filter (`notElem` map fst (NE.toList optsTarget)) (optsSource) of
        Just xx -> failWith ctx $ MissingEnumChoices xx
        Nothing -> pure $ \case
          A.String x -> maybe (failWith ctx (InvalidEnumValue x (fst <$> optsTarget))) pure $ lookup x optsTarget
          other -> failWith ctx (ValueMismatch s other)
    go ctx (TArray sc tof _) s@(Array src) = do
      f <- go ("[]" : ctx) sc src
      return $ \case
        A.Array x -> tof <$> traverse f x
        other -> failWith ctx (ValueMismatch s other)
    go ctx (TMap sc tof _) s@(StringMap src) = do
      f <- go ("Map" : ctx) sc src
      return $ \case
        A.Object x -> tof <$> traverse f x
        other -> failWith ctx (ValueMismatch s other)
    go ctx (TPrim n tof _) (Prim src)
      | n /= src = failWith ctx (PrimMismatch n src)
      | otherwise = return $ \x -> case tof x of
          A.Error   e -> failWith ctx (PrimError n (pack e))
          A.Success a -> return a
    go ctx (TTry n sc _try) x = go (n : ctx) sc x
    go ctx (TAllOf scc) src = do
      let parsers = mapMaybe (\sc -> either (const Nothing) Just $ runExcept $ go ctx sc src) (NE.toList scc)
      return $ \x -> asum (map ($x) parsers)
    go ctx (TOneOf scc) src = do
      let parsers = mapMaybe (\sc -> either (const Nothing) Just $ runExcept $ go ctx sc src) (NE.toList scc)
      return $ \x -> asum (map ($x) parsers)
    go ctx (RecordSchema (RecordFieldsMu rec)) (Record src) = do
      let solutions = nonDet $ coerce $ runAlt f' rec
      -- make sure there are no unused fields
          sourceFields = Map.keys src
          valid = map (\(tgtFields, f) -> case Set.difference (fromList sourceFields) (fromList tgtFields) of
                          [] -> Right f
                          xx -> Left (Set.toList xx)
                      ) solutions
      case partitionEithers valid of
        (ee, []) -> throwE [(reverse ctx, UnusedFields ee)]
        (_ , alts) -> pure $ \x -> asum $ fmap ($ x) alts
     where
          f' :: RecordField v from a -> (NonDet `Compose` (,) [Text] `Compose` (->) Value `Compose` (Except D)) a
          f' x = coerce (f x)
          f :: RecordField v from a -> NonDet ([Text], Value -> Except D a) -- ReaderT Value (Except D) a
          f RequiredAp{..} =
            case Map.lookup fieldName src of
              Nothing -> empty
              Just srcField -> do
                guard $ isRequired srcField
                case runExcept $ go (fieldName:ctx) fieldTypedSchema (fieldSchema srcField) of
                  Left  _ -> empty
                  Right f -> return ([fieldName], \case
                    A.Object o -> case Map.lookup fieldName o of
                      Nothing -> failWith (fieldName:ctx) (MissingRecordField fieldName)
                      Just v  -> f v)
          f OptionalAp{..} =
            case Map.lookup fieldName src of
              Nothing -> return ([fieldName], const $ return fieldDefValue)
              Just srcField -> do
                case runExcept $ go (fieldName:ctx) fieldTypedSchema (fieldSchema srcField) of
                  Left  _ -> empty
                  Right f -> return ([fieldName], \case
                    A.Object o -> case Map.lookup fieldName o of
                      Nothing -> return fieldDefValue
                      Just v  -> f v)

    go ctx s     (OneOf xx) = asum $ fmap (go ctx s) xx
    go ctx s            src = failWith ctx (SchemaMismatch {-(NE.head $ extractSchemaMu s)-} src src)


-- ----------------------------------------------
-- Utils

runAlt_ :: (Alternative g, Monoid m) => (forall a. f a -> g m) -> Alt f b -> g m
runAlt_ f = fmap getConst . getCompose . runAlt (Compose . fmap Const . f)

(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
f <.> g = fmap f . g

infixr 8 <.>

