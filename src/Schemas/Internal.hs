{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
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
import           Data.Either
import           Data.Foldable              (asum)
import           Data.Functor.Compose
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import qualified Data.HashSet               as Set
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NE
import           Data.Maybe
import           Data.Semigroup
import           Data.Text                  (Text, pack, unpack)
import           Data.Tuple
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           GHC.Exts                   (IsList (..))
import           Prelude                    hiding (lookup)
import           Schemas.Untyped

-- Typed schemas
-- --------------------------------------------------------------------------------

-- | @TypedSchemaFlex enc dec@ is a schema for encoding to @enc@ and decoding to @dec@.
--   Usually we want @enc@ and @dec@ to be the same type but this flexibility comes in handy
--   for composition.
--
--   * introduction forms: 'record', 'enum', 'schema'
--   * operations: 'encodeToWith', 'decodeFrom', 'extractSchema'
--   * composition: 'dimap', 'union', 'stringMap', 'liftPrism'
--
data TypedSchemaFlex from a where
  TEnum   :: (NonEmpty (Text, a)) -> (from -> Text) -> TypedSchemaFlex from a
  TArray :: TypedSchema b -> (Vector b -> a) -> (from -> Vector b) -> TypedSchemaFlex from a
  TMap   :: TypedSchema b -> (HashMap Text b -> a) -> (from -> HashMap Text b) -> TypedSchemaFlex from a
  -- | Encoding and decoding support all alternatives
  TAllOf :: NonEmpty (TypedSchemaFlex from a) -> TypedSchemaFlex from a
  -- | Decoding from all alternatives, but encoding only to one
  TOneOf :: NonEmpty (TypedSchemaFlex from a) -> TypedSchemaFlex from a
  TEmpty :: a -> TypedSchemaFlex from a
  TPrim  :: Text -> (Value -> A.Result a) -> (from -> Value) -> TypedSchemaFlex from a
  -- TTry _ is used to implement 'optField' on top of 'optFieldWith'
  -- it could be exposed to provide some form of error handling, but currently is not
  TTry     :: Text -> TypedSchemaFlex a b -> (a' -> Maybe a) -> TypedSchemaFlex a' b
  RecordSchema :: RecordFields from a -> TypedSchemaFlex from a

instance Show (TypedSchemaFlex from a) where
  show (TTry n sc _) = unwords ["TTry", unpack n, show sc]
  show x = show $ extractSchema x

-- | @enum values mapping@ construct a schema for a non empty set of values with a 'Text' mapping
enum :: Eq a => (a -> Text) -> (NonEmpty a) -> TypedSchema a
enum showF opts = TEnum alts (fromMaybe (error "invalid alt") . flip lookup altMap)
 where
  altMap = fmap swap $ alts --TODO fast lookup
  alts   = opts <&> \x -> (showF x, x)

-- | @stringMap sc@ is the schema for a stringmap where the values have schema @sc@
stringMap :: TypedSchema a -> TypedSchema (HashMap Text a)
stringMap sc = TMap sc id id

-- | @list sc@ is the schema for a list of values with schema @sc@
list :: IsList l => TypedSchema (Item l) -> TypedSchema l
list schema = TArray schema (fromList . V.toList) (V.fromList . toList)

-- | @vector sc@ is the schema for a vector of values with schema @sc@
vector :: TypedSchema a -> TypedSchema (Vector a)
vector sc = TArray sc id id

-- | @viaJson label@ constructs a schema reusing existing 'aeson' instances. The resulting schema
--  is opaque and cannot be subtyped and/or versioned, so this constructor should be used sparingly.
--  The @label@ is used to describe the extracted 'Schema'.
viaJSON :: (A.FromJSON a, A.ToJSON a) => Text -> TypedSchema a
viaJSON n = TPrim n A.fromJSON A.toJSON

-- | Apply an isomorphism to a schema
viaIso :: Iso' a b -> TypedSchema a -> TypedSchema b
viaIso iso sc = withIso iso $ \from to -> dimap to from sc

-- | The schema of String values
string :: TypedSchema String
string = viaJSON "String"

-- | A schema for types that can be parsed and pretty-printed. The resulting schema is opaque and cannot
-- be subtyped/versioned, so this constructor is best used for primitive value
readShow :: (Read a, Show a) => TypedSchema a
readShow = dimap show read string

allOf :: NonEmpty (TypedSchemaFlex from a) -> TypedSchemaFlex from a
allOf [x] = x
allOf x = TAllOf $ sconcat $ fmap f x where
  f (TAllOf xx) = xx
  f x = [x]

-- | The schema of undiscriminated unions. Prefer using 'union' where possible
oneOf :: NonEmpty (TypedSchemaFlex from a) -> TypedSchemaFlex from a
oneOf [x] = x
oneOf x = TOneOf $ sconcat $ fmap f x where
  f (TOneOf xx) = xx
  f x = [x]

instance Functor (TypedSchemaFlex from) where
  fmap = rmap

instance Profunctor TypedSchemaFlex where
    dimap _ f (TEmpty a                 ) = TEmpty (f a)
    dimap g f (TTry n       sc       try) = TTry n (rmap f sc) (try . g)
    dimap g f (TAllOf      scc          ) = TAllOf (dimap g f <$> scc)
    dimap g f (TOneOf      scc          ) = TOneOf (dimap g f <$> scc)
    dimap g f (TEnum     opts      fromf) = TEnum (second f <$> opts) (fromf . g)
    dimap g f (TArray      sc  tof fromf) = TArray sc (f . tof) (fromf . g)
    dimap g f (TMap        sc  tof fromf) = TMap sc (f . tof) (fromf . g)
    dimap g f (TPrim       n   tof fromf) = TPrim n (fmap f . tof) (fromf . g)
    dimap g f (RecordSchema sc) = RecordSchema (dimap g f sc)

instance Monoid a => Monoid (TypedSchemaFlex f a) where
  mempty = TEmpty mempty

instance Semigroup (TypedSchemaFlex f a) where
  -- | Allows defining multiple schemas for the same thing, effectively implementing versioning.
  TEmpty a <> TEmpty _ = TEmpty a
  TEmpty{} <> x = x
  x <> TEmpty{} = x
  TAllOf aa <> b = allOf (aa <> [b])
  a <> TAllOf bb = allOf ([a] <> bb)
  a <> b = allOf [a,b]

type TypedSchema a = TypedSchemaFlex a a

-- --------------------------------------------------------------------------------
-- Applicative records

data RecordField from a where
  RequiredAp :: { fieldName :: Text  -- ^ Name of the field
                , fieldTypedSchema :: TypedSchemaFlex from a
                } -> RecordField from a
  OptionalAp :: { fieldName :: Text
                , fieldTypedSchema :: TypedSchemaFlex from a
                , fieldDefValue :: a
                } -> RecordField from a

-- | Lens for the 'fieldName' attribute
fieldNameL :: Lens' (RecordField from a) Text
fieldNameL f (RequiredAp n sc) = (`RequiredAp` sc) <$> f n
fieldNameL f OptionalAp{..} = (\fieldName -> OptionalAp{..}) <$> f fieldName

instance Profunctor RecordField where
  dimap f g (RequiredAp name sc)     = RequiredAp name (dimap f g sc)
  dimap f g (OptionalAp name sc def) = OptionalAp name (dimap f g sc) (g def)

-- | An 'Alternative' profunctor for defining record schemas with versioning
--
-- @
--  schemaPerson = Person
--             \<$\> (field "name" name \<|\> field "full name" name)
--             \<*\> (field "age" age \<|\> pure -1)
-- @
newtype RecordFields from a = RecordFields {getRecordFields :: Alt (RecordField from) a}
  deriving newtype (Alternative, Applicative, Functor, Monoid, Semigroup)

instance Profunctor RecordFields where
  dimap f g = RecordFields . hoistAlt (lmap f) . fmap g . getRecordFields

-- | Map a function over all the field names
overFieldNames :: (Text -> Text) -> RecordFields from a -> RecordFields from a
overFieldNames f = RecordFields . hoistAlt ((over fieldNameL f)) . getRecordFields

-- | Wrap an applicative record schema
record :: RecordFields from a -> TypedSchemaFlex from a
record = RecordSchema

-- | @fieldWith sc n get@ introduces a field
fieldWith :: TypedSchema a -> Text -> (from -> a) -> RecordFields from a
fieldWith schema n get = fieldWith' (lmap get schema) n

-- | Generalised version of 'fieldWith'
fieldWith' :: TypedSchemaFlex from a -> Text -> RecordFields from a
fieldWith' schema n = RecordFields $ liftAlt (RequiredAp n schema)

-- | Project a schema through a Prism. Returns a partial schema.
--   When encoding/decoding a value that doesn't fit the prism,
--   an optional field will be omitted, and a required field will cause
--   this alternative to be aborted.
liftPrism :: Text -> Prism s t a b -> TypedSchemaFlex a b -> TypedSchemaFlex s t
liftPrism n p sc = withPrism p $ \t f -> rmap t (TTry n sc (either (const Nothing) Just . f))

-- | @liftJust = liftPrism _Just@
liftJust :: TypedSchemaFlex a b -> TypedSchemaFlex (Maybe a) (Maybe b)
liftJust = liftPrism "Just" _Just

-- | @liftRight = liftPrism _Right@
liftRight :: TypedSchemaFlex a b -> TypedSchemaFlex (Either c a) (Either c b)
liftRight = liftPrism "Right" _Right

-- | A generalized version of 'optField'. Does not handle infinite/circular data.
optFieldWith
    :: forall a from
     . TypedSchemaFlex from (Maybe a)
    -> Text
    -> RecordFields from (Maybe a)
optFieldWith schema n = RecordFields $ liftAlt (OptionalAp n schema Nothing)

-- | The most general introduction form for optional alts
optFieldGeneral
    :: forall a from
     . TypedSchemaFlex from a
    -> Text
    -> a
    -> RecordFields from a
optFieldGeneral schema n def = RecordFields $ liftAlt (OptionalAp n schema def)

-- | A generalized version of 'optFieldEither'. Does not handle infinite/circular data
optFieldEitherWith
    :: TypedSchemaFlex from (Either e a) -> Text -> e -> RecordFields from (Either e a)
optFieldEitherWith schema n e = optFieldGeneral schema n (Left e)

-- | Extract all the field groups (from alternatives) in the record
extractFields :: RecordFields from a -> NonDet [(Text, Field)]
extractFields = extractFieldsHelper extractField
  where
    extractField :: RecordField from a -> (Text, Field)
    extractField (RequiredAp n sc) = (n,) . (`Field` True) $ extractSchema sc
    extractField (OptionalAp n sc _) = (n,) . (`Field` False) $ extractSchema sc

newtype NonDet a = NonDet { nonDet :: [a] }
  deriving newtype (Applicative, Alternative, Foldable, Functor, Monad)

instance Traversable NonDet where traverse f (NonDet a) = NonDet <$> traverse f a

extractFieldsHelper :: (forall a . RecordField from a -> b) -> RecordFields from a -> NonDet [b]
extractFieldsHelper f = runAlt_ (\x -> pure [f x]) . getRecordFields

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
union :: (NonEmpty (Text, TypedSchema a)) -> TypedSchema a
union args = TOneOf (mk <$> args)
 where
  mk (name, sc) = RecordSchema $ fieldWith' sc name

-- | Existential wrapper for convenient definition of discriminated unions
data UnionTag from where
  UnionTag :: Text -> Prism' from b -> TypedSchema b -> UnionTag from

-- | @altWith name prism schema@ introduces a discriminated union alternative
altWith :: TypedSchema a -> Text -> Prism' from a -> UnionTag from
altWith sc n p = UnionTag n p sc

-- | Given a non empty set of constructors, construct the schema that selects the first
--   matching constructor
union' :: (NonEmpty (UnionTag from)) -> TypedSchema from
union' args = union $ args <&> \(UnionTag c p sc) -> (c, liftPrism c p sc)

-- --------------------------------------------------------------------------------
-- Schema extraction from a TypedSchema

-- | Extract an untyped schema that can be serialized
extractSchema :: TypedSchemaFlex from a -> Schema
extractSchema (TPrim n _ _) = Prim n
extractSchema (TTry _ sc _)      = extractSchema sc
extractSchema (TOneOf scc)     = OneOf $ extractSchema <$> scc
extractSchema (TAllOf scc)     = AllOf $ extractSchema <$> scc
extractSchema TEmpty{}         = Empty
extractSchema (TEnum opts  _)  = Enum (fst <$> opts)
extractSchema (TArray sc _ _)  = Array $ extractSchema sc
extractSchema (TMap sc _ _)    = StringMap $ extractSchema sc
extractSchema (RecordSchema rs) = foldMap (Record . fromList) $ extractFields rs

-- | Returns all the primitive validators embedded in this typed schema
extractValidators :: TypedSchemaFlex from a -> Validators
extractValidators (TPrim n parse _) =
  [ ( n
    , (\x -> case parse x of
        A.Success _ -> Nothing
        A.Error   e -> Just (pack e)
      )
    )
  ]
extractValidators (TOneOf scc) = foldMap extractValidators scc
extractValidators (TAllOf scc) = foldMap extractValidators scc
extractValidators (TArray sc _ _) = extractValidators sc
extractValidators (TMap sc _ _) = extractValidators sc
extractValidators (TTry _ sc _) = extractValidators sc
extractValidators (RecordSchema rs) = mconcat
  $ mconcat $ nonDet (extractFieldsHelper (extractValidators . fieldTypedSchema) rs)
extractValidators _ = []

-- ---------------------------------------------------------------------------------------
-- Encoding to JSON

type E = [(Trace, Mismatch)]

-- | Given a value and its typed schema, produce a JSON record using the 'RecordField's
encodeWith :: TypedSchemaFlex from a -> from -> Value
  -- TODO Better error messages to help debug partial schemas ?
encodeWith sc = either (throw . AllAlternativesFailed . map ([],)) id . runExcept . go sc where
  go :: TypedSchemaFlex from a -> from -> Except [Mismatch] Value
  go (TAllOf scc       ) x = encodeAlternatives <$> traverse (`go` x) scc
  go (TOneOf scc       ) x = asum (fmap (`go` x) scc)
  go (TTry n sc try    ) x = go sc =<< maybe (throwE [TryFailed n]) pure (try x)
  go (TEnum _ fromf    ) b = pure $ A.String (fromf b)
  go (TPrim _ _ fromf  ) b = pure $ fromf b
  go (TEmpty _         ) _ = pure emptyValue
  go (TArray sc _ fromf) b = A.Array <$> go sc `traverse` fromf b
  go (TMap   sc _ fromf) b = A.Object <$> go sc `traverse` fromf b
  go (RecordSchema rec ) x = do
    let alternatives = extractFieldsHelper (extractField x) rec
    let results =
          partitionEithers $ nonDet $ fmap (runExcept . sequenceA) alternatives
    case results of
      (_, NE.nonEmpty -> Just alts') -> pure $ encodeAlternatives $ fmap
        (A.Object . fromList . catMaybes)
        alts'
      (ee, _) -> throwE (concat ee)
   where

    extractField b RequiredAp {..} =
      Just . (fieldName, ) <$> go fieldTypedSchema b `catchE` \mm ->
        throwE [InvalidRecordField fieldName (map ([],) mm)]
    extractField b OptionalAp {..} =
      (Just . (fieldName, ) <$> go fieldTypedSchema b)
        `catchE` \_ -> pure Nothing

encodeToWith :: TypedSchemaFlex from a -> Schema -> Either E (from -> Value)
encodeToWith sc target =
  (\m -> either (throw . AllAlternativesFailed) id . runExcept . m)
    <$> runExcept (go [] sc target)
 where
  failWith ctx m = throwE [(reverse ctx, m)]

  go
    :: Trace
    -> TypedSchemaFlex from a
    -> Schema
    -- Returns a set of mismatches or an encoding function that can fail (TTry)
    -> Except E (from -> Except E Value)
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
  go ctx sc              (AllOf tt) = do
    alts <- itraverse (\i -> go (tag i : ctx) sc) tt
    return $ \x -> encodeAlternatives <$> traverse ($x) alts
  go ctx (TAllOf scc) t = asum $ imap (\i sc -> go (tag i : ctx) sc t) scc
  go ctx (TOneOf scc) t = do
    alts <- itraverse (\i sc -> go (tag i : ctx) sc t) scc
    return $ \x -> asum $ fmap ($ x) alts
  go ctx sc              (OneOf tt) = asum $ fmap (go ctx sc) tt
  go ctx (TTry n sc try) t          = do
    f <- go (n : ctx) sc t
    return $ \x -> f =<< maybe (failWith ctx (TryFailed n)) pure (try x)
  go ctx (RecordSchema rec) (Record ff) = do
    let alternatives =
          [ runExcept $ sequenceOf (traverse . _2) alt
          | altMb <- nonDet $ extractFieldsHelper
            (\f -> (fieldName f, ) <$> extractField f)
            rec
          , let alt = catMaybes altMb
          , Set.fromList (Map.keys ff) == Set.fromList (fmap fst alt)
          ]
    case partitionEithers alternatives of
      (_, NE.nonEmpty -> Just alts) -> pure $ \x -> asum $ fmap
        (\alt ->
          A.Object
            .   fromList
            .   (mapMaybe (sequenceOf _2))
            <$> traverse (\(fn, f) -> (fn, ) <$> f x) alt
        )
        alts
      ([], _) -> failWith ctx NoMatches
      (ee, _) -> failWith ctx $ AllAlternativesFailed (concat ee)
   where
    extractField
      :: RecordField from a -> Maybe (Except E (from -> Except E (Maybe Value)))
    extractField RequiredAp {..} | Just f <- Map.lookup fieldName ff = Just $ do
      f <- go (fieldName : ctx) fieldTypedSchema (fieldSchema f)
      return $ \x -> Just <$> f x `catchE` \mm ->
        failWith ctx (InvalidRecordField fieldName mm)
    extractField OptionalAp {..} | Just f <- Map.lookup fieldName ff = Just $ do
      f <- go (fieldName : ctx) fieldTypedSchema (fieldSchema f)
      return $ \x -> (Just <$> f x) `catchE` \_ -> pure Nothing
    extractField _ = Nothing
  go ctx sc (Array t) = do
    f <- go ctx sc t
    return $ A.Array . fromList . (: []) <.> f
  go _tx _        Empty       = pure $ pure . const emptyValue
  go ctx other tgt = failWith ctx (SchemaMismatch (extractSchema other) tgt)


-- --------------------------------------------------------------------------
-- Decoding

data DecodeError
  = VE Mismatch
  | TriedAndFailed
  deriving (Eq, Show)

-- | Runs a schema as a function @enc -> dec@. Loops for infinite/circular data
runSchema :: TypedSchemaFlex enc dec -> enc -> Either [DecodeError] dec
runSchema sc = runExcept . go sc
    where
        go :: forall from a. TypedSchemaFlex from a -> from -> Except [DecodeError] a
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
        go (RecordSchema alts ) from = runAlt f (getRecordFields alts)
            where
                f :: RecordField from b -> Except [DecodeError] b
                f RequiredAp{..} = go fieldTypedSchema from
                f OptionalAp{..} = go fieldTypedSchema from

        failWith e = throwE [VE e]

-- | Given a JSON 'Value' and a typed schema, extract a Haskell value
decodeWith :: TypedSchemaFlex from a -> Value -> Either [(Trace, DecodeError)] a
-- TODO merge runSchema and decodeWith ?
-- TODO change decode type to reflect partiality due to TTry _?
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
      (\(A.Object alts, _encodedPath) ->
          getCompose $ runAlt (Compose . (: []) . f alts) (getRecordFields rec)
      )
      alts
   where
    f :: A.Object -> RecordField from a -> Except [(Trace, DecodeError)] a
    f alts (RequiredAp n sc) = case Map.lookup n alts of
      Just v  -> go (n : ctx) sc v
      Nothing -> case sc of
--         TArray _ tof' _ -> pure $ tof' []
        _               -> failWith ctx (MissingRecordField n)
    f alts OptionalAp {..} = case Map.lookup fieldName alts of
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

  go ctx (TPrim n tof _) x = case tof x of
    A.Error   e -> failWith ctx (PrimError n (pack e))
    A.Success a -> pure a
  go ctx (TTry _ sc _try) x = go ctx sc x
  go ctx s              x = failWith ctx (ValueMismatch (extractSchema s) x)

decodeFromWith :: TypedSchema a -> Schema -> Maybe (Value -> Either [(Trace, DecodeError)] a)
decodeFromWith sc source = case isSubtypeOf (extractValidators sc) source (extractSchema sc) of
  Right cast -> Just $ decodeWith sc . cast
  _          -> Nothing

-- ----------------------------------------------
-- Utils

runAlt_ :: (Alternative g, Monoid m) => (forall a. f a -> g m) -> Alt f b -> g m
runAlt_ f = fmap getConst . getCompose . runAlt (Compose . fmap Const . f)

(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
f <.> g = fmap f . g

infixr 8 <.>

