{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS -Wno-name-shadowing    #-}
module Schemas.Internal where

import           Control.Alternative.Free
import           Control.Applicative        (Alternative (..))
import           Control.Exception
import           Control.Lens               hiding (Empty, allOf, enum, (<.>))
import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.Aeson                 (Value)
import qualified Data.Aeson                 as A
import           Data.Biapplicative
import           Data.Coerce
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
import           Data.Text                  (Text, pack)
import           Data.Tuple
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           Data.Void
import           GHC.Exts                   (IsList (..))
import           Prelude                    hiding (lookup)
import           Schemas.Untyped

import           Unsafe.Coerce

-- Typed schemas
-- --------------------------------------------------------------------------------

-- | @TypedSchemaFlex enc dec@ is a schema for encoding from @enc@ and decoding to @dec@.
--   Usually we want @enc@ and @dec@ to be the same type but this flexibility comes in handy
--   for composition.
--
--   * introduction forms: 'record', 'enum', 'schema'
--   * operations: 'encodeToWith', 'decodeFrom', 'extractSchema'
--   * composition: 'dimap', 'union', 'stringMap', 'liftPrism'
--
data TypedSchemaFlex from a where
  TNamed ::SchemaName -> TypedSchemaFlex from' a' -> (a' -> a) -> (from -> from') -> TypedSchemaFlex from a
  TEnum  ::(NonEmpty (Text, a)) -> (from -> Text) -> TypedSchemaFlex from a
  TArray ::TypedSchemaFlex b b -> (Vector b -> a) -> (from -> Vector b) -> TypedSchemaFlex from a
  TMap   ::TypedSchemaFlex b b -> (HashMap Text b -> a) -> (from -> HashMap Text b) -> TypedSchemaFlex from a
  -- | Encoding and decoding support all alternatives
  TAllOf ::NonEmpty (TypedSchemaFlex from a) -> TypedSchemaFlex from a
  -- | Decoding from all alternatives, but encoding only to one
  TOneOf ::NonEmpty (TypedSchemaFlex from a) -> TypedSchemaFlex from a
  TEmpty ::a -> TypedSchemaFlex from a
  TPrim  ::Text -> (Value -> A.Result a) -> (from -> Value) -> TypedSchemaFlex from a
  -- TTry _ is used to implement 'optField' on top of 'optFieldWith'
  -- It's also crucial for implementing unions on top of TOneOf
  -- it could be exposed to provide some form of error handling, but currently is not
  TTry     ::Text -> TypedSchemaFlex a b -> (a' -> Maybe a) -> TypedSchemaFlex a' b
  RecordSchema ::RecordFields from a -> TypedSchemaFlex from a

instance Show (TypedSchemaFlex from a) where
  show = show . NE.head . extractSchema

type TypedSchema a = TypedSchemaFlex a a


-- | @named n sc@ annotates a schema with a name, allowing for circular schemas.
named :: SchemaName -> TypedSchemaFlex from' a -> TypedSchemaFlex from' a
named n sc = TNamed n sc id id

-- | @enum values mapping@ construct a schema for a non empty set of values with a 'Text' mapping
enum :: Eq a => (a -> Text) -> (NonEmpty a) -> TypedSchema a
enum showF opts = TEnum
  alts
  (fromMaybe (error "invalid alt") . flip lookup altMap)
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

-- | The schema of undiscriminated unions. Prefer using 'union' where possible
oneOf :: NonEmpty (TypedSchemaFlex from a) -> TypedSchemaFlex from a
oneOf [x] = x
oneOf x   = TOneOf $ sconcat $ fmap f x where
  f (TOneOf xx) = xx
  f (x        ) = [x]

instance Functor (TypedSchemaFlex from) where
  fmap = rmap

instance Profunctor (TypedSchemaFlex) where
  dimap g f (TNamed n sc tof fromf) = TNamed n sc (f . tof) (fromf . g)
  dimap _ f (TEmpty a             ) = TEmpty (f a)
  dimap g f (TTry n sc try        ) = TTry n (rmap f sc) (try . g)
  dimap g f (TAllOf scc           ) = TAllOf (dimap g f <$> scc)
  dimap g f (TOneOf scc           ) = TOneOf (dimap g f <$> scc)
  dimap g f (TEnum opts fromf     ) = TEnum (second f <$> opts) (fromf . g)
  dimap g f (TArray sc tof fromf  ) = TArray sc (f . tof) (fromf . g)
  dimap g f (TMap   sc tof fromf  ) = TMap sc (f . tof) (fromf . g)
  dimap g f (TPrim  n  tof fromf  ) = TPrim n (fmap f . tof) (fromf . g)
  dimap g f (RecordSchema sc      ) = RecordSchema (dimap g f sc)

instance Monoid a => Monoid (TypedSchemaFlex f a) where
  mempty = TEmpty mempty

instance Semigroup (TypedSchemaFlex f a) where
  -- | Allows defining multiple schemas for the same thing, effectively implementing versioning.
  TEmpty a  <> TEmpty _  = TEmpty a
  TEmpty{}  <> x         = x
  x         <> TEmpty{}  = x
  TAllOf aa <> b         = allOf (aa <> [b])
  a         <> TAllOf bb = allOf ([a] <> bb)
  a         <> b         = allOf [a, b]

  sconcat = allOf

allOf :: NonEmpty (TypedSchemaFlex from a) -> TypedSchemaFlex from a
allOf [x] = x
allOf x   = TAllOf $ sconcat $ fmap f x where
  f (TAllOf xx) = xx
  f x           = [x]

-- --------------------------------------------------------------------------------
-- Applicative records

data RecordField from a where
  RequiredAp ::{ fieldName :: Text  -- ^ Name of the field
                , fieldTypedSchema :: TypedSchemaFlex from a
                } -> RecordField from a
  OptionalAp ::{ fieldName :: Text
                , fieldTypedSchema :: TypedSchemaFlex from a
                , fieldDefValue :: a
                } -> RecordField from a

-- | Lens for the 'fieldName' attribute
fieldNameL :: Lens' (RecordField from a) Text
fieldNameL f (RequiredAp n sc) = (`RequiredAp` sc) <$> f n
fieldNameL f OptionalAp {..} =
  (\fieldName -> OptionalAp { .. }) <$> f fieldName

instance Profunctor RecordField where
  dimap f g (RequiredAp name sc    ) = RequiredAp name (dimap f g sc)
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
overFieldNames f =
  RecordFields . hoistAlt ((over fieldNameL f)) . getRecordFields

-- | Wrap an applicative record schema
record :: RecordFields from a -> TypedSchemaFlex from a
record = RecordSchema

-- | @fieldWith sc n get@ introduces a field
fieldWith :: TypedSchema a -> Text -> (from -> a) -> RecordFields from a
fieldWith schema n get = fieldWith' (lmap get schema) n

-- | Generalised version of 'fieldWith'
fieldWith' :: TypedSchemaFlex from a -> Text -> RecordFields from a
fieldWith' (schema) n = RecordFields $ liftAlt (RequiredAp n schema)

-- | Project a schema through a Prism. Returns a partial schema.
--   When encoding/decoding a value that doesn't fit the prism,
--   an optional field will be omitted, and a required field will cause
--   this alternative to be aborted.
liftPrism :: Text -> Prism s t a b -> TypedSchemaFlex a b -> TypedSchemaFlex s t
liftPrism n p sc =
  withPrism p $ \t f -> rmap t (TTry n sc (either (const Nothing) Just . f))

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
  :: forall a from . TypedSchemaFlex from a -> Text -> a -> RecordFields from a
optFieldGeneral schema n def = RecordFields $ liftAlt (OptionalAp n schema def)

-- | A generalized version of 'optFieldEither'. Does not handle infinite/circular data
optFieldEitherWith
  :: TypedSchemaFlex from (Either e a)
  -> Text
  -> e
  -> RecordFields from (Either e a)
optFieldEitherWith schema n e = optFieldGeneral schema n (Left e)

extractFieldsHelper
  :: Alternative f
  => (forall a . RecordField from a -> f b)
  -> RecordFields from a
  -> f [b]
extractFieldsHelper f = runAlt_ (\x -> (: []) <$> f x) . getRecordFields

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
union args = oneOf (mk <$> args)
  where mk (name, sc) = RecordSchema $ fieldWith' sc name

-- | Existential wrapper for convenient definition of discriminated unions
data UnionTag from where
  UnionTag ::Text -> Prism' from b -> TypedSchema b -> UnionTag from

-- | @altWith name prism schema@ introduces a discriminated union alternative
altWith :: TypedSchema a -> Text -> Prism' from a -> UnionTag from
altWith sc n p = UnionTag n p sc

-- | Given a non empty set of constructors, construct the schema that selects the first
--   matching constructor
union' :: (NonEmpty (UnionTag from)) -> TypedSchema from
union' args = union $ args <&> \(UnionTag c p sc) -> (c, liftPrism c p sc)

-- --------------------------------------------------------------------------------
-- Schema extraction from a TypedSchema

-- | Extract an untyped schema that can be serialized.
--
--   For schemas with alternatives, this enumerates all the possible
--   versions lazily.
--   Beware when using on schemas with multiple alternatives,
--   as the number of versions is exponential.
extractSchema :: TypedSchemaFlex from a -> NonEmpty Schema
extractSchema (TNamed n sc _ _) = Named n <$> extractSchema sc
extractSchema (TPrim n _  _   ) = pure $ Prim n
extractSchema (TTry  _ sc _   ) = extractSchema sc
extractSchema (TOneOf scc     ) = pure $ OneOf $ extractSchema =<< scc
extractSchema (TAllOf scc     ) = extractSchema =<< scc
extractSchema (TEmpty{}       ) = pure $ Empty
extractSchema (TEnum opts _   ) = pure $ Enum (fst <$> opts)
extractSchema (TArray sc _ _  ) = Array <$> extractSchema sc
extractSchema (TMap   sc _ _  ) = StringMap <$> extractSchema sc
extractSchema (RecordSchema rs) =
  fromList $ foldMap (\x -> pure (Record (fromList x))) (extractFields rs)

-- | Extract all the field groups (from alternatives) in the record
extractFields :: RecordFields from to -> [[(Text, Field)]]
extractFields =
  runAlt_ (\x -> (: []) <$> NE.toList (extractField x)) . getRecordFields where

  extractField :: RecordField from to -> NonEmpty (Text, Field)
  extractField (RequiredAp n sc) =
    (\s -> (n, (`Field` True) s)) <$> extractSchema sc
  extractField (OptionalAp n sc _) =
    (\s -> (n, (`Field` False) s)) <$> extractSchema sc


-- | Returns all the primitive validators embedded in this typed schema
extractValidators :: TypedSchemaFlex from a -> Validators
extractValidators = go where
  go :: TypedSchemaFlex from a -> Validators
  go (TPrim n parse _) =
    [ ( n
      , (\x -> case parse x of
          A.Success _ -> Nothing
          A.Error   e -> Just (pack e)
        )
      )
    ]
  go (TOneOf scc    ) = foldMap go scc
  go (TAllOf scc    ) = foldMap go scc
  go (TArray sc _  _) = go sc
  go (TMap   sc _  _) = go sc
  go (TTry   _  sc _) = go sc
  go (RecordSchema rs) =
    mconcat $ mconcat (extractFieldsHelper (pure . go . fieldTypedSchema) rs)
  go _ = []

-- ---------------------------------------------------------------------------------------
-- Encoding to JSON

type E = [(Trace, Mismatch)]

-- | Given a typed schema, produce a JSON encoder to the firt version returned by 'extractSchema'
encodeWith :: TypedSchemaFlex from a -> from -> Value
encodeWith sc = fromRight (error "Internal error")
  $ encodeToWith sc (NE.head $ extractSchema sc)

-- | Given source and target schemas, produce a JSON encoder
encodeToWith :: TypedSchemaFlex from a -> Schema -> Either E (from -> Value)
encodeToWith sc target =
  (\m -> either (throw . AllAlternativesFailed) id . runExcept . m)
    <$> runExcept (go [] [] sc (target))
 where
  failWith ctx m = throwE [(reverse ctx, m)]

  go
    :: forall from a
     . [(SchemaName, Except E (Void -> Except E Value))]
    -> Trace
    -> TypedSchemaFlex from a
    -> Schema
    -> Except E (from -> Except E Value)
  go env ctx (TNamed n sct _ fromf) (Named n' sc) | n == n' =
    case lookup n env of
      Just res -> do
        -- TODO understand why this delay is necessary
        return $ unsafeDelay $ lmap (unsafeCoerce . fromf) <$> res
      Nothing ->
        let res    = go ((n, resDyn) : env) ctx sct sc
            resDyn = lmap unsafeCoerce <$> res
        in  lmap fromf <$> res
  go _ _tx TEmpty{} Array{}     = pure $ pure . const (A.Array [])
  go _ _tx TEmpty{} Record{}    = pure $ pure . const (A.Object [])
  go _ _tx TEmpty{} StringMap{} = pure $ pure . const (A.Object [])
  go _ _tx TEmpty{} OneOf{}     = pure $ pure . const emptyValue
  go _ ctx (TPrim n _ fromf) (Prim n')
    | n == n'   = pure $ pure . fromf
    | otherwise = failWith ctx (PrimMismatch n n')
  go i ctx (TArray sc _ fromf) (Array t) = do
    f <- go i ("[]" : ctx) sc t
    return $ A.Array <.> traverse f . fromf
  go i ctx (TMap sc _ fromf) (StringMap t) = do
    f <- go i ("Map" : ctx) sc t
    return $ A.Object <.> traverse f . fromf
  go _ ctx (TEnum opts fromf) (Enum optsTarget) = do
    case NE.nonEmpty $ NE.filter (`notElem` optsTarget) (fst <$> opts) of
      Nothing -> pure $ pure . A.String . fromf
      Just xx -> failWith ctx $ MissingEnumChoices xx
  go n ctx (TAllOf scc) t = asum $ imap (\i sc -> go n (tag i : ctx) sc t) scc
  go n ctx (TOneOf scc) t = do
    alts <- itraverse (\i sc -> go n (tag i : ctx) sc t) scc
    return $ \x -> asum $ fmap ($ x) alts
  go i ctx sc              (OneOf tt) = asum $ fmap (go i ctx sc) tt
  go i ctx (TTry n sc try) t          = do
    f <- go i (n : ctx) sc t
    return $ \x -> f =<< maybe (failWith ctx (TryFailed n)) pure (try x)
  go i ctx (RecordSchema rec) (Record target) = do
    let alternatives = runAlt_ extractField (getRecordFields rec)
    let targetFields = Set.fromList (Map.keys target)
    let complete =
          filter ((targetFields ==) . Set.fromList . fmap fst) alternatives
    case complete of
      []   -> failWith ctx NoMatches
      alts -> pure $ \x -> asum $ fmap
        (\alt ->
          A.Object
            .   fromList
            .   (mapMaybe (sequenceOf _2))
            <$> traverse (\(fn, f) -> (fn, ) <$> f x) alt
        )
        alts
   where
    extractField
      :: forall from a
       . RecordField from a
      -> [[(Text, from -> Except E (Maybe Value))]]
    extractField RequiredAp {..} = case Map.lookup fieldName target of
      Nothing          -> return []
      Just targetField -> do
        case
            runExcept $ go i
                           (fieldName : ctx)
                           fieldTypedSchema
                           (fieldSchema targetField)
          of
            Left  _ -> empty
            Right f -> do
              let decoder x = Just <$> f x `catchE` \mm ->
                    failWith ctx (InvalidRecordField fieldName mm)
              return [(fieldName, decoder)]

    extractField OptionalAp {..} = case Map.lookup fieldName target of
      Nothing          -> return []
      Just targetField -> do
        guard $ not (isRequired targetField)
        case
            runExcept $ go i
                           (fieldName : ctx)
                           fieldTypedSchema
                           (fieldSchema targetField)
          of
            Left  _ -> empty
            Right f -> do
              let decoder x = (Just <$> f x) `catchE` \_ -> pure Nothing
              return [(fieldName, decoder)]
  go i ctx sc (Array t) = do
    f <- go i ctx sc t
    return $ A.Array . fromList . (: []) <.> f
  go _ _tx _ Empty = pure $ pure . const emptyValue
  go _ ctx other src =
    failWith ctx (SchemaMismatch (NE.head $ extractSchema other) src)

-- --------------------------------------------------------------------------
-- Decoding

type D = [(Trace, DecodeError)]

type DecodeError = Mismatch

-- | Runs a schema as a function @enc -> dec@. Loops for infinite/circular data
runSchema :: TypedSchemaFlex enc dec -> enc -> Either [DecodeError] dec
runSchema sc = runExcept . go sc
 where
  go :: forall from a . TypedSchemaFlex from a -> from -> Except [DecodeError] a
  go (TEmpty a             ) _    = pure a
  go (TNamed _ sc tof fromF) a    = tof <$> go sc (fromF a)
  go (TTry n sc try) from = maybe (throwE [TryFailed n]) (go sc) (try from)
  go (TPrim n toF fromF    ) from = case toF (fromF from) of
    A.Success a -> pure a
    A.Error   e -> failWith (PrimError n (pack e))
  go (TEnum opts fromF) from = case lookup enumValue opts of
    Just x  -> pure x
    Nothing -> failWith $ InvalidEnumValue enumValue (fst <$> opts)
    where enumValue = fromF from
  go (TMap   _sc toF fromF) from = pure $ toF (fromF from)
  go (TArray _sc toF fromF) from = pure $ toF (fromF from)
  go (TAllOf       scc    ) from = msum $ (`go` from) <$> scc
  go (TOneOf       scc    ) from = msum $ (`go` from) <$> scc
  go (RecordSchema alts   ) from = runAlt f (getRecordFields alts)
   where
    f :: RecordField from b -> Except [DecodeError] b
    f RequiredAp {..} = go fieldTypedSchema from
    f OptionalAp {..} = go fieldTypedSchema from

  failWith e = throwE [e]

-- | Given a JSON 'Value' and a typed schema, extract a Haskell value
decodeWith :: TypedSchemaFlex from a -> Value -> Either D a
decodeWith sc v = decoder >>= ($ v)
  where decoder = decodeFromWith sc (NE.head $ extractSchema sc)

decodeFromWith
  :: TypedSchemaFlex from a -> Schema -> Either D (Value -> Either D a)
-- TODO merge runSchema and decodeFromWith ?
decodeFromWith sc source = (runExcept .) <$> runExcept (go [] [] sc (source))
 where
  failWith ctx e = throwE [(reverse ctx, e)]

  go
    :: [(SchemaName, Except D (Value -> Except D Void))]
    -> Trace
    -> TypedSchemaFlex from a
    -> Schema
    -> Except D (Value -> Except D a)
  go _nv _tx (TEmpty a) _                               = pure $ const $ pure a
  go env ctx (TNamed n sc tof _) (Named n' s) | n == n' = case lookup n env of
    Just sol -> do
      -- TODO understand why this delay is necessary
      return $ unsafeDelay $ (fmap . fmap . fmap) (tof . unsafeCoerce) sol
    Nothing ->
      let sol    = go ((n, solDyn) : env) ctx sc s
          solDyn = (fmap . fmap . fmap) unsafeCoerce sol
      in  (fmap . fmap . fmap) tof sol
  go env ctx (TNamed _ sc tof _) s = (fmap . fmap . fmap) tof $ go env ctx sc s
  go _nv ctx (TEnum optsTarget _) s@(Enum optsSource) =
    case
        NE.nonEmpty
          $ NE.filter (`notElem` map fst (NE.toList optsTarget)) (optsSource)
      of
        Just xx -> failWith ctx $ MissingEnumChoices xx
        Nothing -> pure $ \case
          A.String x ->
            maybe (failWith ctx (InvalidEnumValue x (fst <$> optsTarget))) pure
              $ lookup x optsTarget
          other -> failWith ctx (ValueMismatch s other)
  go env ctx (TArray sc tof _) s@(Array src) = do
    f <- go env ("[]" : ctx) sc src
    return $ \case
      A.Array x -> tof <$> traverse f x
      other     -> failWith ctx (ValueMismatch s other)
  go env ctx (TMap sc tof _) s@(StringMap src) = do
    f <- go env ("Map" : ctx) sc src
    return $ \case
      A.Object x -> tof <$> traverse f x
      other      -> failWith ctx (ValueMismatch s other)
  go _nv ctx (TPrim n tof _) (Prim src)
    | n /= src = failWith ctx (PrimMismatch n src)
    | otherwise = return $ \x -> case tof x of
      A.Error   e -> failWith ctx (PrimError n (pack e))
      A.Success a -> return a
  go env ctx (TTry n sc _try) x   = go env (n : ctx) sc x
  go env ctx (TAllOf scc    ) src = do
    let parsers = map (\sc -> runExcept $ go env ctx sc src) (NE.toList scc)
    case partitionEithers parsers of
      (ee, []) -> failWith ctx (AllAlternativesFailed (concat ee))
      (_ , pp) -> return $ \x -> asum (map ($ x) pp)
  go env ctx (TOneOf scc) src = do
    let parsers = map (\sc -> runExcept $ go env ctx sc src) (NE.toList scc)
    case partitionEithers parsers of
      (ee, []) -> failWith ctx (AllAlternativesFailed (concat ee))
      (_ , pp) -> return $ \x -> asum (map ($ x) pp)
  go env ctx (RecordSchema (RecordFields rec)) (Record src) = do
    let solutions    = coerce $ runAlt f' rec
    -- make sure there are no unused fields
        sourceFields = Map.keys src
        valid        = map
          (\(tgtFields, f) ->
            case Set.difference (fromList sourceFields) (fromList tgtFields) of
              [] -> Right f
              xx -> Left (Set.toList xx)
          )
          solutions
    case partitionEithers valid of
      (ee, []  ) -> throwE [(reverse ctx, UnusedFields ee)]
      (_ , alts) -> pure $ \x -> asum $ fmap ($ x) alts
   where
    f'
      :: RecordField from a
      -> ([] `Compose` (,) [Text] `Compose` (->) Value `Compose` (Except D))
           a
    f' x = coerce (f x)
    f :: RecordField from a -> [([Text], Value -> Except D a)]
    f RequiredAp {..} = case Map.lookup fieldName src of
      Nothing       -> empty
      Just srcField -> do
        guard $ isRequired srcField
        case
            runExcept
              $ go env (fieldName : ctx) fieldTypedSchema (fieldSchema srcField)
          of
            Left  _ -> empty
            Right f -> return
              ( [fieldName]
              , \case
                A.Object o -> case Map.lookup fieldName o of
                  Nothing ->
                    failWith (fieldName : ctx) (MissingRecordField fieldName)
                  Just v -> f v
              )
    f OptionalAp {..} = case Map.lookup fieldName src of
      Nothing       -> return ([fieldName], const $ return fieldDefValue)
      Just srcField -> do
        case
            runExcept
              $ go env (fieldName : ctx) fieldTypedSchema (fieldSchema srcField)
          of
            Left  _ -> empty
            Right f -> return
              ( [fieldName]
              , \case
                A.Object o -> case Map.lookup fieldName o of
                  Nothing -> return fieldDefValue
                  Just v  -> f v
              )

  go env ctx s  (OneOf xx ) = asum $ fmap (go env ctx s) xx
  go env ctx sc (Named _ s) = go env ctx sc s
  go _nv ctx s src =
    failWith ctx (SchemaMismatch (NE.head $ extractSchema s) src)


-- ----------------------------------------------
-- Utils

runAlt_
  :: (Alternative g, Monoid m) => (forall a . f a -> g m) -> Alt f b -> g m
runAlt_ f = fmap getConst . getCompose . runAlt (Compose . fmap Const . f)

(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
f <.> g = fmap f . g

infixr 8 <.>


unsafeDelay :: Except a c -> c
unsafeDelay = fromRight (error "internal error") . runExcept
