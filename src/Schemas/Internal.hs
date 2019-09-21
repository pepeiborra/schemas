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
import           Control.Lens               hiding (Empty, enum, (<.>))
import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.Aeson                 (Value)
import qualified Data.Aeson                 as A
import           Data.Biapplicative
import           Data.Either
import           Data.Foldable              (asum)
import           Data.Functor.Compose
import           Data.Generics.Labels       ()
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NE
import           Data.Maybe
import           Data.Text                  (Text, pack)
import           Data.Tuple
import           Data.Typeable              (Typeable)
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           GHC.Exts                   (IsList (..))
import           Prelude                    hiding (lookup)
import           Schemas.Untyped

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
readShow = dimap show read viaJSON

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

fieldWith :: TypedSchema a -> Text -> (from -> a) -> RecordFields from a
fieldWith schema n get = fieldWith' (lmap get schema) n

fieldWith' :: TypedSchemaFlex from a -> Text -> RecordFields from a
fieldWith' schema n = RecordFields $ liftAlt (RequiredAp n schema)

data TryFailed = TryFailed
 deriving (Exception, Show, Typeable)

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

altWith :: TypedSchema a -> Text -> Prism' from a -> UnionTag from
altWith sc n p = UnionTag n p sc

union' :: (NonEmpty (UnionTag from)) -> TypedSchema from
union' args = union $ args <&> \(UnionTag c p sc) -> (c, liftPrism p sc)

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

-- ---------------------------------------------------------------------------------------
-- Encoding to JSON

-- | Given a value and its typed schema, produce a JSON record using the 'RecordField's
encodeWith :: TypedSchemaFlex from a -> from -> Value
  -- TODO Better error messages to help debug partial schemas ?
encodeWith sc = either (throw . head) id . runExcept . go sc where
  go :: TypedSchemaFlex from a -> from -> Except [SomeException] Value
  go (TAllOf scc) x = encodeAlternatives <$> traverse (`go` x) scc
  go (TOneOf scc) x = asum (fmap (`go` x) scc)
  go (TTry   sc  try   ) x = go sc =<< maybe (throwE [toException TryFailed]) pure (try x)
  go (TEnum  _   fromf ) b = pure $ A.String (fromf b)
  go (TPrim  _   fromf ) b = pure $ fromf b
  go (TEmpty _         ) _ = pure emptyValue
  go (TArray sc _ fromf) b = A.Array <$> go sc `traverse` fromf b
  go (TMap   sc _ fromf) b = A.Object <$> go sc `traverse` fromf b
  go (RecordSchema rec ) x = do
    fields' <- traverse((`catchE` \_ -> pure Nothing) . fmap Just . sequence) fields
    case NE.nonEmpty (catMaybes fields') of
      Nothing -> throwE [] -- NOTE test
      Just fields' -> pure $ encodeAlternatives $ fmap (A.Object . fromList . catMaybes) fields'
   where
    fields = extractFieldsHelper (extractField x) rec

    extractField b RequiredAp {..} =  Just . (fieldName,) <$> go fieldTypedSchema b
    extractField b OptionalAp {..} = (Just . (fieldName,) <$> go fieldTypedSchema b) `catchE` \_ -> pure Nothing

encodeToWith :: TypedSchema a -> Schema -> Maybe (a -> Value)
encodeToWith sc target = case extractSchema sc `isSubtypeOf` target of
  Just cast -> Just $ cast . encodeWith sc
  Nothing   -> Nothing

-- --------------------------------------------------------------------------
-- Decoding

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

decodeFromWith :: TypedSchema a -> Schema -> Maybe (Value -> Either [(Trace, DecodeError)] a)
decodeFromWith sc source = case source `isSubtypeOf` extractSchema sc of
  Just cast -> Just $ decodeWith sc . cast
  Nothing   -> Nothing

-- ----------------------------------------------
-- Utils

runAlt_ :: (Alternative g, Monoid m) => (forall a. f a -> g m) -> Alt f b -> g m
runAlt_ f = fmap getConst . getCompose . runAlt (Compose . fmap Const . f)

(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
f <.> g = fmap f . g

infixr 9 <.>

