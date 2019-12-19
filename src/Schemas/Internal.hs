{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS -Wno-name-shadowing         #-}

module Schemas.Internal where

import           Control.Alternative.Free
import           Control.Applicative        (Alternative (..), optional)
import           Control.Lens               hiding (Empty, allOf, enum, (<.>))
import           Control.Monad.Except
import           Control.Monad.Trans.Iter
import           Control.Monad.State
import           Data.Aeson                 (Value)
import qualified Data.Aeson                 as A
import           Data.Biapplicative
import           Data.Bitraversable
import           Data.Coerce
import           Data.Either
import           Data.Foldable              (asum)
import           Data.Functor.Compose
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import qualified Data.HashSet               as Set
import           Data.List                  (find)
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
import           Numeric.Natural
import           Prelude                    hiding (lookup)
import           Schemas.Attempt            as Attempt
import           Schemas.Untyped            as U

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
  TOneOf :: TypedSchemaFlex from' a'
         -> TypedSchemaFlex from'' a''
         -> (Either a' a'' -> a)
         -> (from -> Either from' from'')
         -> TypedSchemaFlex from a
  TPure  :: a -> TypedSchemaFlex from a
  TEmpty :: (Void -> a) -> (from -> Void) -> TypedSchemaFlex from a
  TPrim  :: Text -> (Value -> A.Result a) -> (from -> Value) -> TypedSchemaFlex from a
  RecordSchema ::RecordFields from a -> TypedSchemaFlex from a

instance Show (TypedSchemaFlex from a) where
  show = show . NE.head . extractSchema

instance Functor (TypedSchemaFlex from) where
  fmap = rmap

instance Profunctor TypedSchemaFlex where
  dimap g f (TEmpty tof fromf)      = TEmpty (f . tof) (fromf . g)
  dimap _ f (TPure a)               = TPure (f a)
  dimap g f (TNamed n sc tof fromf) = TNamed n sc (f . tof) (fromf . g)
  dimap g f (TAllOf scc           ) = TAllOf (dimap g f <$> scc)
  dimap g f (TOneOf sca scb to fr ) = TOneOf sca scb (f . to) (fr . g)
  dimap g f (TEnum opts fromf     ) = TEnum (second f <$> opts) (fromf . g)
  dimap g f (TArray sc tof fromf  ) = TArray sc (f . tof) (fromf . g)
  dimap g f (TMap   sc tof fromf  ) = TMap sc (f . tof) (fromf . g)
  dimap g f (TPrim  n  tof fromf  ) = TPrim n (fmap f . tof) (fromf . g)
  dimap g f (RecordSchema sc      ) = RecordSchema (dimap g f sc)

instance Monoid (TypedSchemaFlex Void Void) where
  mempty = emptySchema

instance Semigroup (TypedSchemaFlex f a) where
  -- | Allows defining multiple schemas for the same thing, effectively implementing versioning.
  -- a <> b | pTraceShow ("Semigroup TypedSchema", a,b) False= undefined
  x         <> TEmpty{}  = x
  TEmpty{}  <> x         = x
  TAllOf aa <> b         = allOf (aa <> [b])
  a         <> TAllOf bb = allOf ([a] <> bb)
  a         <> b         = allOf [a, b]

  sconcat = allOf

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

-- | 'eitherSchema' and 'emptySchema' make 'TypedSchemaFlex' an almost instance of 'SumProfunctor' (no 'Choice')
eitherSchema
  :: TypedSchemaFlex from a
  -> TypedSchemaFlex from' a'
  -> TypedSchemaFlex (Either from from') (Either a a')
eitherSchema sc sc' = TOneOf sc sc' id id

-- | The vacuous schema
emptySchema :: TypedSchema Void
emptySchema = TEmpty id id

-- | The schema that can be decoded but not encoded
pureSchema :: a -> TypedSchemaFlex from a
pureSchema = TPure

allOf :: NonEmpty (TypedSchemaFlex from a) -> TypedSchemaFlex from a
allOf x   = allOf' $ sconcat $ fmap f x where
  f (TAllOf xx) = NE.toList xx
  f TEmpty{}    = []
  f x           = [x]
  allOf' []  = error "empty allOf"
  allOf' [x] = x
  allOf' x = TAllOf $ NE.fromList x

-- --------------------------------------------------------------------------------
-- Applicative records

data RecordField from a where
  RequiredAp ::{ fieldName :: Text  -- ^ Name of the field
                , fieldTypedSchema :: TypedSchemaFlex from a
                } -> RecordField from a
  OptionalAp ::{ fieldName :: Text
               , fieldTypedSchema :: TypedSchemaFlex from a
               , fieldDefaultValue :: a
               } -> RecordField from a

-- | Lens for the 'fieldName' attribute
fieldNameL :: Lens' (RecordField from a) Text
fieldNameL f (RequiredAp n sc) = (`RequiredAp` sc) <$> f n
fieldNameL f OptionalAp {..} =
  (\fieldName -> OptionalAp { .. }) <$> f fieldName

instance Profunctor RecordField where
  dimap f g (RequiredAp name sc) = RequiredAp name (dimap f g sc)
  dimap f g (OptionalAp name sc v) = OptionalAp name (dimap f g sc) (g v)

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

-- | Project a schema through a Prism.
liftPrism :: Prism s t a b -> TypedSchemaFlex a b -> TypedSchemaFlex t t -> TypedSchemaFlex s t
liftPrism p sc otherwise = withPrism p $ \t f -> TOneOf otherwise sc (either id t) f

-- | Returns a partial schema.
--   When encoding/decoding a Nothing value,
--   an optional field will be omitted, and a required field will cause
--   this alternative to be aborted.
liftJust :: TypedSchemaFlex a b -> TypedSchemaFlex (Maybe a) (Maybe b)
liftJust sc = liftPrism _Just sc $ TEmpty absurd $ error "liftJust"

-- | Returns a partial schema.
--   When encoding/decoding a Left value,
--   an optional field will be omitted, and a required field will cause
--   this alternative to be aborted.
liftRight :: TypedSchemaFlex a b -> TypedSchemaFlex (Either c a) (Either c b)
liftRight sc = liftPrism _Right sc $ TEmpty absurd $ error "liftRight"

optFieldWith
  :: forall a from
   . TypedSchemaFlex from (Maybe a)
  -> Text
  -> RecordFields from (Maybe a)
optFieldWith = optFieldGeneral Nothing

-- | The most general introduction form for optional alts
optFieldGeneral
  :: forall a from . a ->TypedSchemaFlex from a -> Text -> RecordFields from a
optFieldGeneral def schema n = RecordFields $ liftAlt (OptionalAp n schema def)

-- | A generalized version of 'optFieldEither'.
optFieldEitherWith
  :: TypedSchemaFlex from (Either e a)
  -> Text
  -> e
  -> RecordFields from (Either e a)
optFieldEitherWith sc n e = optFieldGeneral (Left e) sc n

extractFieldsHelper
  :: Alternative f
  => (forall a . RecordField from a -> f b)
  -> RecordFields from a
  -> f [b]
extractFieldsHelper f = runAlt_ (\x -> (: []) <$> f x) . getRecordFields

-- --------------------------------------------------------------------------------
-- Typed Unions


-- | An alternative in a union type
data UnionAlt from where
  UnionAlt :: Prism' from b -> TypedSchema b -> UnionAlt from

-- | Declare an alternative in a union type
altWith :: TypedSchema a -> Prism' from a -> UnionAlt from
altWith sc p = UnionAlt p sc

-- | Discriminated unions that record the name of the chosen constructor in the schema
-- @
--   data Education = Degree Text | PhD Text | NoEducation
--
--   schemaEducation = union'
--     [ (\"NoEducation\", alt #_NoEducation)
--     , (\"Degree\"     , alt #_Degree)
--     , (\"PhD\"        , alt #_PhD)
--     ]
--   @
union :: (NonEmpty ((Text,UnionAlt from))) -> TypedSchema from
union (a :| rest) = go (a:rest) where
  go ((n, UnionAlt p sc) : rest) = liftPrism p (RecordSchema $ fieldWith' sc n) $ go rest
  go [] = TEmpty absurd (error "incomplete union definition")

-- | Undiscriminated union that do not record the name of the constructor in the schema
--   data Education = Degree Text | PhD Text | NoEducation
--
--   schemaEducation = oneOf
--     [ alt #_NoEducation
--     , alt #_Degree
--     , alt #_PhD
--     ]
--   @
oneOf :: (NonEmpty (UnionAlt from)) -> TypedSchema from
oneOf (a :| rest) = go (a:rest) where
  go (UnionAlt p sc : rest) = liftPrism p sc $ go rest
  go [] = TEmpty absurd (error "incomplete oneOf definition")

-- --------------------------------------------------------------------------------
-- Schema extraction from a TypedSchema

-- | Extract an untyped schema that can be serialized.
--
--   For schemas with alternatives, this enumerates all the possible
--   versions lazily.
--   Beware when using on schemas with multiple alternatives,
--   as the number of versions is exponential.
extractSchema :: TypedSchemaFlex from a -> NonEmpty Schema
-- extractSchema xx | pTraceShow ("extractSchema") False = undefined
extractSchema TPure{}          = pure Unit
extractSchema (TNamed n sc _ _) = Named n <$> extractSchema sc
extractSchema (TPrim n _  _   ) = pure $ Prim n
extractSchema (TOneOf s s' _ _) = (<>) <$> extractSchema s <*> extractSchema s'
extractSchema (TAllOf scc     ) = extractSchema =<< scc
extractSchema (TEnum opts _   ) = pure $ Enum (fst <$> opts)
extractSchema (TArray sc _ _  ) = Array     <$> extractSchema sc
extractSchema (TMap   sc _ _  ) = StringMap <$> extractSchema sc
extractSchema (RecordSchema rs) =
  case foldMap (\x -> pure (Record (fromList x))) (extractFields rs) of
    [] -> pure Empty
    other -> fromList other
extractSchema TEmpty{} = pure Empty

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
  go (TOneOf a b _ _) = go a <> go b
  go (TAllOf scc    ) = foldMap go scc
  go (TArray sc _  _) = go sc
  go (TMap   sc _  _) = go sc
  go (RecordSchema rs) =
    mconcat $ mconcat (extractFieldsHelper (pure . go . fieldTypedSchema) rs)
  go _ = []

-- ---------------------------------------------------------------------------------------
-- Results

type TracedMismatches = [(Trace, Mismatch)]

newtype IterAltT m a = IterAlt {runIterAlt :: IterT m a}
  deriving newtype (Applicative, Functor, Monad, MonadError e, MonadState s, MonadTrans, MonadFree Identity, Eq, Show)

instance (MonadPlus m) => Alternative (IterAltT m) where
  empty = IterAlt (lift empty)
  IterAlt (IterT ma) <|> IterAlt (IterT mb) = IterAlt $ IterT $
    do a <- optional ma
       case a of
         Nothing -> mb
         Just (Left done) -> pure (Left done)
         Just (Right more_a) -> do
           b <- optional mb
           case b of
             Nothing -> pure $ Right more_a
             Just (Left done) -> pure (Left done)
             Just (Right more_b) -> pure $ Right (more_a <|> more_b)


runDelay :: Monad m => Natural -> IterAltT m a -> m (Maybe a)
runDelay n = retract . cutoff (fromIntegral n) . runIterAlt

-- | A monad encapsulating failure as well as non-termination
newtype Result a = Result { getResult :: IterAltT (Attempt TracedMismatches) a}
  deriving newtype (Applicative, Alternative, Functor, Monad, MonadError TracedMismatches, MonadFree Identity, Eq, Show)

liftAttempt :: Attempt TracedMismatches a -> Result a
liftAttempt  = Result . lift

-- | Run a 'Result' up with bounded depth. Returns nothing if it runs out of steps.
runResult :: MonadError TracedMismatches g => Natural -> Result a -> g (Maybe a)
runResult maxSteps = execAttempt . runDelay maxSteps . getResult

-- ---------------------------------------------------------------------------------------
-- Encoding to JSON

type Partial = IterT Maybe

-- | Given a typed schema, produce a JSON encoder to the first version produced by 'extractSchema'
encodeWith :: TypedSchemaFlex from a -> (from -> Value)
encodeWith sc = ensureSuccess encoder
  where
    encoder = encodeToWith sc (NE.head $ extractSchema sc)
    ensureSuccess = either (error.show) id

-- | Given source and target schemas, produce a JSON encoder
encodeToWith :: TypedSchemaFlex from a -> Schema -> Either TracedMismatches (from -> Value)
encodeToWith sc target = runAttempt $
  (fmap.fmap) (fromMaybe (error "Empty schema")) $
  (go [] [] sc (target))
 where
  failWith ctx m = throwError [(reverse ctx, m)]

  go
    :: forall from a
     . [(SchemaName, Attempt TracedMismatches (Void -> Maybe Value))]
    -> Trace
    -> TypedSchemaFlex from a
    -> Schema
    -> Attempt TracedMismatches (from -> Maybe Value)
  -- go _ _ sc s | pTraceShow ("encode", sc, s) False = undefined
  go env ctx (TNamed n sct _ fromf) (Named n' sc) | n == n' =
    case lookup n env of
      Just res ->
        lmap (unsafeCoerce . fromf) <$> res
      Nothing ->
        let res    = go ((n, resDynLater) : env) ctx sct sc
            resDyn = lmap unsafeCoerce <$> res
            resDynLater = (pure . fromMaybe (error "impossible") . attemptSuccess) resDyn
        in  lmap fromf <$> res
  go _ _   _       Empty       = pure $ pure . const emptyValue
  go _ _tx TPure{} Array{}     = pure $ pure . const (A.Array [])
  go _ _tx TPure{} StringMap{} = pure $ pure . const (emptyValue)
  go _ _tx (TEmpty _ _)     _  = pure $ const empty
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
  go n ctx (TOneOf a b _ fromf) t = do
    encoderA <- go n ("L" : ctx) a t
    encoderB <- go n ("R" : ctx) b t
    pure $ \x -> either encoderA encoderB (fromf x)
  go i ctx sc                 (OneOf tt) = asum $ fmap (go i ctx sc) (tt <> [Empty])
  go i ctx (RecordSchema rec) (Record target) = do
    let candidates = runAlt_ extractField (getRecordFields rec)
    case find (\candidate -> Set.fromList (map fst candidate) == targetFields) candidates of
          Nothing  -> failWith ctx $
                 SchemaMismatch (NE.head $ extractSchema $ RecordSchema rec) (Record target)
          Just solution -> pure $ \x -> do
            fields <- traverse (\(_,f) -> case f x of Nothing -> Nothing ; Just (n,y) -> Just $ (n,) <$> y) solution
            return $ A.object $ catMaybes fields
   where
    targetFields = Set.fromList (Map.keys target)

    liftGo   = (either (const empty) pure . runAttempt)

    extractField
      :: forall from a
       . RecordField from a
      -> [] [(Text, from -> Maybe (Text, Maybe Value))]
    extractField RequiredAp {..} = case Map.lookup fieldName target of
      Nothing          -> pure []
      Just targetField -> do
        f <- liftGo $
             go i (fieldName : ctx)
                  fieldTypedSchema
                  (fieldSchema targetField)
        return $
          let encoder = fmap ((fieldName,) . Just) . f
          in [(fieldName, encoder)]

    extractField OptionalAp {..} = case Map.lookup fieldName target of
      Nothing          -> pure []
      Just targetField -> do
        guard $ not (isRequired targetField)
        f <- liftGo $ go i (fieldName : ctx)
                           fieldTypedSchema
                           (fieldSchema targetField)

        return $
          let encoder = Just . (fieldName,) . f
          in [(fieldName, encoder)]
  go i ctx sc (Array t) = do
    f <- go i ctx sc t
    return $ A.Array . fromList . (: []) <.> f
  go _ _tx _ Unit    = pure $ const (pure emptyValue)
  -- go _ _ other src | pTraceShow ("mismatch", other, src) False = undefined
  go _ ctx other src =
    failWith ctx (SchemaMismatch (NE.head $ extractSchema other) src)

-- --------------------------------------------------------------------------
-- Decoding

-- | Runs a schema as a function @enc -> dec@. Loops for infinite/circular data
runSchema :: TypedSchemaFlex enc dec -> enc -> Either [Mismatch] dec
runSchema sc = runExcept . go sc
 where
  go :: forall from a . TypedSchemaFlex from a -> from -> Except [Mismatch] a
  go (TPure              x) _    = pure x
  go (TEmpty toF fromF    ) x    = pure $ toF $ fromF x
  -- TODO handle circular data
  go (TNamed _ sc tof fromF) a    = tof <$> go sc (fromF a)
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
  go (TOneOf sc sc' toF fF) from = toF <$> bitraverse (go sc) (go sc') (fF from)
  go (RecordSchema alts   ) from = runAlt f (getRecordFields alts)
   where
    f :: RecordField from b -> Except [Mismatch] b
    f RequiredAp {..} = go fieldTypedSchema from
    f OptionalAp {..} = go fieldTypedSchema from

  failWith e = throwError [e]

-- | Evaluates a schema as a value of type 'dec'. Can only succeed if the schema contains a 'TPure' alternative
evalSchema :: forall enc dec . TypedSchemaFlex enc dec -> Maybe dec
evalSchema (TPure x             ) = pure x
evalSchema  TEmpty{}              = Nothing
-- TODO handle circular data
evalSchema (TNamed _ sc tof _) = tof <$> evalSchema sc
evalSchema (TPrim _ _ _) = empty
evalSchema (TEnum _ _) = empty
evalSchema (TMap   _sc _ _) = empty
evalSchema (TArray _sc _ _) = empty
evalSchema (TAllOf scc           ) = msum $ evalSchema <$> scc
evalSchema (TOneOf sc sc' toF _) =
  toF <$> ((Left <$> evalSchema sc) <|> (Right <$> evalSchema sc'))
evalSchema (RecordSchema alts    ) = runAlt f (getRecordFields alts)
  where
  f :: RecordField from b -> Maybe b
  f RequiredAp {..} = evalSchema fieldTypedSchema
  f OptionalAp {..} = evalSchema fieldTypedSchema

-- | Given a JSON 'Value' and a typed schema, extract a Haskell value
decodeWith :: TypedSchemaFlex from a -> Value -> Result a
decodeWith sc v = decoder >>= ($ v)
  where decoder = decodeFromWith sc (NE.head $ extractSchema sc)

decodeFromWith
  :: TypedSchemaFlex from a -> Schema -> Result(Value -> Result a)
-- TODO merge runSchema and decodeFromWith ?
-- TODO expose non-termination as an effect
decodeFromWith sc source =  Result $ todoExposeNonTermination $ go [] [] sc source
 where
  todoExposeNonTermination = lift

  failWith ctx e = throwError [(reverse ctx, e)]

  go
    :: [(SchemaName, Attempt TracedMismatches (Value -> Result Void))]
    -> Trace
    -> TypedSchemaFlex from a
    -> Schema
    -> Attempt TracedMismatches (Value -> Result a)
  -- go _ _ t s | pTraceShow ("decode", t,s) False = undefined
  go _nv _tx (TPure a) Unit  = pure $ \_ -> pure a
  go _   ctx  TEmpty{} Empty = pure $ const $ failWith ctx EmptySchema
  go env ctx (TNamed n sc tof _) (Named n' s) | n == n' = case lookup n env of
    Just sol ->
       (fmap . fmap . fmap) (tof . unsafeCoerce) sol
    Nothing ->
      let sol    = go ((n, solDynLater) : env) ctx sc s
          solDelayed = (fmap . fmap) delay sol
          solDyn = (fmap . fmap . fmap) unsafeCoerce solDelayed
          solDynLater = pure $ fromMaybe (error "impossible") $ attemptSuccess solDyn
      in  (fmap . fmap . fmap) tof sol
  -- go env ctx (TNamed _ sc tof _) s = (fmap . fmap . fmap) tof $ go env ctx sc s
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
    pure $ \case
      A.Array x -> tof <$> traverse f x
      other     -> failWith ctx (ValueMismatch s other)
  go env ctx (TMap sc tof _) s@(StringMap src) = do
    f <- go env ("Map" : ctx) sc src
    pure $ \case
      A.Object x -> tof <$> traverse f x
      other      -> failWith ctx (ValueMismatch s other)
  go _nv ctx (TPrim n tof _) (Prim src)
    | n /= src = failWith ctx (PrimMismatch n src)
    | otherwise = pure $ \x -> case tof x of
      A.Error   e -> failWith ctx (PrimError n (pack e))
      A.Success a -> pure a
  go env ctx (TAllOf scc    ) src = do
    let parsers = map (\sc -> runAttempt $ go env ctx sc src) (NE.toList scc)
    case partitionEithers parsers of
      (ee, []) -> failWith ctx (AllAlternativesFailed (concat ee))
      (_ , pp) -> do
        pure $ \x -> asum (map ($ x) pp)
  go env ctx (TOneOf sc sc' tof _) src = do
    let parserL = runAttempt $ (Left <.>) <$> go env ctx sc src
    let parserR = runAttempt $ (Right <.>) <$> go env ctx sc' src
    case partitionEithers [parserL, parserR] of
      (ee, []) -> failWith ctx (AllAlternativesFailed (concat ee))
      (_ , pp) -> do
        pure $ \x -> tof <$> asum (map ($ x) pp)
  go env ctx (RecordSchema (RecordFields rec)) (Record src) = unliftGo $ coerce $ runAlt f' rec
   where
    sourceFields = Map.keysSet src

    liftGo = either (const empty) pure . runAttempt

    unliftGo = maybe (failWith ctx NoMatches) (pure . snd)
             . find @[] (\(tgtFields,_) -> null $ Set.difference sourceFields (fromList tgtFields))

    f' :: RecordField from a -> ([] `Compose`(,) [Text] `Compose` (->) Value `Compose` Result) a
    f' x = coerce (f x)

    f :: RecordField from a -> [([Text], Value -> Result a)]
    f RequiredAp {..} = case Map.lookup fieldName src of
      Nothing       -> empty
      Just srcField -> do
        guard $ isRequired srcField
        f <- liftGo $ go env (fieldName : ctx) fieldTypedSchema (fieldSchema srcField)
        pure $
          let decoder v =
               case v of
                  A.Object o -> case Map.lookup fieldName o of
                    Nothing ->
                      failWith (fieldName : ctx) (MissingRecordField fieldName)
                    Just v -> f v
                  other -> failWith ctx (InvalidRecordValue other)
          in ([fieldName], decoder)
    f OptionalAp {..} = case Map.lookup fieldName src of
      Nothing       -> pure ([], const $ pure fieldDefaultValue)
      Just srcField -> do
        f <- liftGo $ go env (fieldName : ctx) fieldTypedSchema (fieldSchema srcField)
        pure $
          let decoder v = case v of
                A.Object o -> case Map.lookup fieldName o of
                  Nothing -> pure fieldDefaultValue
                  Just v  -> f v
                other -> failWith ctx (InvalidRecordValue other)
          in ([fieldName], decoder)

  go env ctx s  (OneOf xx) = asum $ fmap (go env ctx s) xx
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
