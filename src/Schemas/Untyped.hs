{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS -Wno-name-shadowing    #-}

module Schemas.Untyped where

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
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.List                  (find)
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NE
import           Data.Maybe
import           Data.Text                  (Text, pack, unpack)
import           Data.Typeable
import           GHC.Exts                   (IsList (..))
import           GHC.Generics               (Generic)
import           Numeric.Natural
import           Prelude                    hiding (lookup)
import           Text.Read
import           Text.Show.Functions        ()

-- import           Debug.Pretty.Simple


-- Schemas
-- --------------------------------------------------------------------------------

-- | A schema for untyped data, such as JSON or XML.
--
--   * introduction forms: 'extractSchema', 'theSchema', 'mempty'
--   * operations: 'isSubtypeOf', 'versions', 'coerce', 'validate'
--   * composition: '(<>)'
data Schema a
  = Array (Schema a)
  | StringMap (Schema a)
  | Enum   (NonEmpty Text)
  | Record (HashMap Text (Field a))
  | OneOf (NonEmpty (Schema a))   -- ^ Decoding works for all alternatives, encoding only for one
  | Prim Text                     -- ^ Carries the name of primitive type
  | Var a
  | Mu (a -> NonEmpty (Schema a))          -- ^ Recursive binder with non-determinism
  deriving (Generic, Show)

type SchemaSealed = forall a . Schema a

newtype Sealed f = Sealed {unseal :: forall a . f a}

overSealed :: (forall a . f a -> g a) -> Sealed f -> Sealed g
overSealed f (Sealed x) = Sealed (f x)

instance Monoid (Schema a) where mempty = Empty
instance Semigroup (Schema a) where
  Empty <> x    = x
  x <> Empty    = x
  OneOf aa <> b = OneOf (aa <> [b])
  b <> OneOf aa = OneOf ([b] <> aa)
  a <> b        = OneOf [a,b]

data Field a = Field
  { fieldSchema :: Schema a
  , isRequired  :: Bool -- ^ defaults to True
  }
  deriving (Generic)

instance Show a => Show (Field a) where
  showsPrec p (Field sc True)  = showsPrec p sc
  showsPrec p (Field sc False) = ("?" ++) . showsPrec p sc

fieldSchemaL :: Applicative f => ((Schema a) -> f (Schema a)) -> (Field a) -> f (Field a)
fieldSchemaL f Field{..} = Field <$> f fieldSchema <*> pure isRequired

pattern Empty :: (Schema a)
pattern Empty <- Record [] where Empty = Record []

pattern Union :: NonEmpty (Text, (Schema a)) -> (Schema a)
pattern Union alts <- (preview _Union -> Just alts) where
  Union alts = review _Union alts

_Empty :: Prism' (Schema a) ()
_Empty = prism' build match
  where
    build () = Record []

    match (Record []) = Just ()
    match _           = Nothing

_Union :: Prism' (Schema a) (NonEmpty (Text, (Schema a)))
_Union = prism' build match
  where
    build = OneOf . fmap (\(n,sc) -> Record [(n, Field sc True)])

    match (OneOf scc) = traverse viewAlt scc
    match _           = Nothing

    viewAlt :: (Schema a) -> Maybe (Text, (Schema a))
    viewAlt (Record [(n,Field sc True)]) = Just (n, sc)
    viewAlt _                            = Nothing

-- --------------------------------------------------------------------------------
-- Finite schemes

-- | Ensure that a '(Schema a)' is finite by enforcing a max depth.
--   The result is guaranteed to be a supertype of the input.
finite :: Natural -> (Schema a) -> (Schema a)
finite = go
 where
  go :: Natural -> (Schema a) -> (Schema a)
  go 0 _ = Empty
  go d (Record    opts) = Record $ fromList $ mapMaybe
    (\(fieldname, Field sc isOptional) -> case go (max 0 (pred d)) sc of
      Empty -> Nothing
      sc'   -> Just (fieldname, Field sc' isOptional)
    )
    (Map.toList opts)
  go d (Array     sc  ) = Array (go (max 0 (pred d)) sc)
  go d (StringMap sc  ) = StringMap (go (max 0 (pred d)) sc)
  go d (OneOf     opts) = let d' = max 0 (pred d) in OneOf (finite d' <$> opts)
  go _ other            = other

-- | Ensure that a 'Value' is finite by enforcing a max depth in a schema preserving way
finiteValue :: Show a => Validators -> Natural -> (Schema a) -> Value -> Value
finiteValue validators d sc
  | Right cast <- isSubtypeOf validators sc (finite d sc) = cast
  | otherwise = error "bug in isSubtypeOf"

-- ------------------------------------------------------------------------------------------------------
-- Validation

type Trace = [Text]

data Mismatch
  = MissingRecordField { name :: Text }
  | MissingEnumChoices { choices :: NonEmpty Text }
  | OptionalRecordField { name :: Text }
  | InvalidRecordField { name :: Text, mismatches :: [(Trace, Mismatch)] }
  | InvalidEnumValue   { given :: Text, options :: NonEmpty Text}
  | InvalidConstructor { name :: Text}
  | InvalidUnionValue  { contents :: Value}
  | SchemaMismatch     {a, b :: String}
  | ValueMismatch      {expected :: String, got :: Value}
  | EmptyAllOf
  | PrimValidatorMissing { name :: Text }
  | PrimError {name, primError :: Text}
  | PrimMismatch {have, want :: Text}
  | InvalidChoice{choiceNumber :: Int}
  | TryFailed { name :: Text }
  | AllAlternativesFailed { mismatches :: [(Trace,Mismatch)]}
  | UnexpectedAllOf
  | NoMatches
  deriving (Typeable)

instance Show Mismatch where
  show _ = "TBI"

instance Exception Mismatch

type Validators = HashMap Text ValidatePrim
type ValidatePrim = Value -> Maybe Text

-- | Structural validation of a JSON value against a schema
--   Ignores extraneous fields in records
validate :: forall a. Show a => Validators -> (Schema a) -> Value -> [(Trace, Mismatch)]
validate validators sc v = either (fmap (first reverse)) (\() -> []) $ runExcept (go [] sc v) where
  failWith :: Trace -> Mismatch -> Except [(Trace, Mismatch)] ()
  failWith ctx e = throwE [(ctx, e)]

  go :: Trace -> (Schema a) -> Value -> Except [(Trace, Mismatch)] ()
  go ctx (Prim n) x = case Map.lookup n validators of
    Nothing -> failWith ctx (PrimValidatorMissing n)
    Just v -> case v x of
      Nothing  -> pure ()
      Just err -> failWith ctx (PrimError n err)
  go ctx (StringMap sc) (A.Object xx) = ifor_ xx $ \i -> go (i : ctx) sc
  go ctx (Array sc) (A.Array xx) =
    ifor_ xx $ \i -> go (pack ("[" <> show i <> "]") : ctx) sc
  go ctx (Enum opts) (A.String s) =
    if s `elem` opts then pure () else failWith ctx (InvalidEnumValue s opts)
  go ctx (Record ff) (A.Object xx) = ifor_ ff $ \n (Field sc opt) ->
    case (opt, Map.lookup n xx) of
      (_   , Just y ) -> go (n : ctx) sc y
      (True, Nothing) -> pure ()
      _               -> failWith ctx (MissingRecordField n)
  go ctx (Union constructors) v@(A.Object xx) = case toList xx of
    [(n, v)] | Just sc <- lookup n constructors -> go (n : ctx) sc v
             | otherwise -> failWith ctx (InvalidConstructor n)
    _ -> throwE [(ctx, InvalidUnionValue v)]
  go ctx (OneOf scc) v = case decodeAlternatives v of
    [(v, 0)] -> msum $ fmap (\sc -> go ctx sc v) scc
    alts     -> msum $ fmap
      (\(v, n) ->
        fromMaybe (failWith ctx (InvalidChoice n)) $ selectPath n $ fmap
          (\sc -> go (pack (show n) : ctx) sc v)
          (toList scc)
      )
      alts
  go ctx a           b = failWith ctx (ValueMismatch (show a) b)

-- ------------------------------------------------------------------------------------------------------
-- Subtype relation

-- | @sub `isSubtypeOf` sup@ returns a witness that @sub@ is a subtype of @sup@, i.e. a cast function @sub -> sup@
--
-- > Array Bool `isSubtypeOf` Bool
--   Just <function>
-- > Record [("a", Bool)] `isSubtypeOf` Record [("a", Number)]
--   Nothing
isSubtypeOf :: forall a . Show a => Validators -> (Schema a) -> (Schema a) -> Either [(Trace, Mismatch)] (Value -> Value)
isSubtypeOf validators sub sup = runExcept $ go [] sup sub
 where
  failWith :: Trace -> Mismatch -> Except [(Trace, Mismatch)] b
  failWith ctx m = throwE [(reverse ctx, m)]

        -- TODO go: fix confusing order of arguments
  go :: Trace -> (Schema a) -> (Schema a) -> Except [(Trace,Mismatch)] (Value -> Value)
--  go _ sup sub | pTraceShow ("isSubtypeOf", sub, sup) False = undefined
  go _tx Empty         _         = pure $ const emptyValue
  go _tx (Array     _) Empty     = pure $ const (A.Array [])
  go _tx (Record    _) Empty     = pure $ const emptyValue
  go _tx (StringMap _) Empty     = pure $ const emptyValue
  go _tx OneOf{}       Empty     = pure $ const emptyValue
  go ctx (Prim      a) (Prim b ) = do
    unless (a == b) $ failWith ctx (PrimMismatch b a)
    pure id
  go ctx (Array a)     (Array b) = do
    f <- go ("[]" : ctx) a b
    pure $ over (_Array . traverse) f
  go ctx (StringMap a) (StringMap b) = do
    f <- go ("Map" : ctx) a b
    pure $ over (_Object . traverse) f
  go ctx (Enum opts) (Enum opts') =
    case NE.nonEmpty $ NE.filter (`notElem` opts) opts' of
      Nothing -> pure id
      Just xx -> failWith ctx $ MissingEnumChoices xx
  go ctx (Union opts) (Union opts') = do
    ff <- forM opts' $ \(n, sc) -> do
      sc' :: Schema a <- maybe (failWith ctx $ InvalidConstructor n) return $ lookup n (toList opts)
      f   <- go (n : ctx) sc sc'
      return $ over (_Object . ix n) f
    return (foldr (.) id ff)
  go ctx (Record opts) (Record opts') = do
    forM_ (Map.toList opts) $ \(n, f) ->
      unless (not (isRequired f) || Map.member n opts') $
        failWith ctx $ MissingRecordField n
    ff <- forM (Map.toList opts') $ \(n', f'@(Field sc' _)) -> do
      case Map.lookup n' opts of
        Nothing -> do
          pure $ over (_Object) (Map.delete n')
        Just f@(Field sc _) -> do
          unless (not (isRequired f) || isRequired f') $
            failWith ctx $ OptionalRecordField n'
          witness <- go (n' : ctx) sc sc'
          pure $ over (_Object . ix n') witness
    return (foldr (.) id ff)
  go ctx sup (OneOf [sub]) = go ctx sup sub
  go ctx sup (OneOf sub  ) = do
    alts <- traverse (\sc -> (sc, ) <$> go ctx sup sc) sub
    return $ \v -> head $ mapMaybe
      (\(sc, f) -> if null (validate validators sc v) then Just (f v) else Nothing)
      (toList alts)
  go ctx (OneOf sup) sub = asum $ fmap (\x -> go ctx x sub) sup
  go ctx (Array a) b = do
    f <- go ctx a b
    pure (A.Array . fromList . (: []) . f)
  -- go _tx a b | a == b  = pure id
  go ctx a b           = failWith ctx (SchemaMismatch (show a) (show b))

-- ----------------------------------------------
-- Utils

type Path = Int

selectPath :: Path -> [a] -> Maybe a
selectPath 0 (x : _)  = Just x
selectPath n (_ : xx) = selectPath (pred n) xx
selectPath _ _        = Nothing

tag :: Int -> Text
tag i = "#" <> pack (show i)

decodeAlternatives :: Value -> [(Value, Path)]
decodeAlternatives obj@(A.Object x) =
  case
      [ (v, n) | (unpack -> '#' : (readMaybe -> Just n), v) <- Map.toList x ]
    of
      []    -> [(obj, 0)]
      other -> other
decodeAlternatives x = [(x,0)]
-- | Generalized lookup for Foldables
lookup :: (Eq a, Foldable f) => a -> f (a,b) -> Maybe b
lookup a = fmap snd . find ((== a) . fst)

-- Is there more than one choice here ? Maybe this should be configuration
emptyValue :: Value
emptyValue = A.object []
