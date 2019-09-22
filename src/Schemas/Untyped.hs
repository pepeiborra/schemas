{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS -Wno-name-shadowing    #-}

module Schemas.Untyped where

import           Control.Lens               hiding (Empty, enum, (<.>))
import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.Aeson                 (Value)
import qualified Data.Aeson                 as A
import           Data.Aeson.Lens
import           Data.Biapplicative
import           Data.Either
import           Data.Foldable              (asum)
import           Data.Generics.Labels       ()
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.List                  (find)
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NE
import           Data.Maybe
import           Data.Text                  (Text, pack, unpack)
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
  | AllOf (NonEmpty Schema)   -- ^ Encoding and decoding work for all alternatives
  | OneOf (NonEmpty Schema)   -- ^ Decoding works for all alternatives, encoding only for one
  | Prim Text                 -- ^ Carries the name of primitive type
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
  deriving (Eq, Generic, Show)

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

-- --------------------------------------------------------------------------------
-- Finite schemes

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
finiteValue :: Validators -> Natural -> Schema -> Value -> Value
finiteValue validators d sc
  | Just cast <- isSubtypeOf validators sc (finite d sc) = cast
  | otherwise = error "bug in isSubtypeOf"

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

type Trace = [Text]

data ValidationError
  = MissingRecordField { name :: Text}
  | InvalidEnumValue   { given :: Text, options :: NonEmpty Text}
  | InvalidConstructor { name :: Text}
  | InvalidUnionValue { contents :: Value}
  | SchemaMismatch
  | EmptyAllOf
  | PrimValidatorMissing { name :: Text }
  | PrimError {name, primError :: Text}
  | InvalidChoice{choiceNumber :: Int}
  deriving (Eq, Show)

type Validators = HashMap Text ValidatePrim
type ValidatePrim = Value -> Maybe Text

-- | Structural validation of a JSON value against a schema
--   Ignores extraneous fields in records
validate :: Validators -> Schema -> Value -> [(Trace, ValidationError)]
validate validators sc v = either (fmap (first reverse)) (\() -> []) $ runExcept (go [] sc v) where
  failWith :: Trace -> ValidationError -> Except [(Trace, ValidationError)] ()
  failWith ctx e = throwE [(ctx, e)]

  go :: Trace -> Schema -> Value -> Except [(Trace, ValidationError)] ()
  go ctx (Prim n) x = case Map.lookup n validators of
    Nothing -> failWith ctx (PrimValidatorMissing n)
    Just v -> case v x of
      Nothing -> pure ()
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
  go ctx (AllOf scc) v = go ctx (OneOf scc) v
  go ctx _           _ = failWith ctx (SchemaMismatch)

-- ------------------------------------------------------------------------------------------------------
-- Subtype relation

-- | @sub `isSubtypeOf` sup@ returns a witness that @sub@ is a subtype of @sup@, i.e. a cast function @sub -> sup@
--
-- > Array Bool `isSubtypeOf` Bool
--   Just <function>
-- > Record [("a", Bool)] `isSubtypeOf` Record [("a", Number)]
--   Nothing
isSubtypeOf :: Validators -> Schema -> Schema -> Maybe (Value -> Value)
isSubtypeOf validators sub sup = go sup sub
 where
        -- TODO go: fix confusing order of arguments
  go Empty         _         = pure $ const emptyValue
  go (Array     _) Empty     = pure $ const (A.Array [])
  go (Record    _) Empty     = pure $ const emptyValue
  go (StringMap _) Empty     = pure $ const emptyValue
  go OneOf{}       Empty     = pure $ const emptyValue
  go (Prim      a) (Prim b ) = guard (a == b) >> pure id
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
      (\(sc, f) -> if null (validate validators sc v) then Just (f v) else Nothing)
      (toList alts)
  go (OneOf sup) sub = asum $ fmap (`go` sub) sup
  go a b | a == b  = pure id
  go _ _           = Nothing

-- ----------------------------------------------
-- Utils

type Path = Int

selectPath :: Path -> [a] -> Maybe a
selectPath 0 (x : _)  = Just x
selectPath n (_ : xx) = selectPath (pred n) xx
selectPath _ _        = Nothing

decodeAlternatives :: Value -> [(Value, Path)]
decodeAlternatives obj@(A.Object x) =
  case
      [ (v, n) | (unpack -> '#' : (readMaybe -> Just n), v) <- Map.toList x ]
    of
      []    -> [(obj, 0)]
      other -> other
decodeAlternatives x = [(x,0)]

encodeAlternatives :: NonEmpty Value -> Value
encodeAlternatives [x] = x
encodeAlternatives xx  = A.object $ fromList [ ("#" <> pack (show i), x) | (i,x) <- zip [(1::Int)..] (toList xx) ]

-- | Generalized lookup for Foldables
lookup :: (Eq a, Foldable f) => a -> f (a,b) -> Maybe b
lookup a = fmap snd . find ((== a) . fst)

-- Is there more than one choice here ? Maybe this should be configuration
emptyValue :: Value
emptyValue = A.object []
