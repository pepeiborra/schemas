{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
import           Data.HashSet               (HashSet)
import           Data.List                  (find, intersperse, intercalate)
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NE
import           Data.Maybe
import           Data.Semigroup
import           Data.Text                  (Text, pack, unpack)
import           Data.Typeable
import           GHC.Exts                   (IsList (..), IsString(..))
import           GHC.Generics               (Generic)
import           Prelude                    hiding (lookup)
import           Text.Read
import           Text.Show.Functions        ()

-- Schemas
-- --------------------------------------------------------------------------------

newtype SchemaName = SchemaName String
  deriving newtype (Eq, IsString)

instance Show SchemaName where show (SchemaName s) = s

-- | A schema for untyped data, such as JSON or XML.
--
--   * introduction forms: 'extractSchema', 'theSchema', 'mempty'
--   * operations: 'isSubtypeOf', 'versions', 'coerce', 'validate'
--   * composition: '(<>)'
data Schema
  = Array Schema
  | StringMap Schema
  | Enum   (NonEmpty Text)
  | Record (HashMap Text Field)
  | OneOf (NonEmpty Schema)   -- ^ Decoding works for all alternatives, encoding only for one
  | Prim Text                     -- ^ Carries the name of primitive type
  | Named SchemaName Schema
  | Empty
  deriving (Eq, Generic)

instance Monoid Schema where mempty = Empty
instance Semigroup Schema where
  Empty <> x    = x
  x <> Empty    = x
  OneOf aa <> b = OneOf (aa <> [b])
  b <> OneOf aa = OneOf ([b] <> aa)
  a <> b        = OneOf [a,b]

instance Show Schema where
  showsPrec = go []   where
    go _een p  Empty          = showParen (p>0) $ ("Empty " ++)
    go seen _ (Array     sc) = (('[' :) . go seen 5 sc . (']' :))
    go seen p (StringMap sc) = showParen (p > 5) (("Map " ++) . go seen 5 sc)
    go _een p (Enum opts) =
      showParen (p > 5) (intercalate "|" (NE.toList $ fmap unpack opts) ++)
    go seen p (OneOf scc) = showParen (p > 5) $ foldr (.) id $ NE.intersperse
      (" | " ++)
      (fmap (go seen 6) scc)
    go seen _ (Record fields) =
      ('{' :)
        . foldr
            (.)
            id
            (intersperse
              (", " ++)
              (fmap
                (\(r, Field {..}) ->
                  (unpack r ++) . ((if isRequired then " :: " else " ?? ")  ++) . go seen 0 fieldSchema
                )
                (Map.toList fields)
              )
            )
        . ('}' :)
    go _een _ (Prim t    ) = (unpack t ++)
    go seen p (Named n sc) = case n `elem` seen of
      False ->
        ("let " ++)
          . (show n ++)
          . (" = " ++)
          . self
          . (" in " ++)
          . (show n ++)
      True -> (show n ++)
      where self = go (n : seen) p sc

data Field = Field
  { fieldSchema :: Schema
  , isRequired  :: Bool -- ^ defaults to True
  }
  deriving (Eq, Generic)

instance Show Field where
  showsPrec p (Field sc True)  = showsPrec p sc
  showsPrec p (Field sc False) = ("?" ++) . showsPrec p sc

fieldSchemaL :: Applicative f => (Schema -> f Schema) -> Field -> f Field
fieldSchemaL f Field{..} = Field <$> f fieldSchema <*> pure isRequired

pattern Unit :: Schema
pattern Unit <- Record [] where Unit = Record []

pattern Union :: NonEmpty (Text, Schema) -> Schema
pattern Union alts <- (preview _Union -> Just alts) where
  Union alts = review _Union alts

_Unit :: Prism' Schema ()
_Unit = prism' build match
  where
    build () = Record []

    match (Record []) = Just ()
    match _           = Nothing

_Union :: Prism' Schema (NonEmpty (Text, Schema))
_Union = prism' build match
  where
    build = foldMap (\(n,sc) -> Record [(n, Field sc True)])

    match (OneOf scc) = traverse viewAlt scc
    match x = (:| []) <$> viewAlt x

    viewAlt :: Schema -> Maybe (Text, Schema)
    viewAlt (Record [(n,Field sc True)]) = Just (n, sc)
    viewAlt _                            = Nothing

-- ------------------------------------------------------------------------------------------------------
-- Validation

type Trace = [Text]

data Mismatch
  = MissingRecordField { name :: Text }
  | MissingEnumChoices { choices :: NonEmpty Text }
  | OptionalRecordField { name :: Text }
  | InvalidRecordField { name :: Text, mismatches :: [(Trace, Mismatch)] }
  | InvalidEnumValue   { given :: Text, options :: NonEmpty Text}
  | InvalidRecordValue { value :: Value }
  | InvalidConstructor { name :: Text}
  | InvalidUnionValue  { contents :: Value}
  | SchemaMismatch     {a, b :: Schema}
  | ValueMismatch      {expected :: Schema, got :: Value}
  | EmptySchema
  | PrimValidatorMissing { name :: Text }
  | PrimError {name, primError :: Text}
  | PrimMismatch {have, want :: Text}
  | InvalidChoice{choiceNumber :: Int}
  | UnusedFields (HashSet Text)
  | AllAlternativesFailed { mismatches :: [(Trace,Mismatch)]}
  | UnexpectedAllOf
  | NoMatches
  deriving (Eq, Show, Typeable)

instance Exception Mismatch

type Validators = HashMap Text ValidatePrim
type ValidatePrim = Value -> Maybe Text

-- | Structural validation of a JSON value against a schema
--   Ignores extraneous fields in records
validate :: Validators -> Schema -> Value -> [(Trace, Mismatch)]
validate validators sc v = either (fmap (first reverse)) (\() -> []) $ runExcept (go [] sc v) where
  failWith :: Trace -> Mismatch -> Except [(Trace, Mismatch)] ()
  failWith ctx e = throwE [(ctx, e)]

  go :: Trace -> Schema -> Value -> Except [(Trace, Mismatch)] ()
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
  go ctx a           b = failWith ctx (ValueMismatch a b)

-- ------------------------------------------------------------------------------------------------------
-- Subtype relation

-- | @sub `isSubtypeOf` sup@ returns a witness that @sub@ is a subtype of @sup@, i.e. a cast function @sub -> sup@
--
-- > Array Bool `isSubtypeOf` Bool
--   Just <function>
-- > Record [("a", Bool)] `isSubtypeOf` Record [("a", Number)]
--   Nothing
isSubtypeOf :: Validators -> Schema -> Schema -> Either [(Trace, Mismatch)] (Value -> Value)
isSubtypeOf validators sub sup = runExcept $ go [] [] sup sub
 where
  failWith :: Trace -> Mismatch -> Except [(Trace, Mismatch)] b
  failWith ctx m = throwE [(reverse ctx, m)]

        -- TODO go: fix confusing order of arguments
  go
    :: [(SchemaName, Except [(Trace, Mismatch)] (Value -> Value))]
    -> Trace
    -> Schema
    -> Schema
    -> Except [(Trace, Mismatch)] (Value -> Value)
  -- go _ _ sup sub | pTraceShow ("isSubtypeOf", sub, sup) False = undefined
  go env ctx (Named a sa) (Named b sb) | a == b =
    case lookup a env of
      Just sol -> sol
      Nothing ->
        let sol = go ((a,sol) : env) ctx sa sb
        in sol
  go _   _   Empty        Empty     = pure id
  go _nv _tx Unit         _         = pure $ const emptyValue
  go _nv _tx (Array     _) Unit     = pure $ const (A.Array [])
  go _nv _tx (Record    _) Unit     = pure $ const emptyValue
  go _nv _tx (StringMap _) Unit     = pure $ const emptyValue
  go _nv _tx OneOf{}       Unit     = pure $ const emptyValue
  go _nv ctx (Prim      a) (Prim b ) = do
    unless (a == b) $ failWith ctx (PrimMismatch b a)
    pure id
  go env ctx (Array a)     (Array b) = do
    f <- go env ("[]" : ctx) a b
    pure $ over (_Array . traverse) f
  go env ctx (StringMap a) (StringMap b) = do
    f <- go env ("Map" : ctx) a b
    pure $ over (_Object . traverse) f
  go _nv ctx (Enum opts) (Enum opts') =
    case NE.nonEmpty $ NE.filter (`notElem` opts) opts' of
      Nothing -> pure id
      Just xx -> failWith ctx $ MissingEnumChoices xx
  go env ctx (Union opts) (Union opts') = do
    ff <- forM opts' $ \(n, sc) -> do
      sc' :: Schema <- maybe (failWith ctx $ InvalidConstructor n) return $ lookup n (toList opts)
      f   <- go env (n : ctx) sc' sc
      return $ over (_Object . ix n) f
    return (foldr (.) id ff)
  go env ctx (Record opts) (Record opts') = do
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
          witness <- go env (n' : ctx) sc sc'
          pure $ over (_Object . ix n') witness
    return (foldr (.) id ff)
  go env ctx sup (OneOf [sub]) = go env ctx sup sub
  go env ctx sup (OneOf sub  ) = do
    alts <- traverse (\sc -> (sc, ) <$> go env ctx sup sc) sub
    return $ \v -> head $ mapMaybe
      (\(sc, f) -> if null (validate validators sc v) then Just (f v) else Nothing)
      (toList alts)
  go env ctx (OneOf sup) sub = asum $ fmap (\x -> go env ctx x sub) sup
  go env ctx (Array a) b = do
    f <- go env ctx a b
    pure (A.Array . fromList . (: []) . f)
  -- go _tx a b | a == b  = pure id
  go _nv ctx a b           = failWith ctx (SchemaMismatch a b)

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
