{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | This module defines a 'TypedSchema' for the 'Schema' datatype,
--   inspired in the OpenApi 2.0 specification, which may be useful
--   to render 'Schema' values in OpeApi 2.0 format
module Schemas.OpenApi2
  ( OpenApi2Document(..)
  , OpenApi2Schema(..)
  , defOpenApi2Schema
  , OpenApi2Type(..)
  , OpenApi2Options(..)
  , defaultOptions
  , toOpenApi2Document
  , encodeAsOpenApi2Document
  )
where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Writer
import           Data.Aeson                 (Value)
import           Data.Functor
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import qualified Data.List.NonEmpty         as NE
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Generics.SOP               as SOP
import           GHC.Generics
import           Schemas
import           Schemas.SOP

-- | Given a schema free of undiscriminated unions
--   @encodeAsOpenApi2Document name schema@ produces an encoding of an
--   OpenApi2 document that models the given schema.
--   Failures are omitted, use 'toOpenApi2Document' if you care.
encodeAsOpenApi2Document :: OpenApi2Options -> Text -> Schema -> Value
encodeAsOpenApi2Document opts n sc =
  encode $ toOpenApi2Document opts (Map.fromList [(n, sc)])

-- | A catalog of definitions
data OpenApi2Document = OpenApi2Document
  { definitions :: HashMap Text OpenApi2Schema
  , failures    :: HashMap Text Reason
  }
  deriving (Show)

instance Monoid OpenApi2Document where
  mempty = OpenApi2Document [] []

instance Semigroup OpenApi2Document where
  OpenApi2Document d f <> OpenApi2Document d' f' =
    OpenApi2Document (d <> d') (f <> f')

instance HasSchema OpenApi2Document where
  schema =
    record $ OpenApi2Document
    <$> field "definitions" definitions
    <*> field "failures"    failures

-- | The representation of an OpenApi 2.0 schema
data OpenApi2Schema = OpenApi2Schema
  { _type                :: OpenApi2Type
  , additionalProperties :: Maybe OpenApi2Schema
  , discriminator        :: Maybe  Text
  , enum                 :: Maybe [Text]
  , format               :: Maybe Text
  , items                :: Maybe OpenApi2Schema
  , properties           :: Maybe (HashMap Text OpenApi2Schema)
  , required             :: Maybe [Text]
  }
  deriving (Generic, Show)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance HasSchema OpenApi2Schema where
  schema = named "OpenApi2" $ gSchema defOptions { fieldLabelModifier = dropWhile (== '_') }

defOpenApi2Schema :: OpenApi2Type -> OpenApi2Schema
defOpenApi2Schema t =
  OpenApi2Schema t Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data OpenApi2Type
  = OpenApi2Object
  | OpenApi2Array
  | OpenApi2Boolean
  | OpenApi2Integer
  | OpenApi2Number
  | OpenApi2String
  deriving (Bounded, Enum, Eq, Show)

instance HasSchema OpenApi2Type where
  schema = Schemas.enum (Text.toLower . Text.pack . drop 8 . show)
                        [minBound .. maxBound]

data OpenApi2Options = OpenApi2Options
  { -- | Please tell me what to do with Prims
    primMapping :: Text -> Maybe OpenApi2Schema
  }

defaultOptions :: OpenApi2Options
defaultOptions = OpenApi2Options { primMapping = f }
 where
  f "Boolean" = Just $ defOpenApi2Schema OpenApi2Boolean
  f "String"  = Just $ defOpenApi2Schema OpenApi2String
  f "Number"  = Just $ defOpenApi2Schema OpenApi2Number
  f "Integer" = Just $ defOpenApi2Schema OpenApi2Integer
  f _         = Nothing

toOpenApi2Document :: OpenApi2Options -> HashMap Text Schema -> OpenApi2Document
toOpenApi2Document opts schemas =
  foldMap wrap (Map.toList topLevelSchemas) <> internalSchemas
 where
  results = runExcept . runWriterT . toOpenApi2 (primMapping opts) <$> schemas

  (topLevelSchemas, internalSchemas) = runWriter $ forM results $ \case
    Left  reason      -> pure $ Left reason
    Right (sc, inner) -> tell inner $> Right sc

  wrap (n, Left reason) = OpenApi2Document [] [(n, reason)]
  wrap (n, Right sc   ) = OpenApi2Document [(n, sc)] []

newtype Reason = Unsupported Text
  deriving Show
  deriving newtype HasSchema

-- | Alternatives and undiscriminated Unions are not supported
toOpenApi2
  :: (Text -> Maybe OpenApi2Schema)
  -> Schema
  -> WriterT OpenApi2Document (Except Reason) OpenApi2Schema
toOpenApi2 prim Empty = lift $ throwE $ Unsupported "empty"
toOpenApi2 prim (Array sc) = toOpenApi2 prim sc
  <&> \sc2 -> (defOpenApi2Schema OpenApi2Array) { items = Just sc2 }
toOpenApi2 prim (StringMap sc) = toOpenApi2 prim sc <&> \sc2 ->
  (defOpenApi2Schema OpenApi2Object) { additionalProperties = Just sc2 }
toOpenApi2 _rim (Enum vals) = pure $ (defOpenApi2Schema OpenApi2String)
  { Schemas.OpenApi2.enum = Just (NE.toList vals)
  }
toOpenApi2 prim (Record fields) = do
  let req = [ n | (n, Field _ True) <- Map.toList fields ]
  pp <- traverse (toOpenApi2 prim . fieldSchema) fields
  return (defOpenApi2Schema OpenApi2Object) { properties = Just pp
                                            , required   = Just req
                                            }
toOpenApi2 prim (Union alts) = do
  altSchemas <- traverse (toOpenApi2 prim) (Map.fromList $ NE.toList alts)
  tell $ OpenApi2Document altSchemas []
  return $ (defOpenApi2Schema OpenApi2Object)
    { discriminator = Just "tag"
    , required      = Just ["tag"]
    , properties    = Just [("tag", defOpenApi2Schema OpenApi2String)]
    }
toOpenApi2 prim (Prim p) | Just y <- prim p = pure y
toOpenApi2 _rim (Prim p) = lift $ throwE $ Unsupported $ "Unknown prim: " <> p
toOpenApi2 _rim s@OneOf{} =
  lift $ throwE $ Unsupported $ "undiscriminated unions (OneOf): " <> Text.pack (show s)

-- TODO future work
-- fromOpenApi2 :: OpenApi2 -> Schema
-- fromOpenApi2 _ = undefined

