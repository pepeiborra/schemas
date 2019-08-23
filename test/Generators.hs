{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Generators where

import Control.Monad
import Data.Functor.Identity
import Data.List.NonEmpty (fromList)
import Data.Text (Text)
import Schemas
import Test.QuickCheck

instance Arbitrary Schema where
  arbitrary = sized genSchema

fieldNames :: [Identity Text]
fieldNames = ["field1", "field2", "field3"]

constructorNames :: [Identity Text]
constructorNames = ["constructor1", "constructor2"]

genSchema ::  Int -> Gen (Schema)
genSchema 0 = elements [Empty, Bool, Number, String]
genSchema _ = oneof
  [ Record <$> do
      nfields <- choose (1,2)
      fieldArgs <- replicateM nfields (scale (`div` 2) arbitrary)
      return $ fromList (zipWith (\n (sc,a) -> Field n sc a) fieldNames fieldArgs)
  , Union  <$> do
      nconstructors <- choose (1,2)
      args <- replicateM nconstructors (scale (`div` 2) arbitrary)
      return $ fromList $ zipWith Constructor constructorNames args
  , Array  <$> scale(`div`2) arbitrary
  , Enum   <$> do
      n <- choose (1,2)
      return $ fromList $ take n ["Enum1", "Enum2"]
  , genSchema 0
  ]
