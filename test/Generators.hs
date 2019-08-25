{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Generators where

import Control.Monad
import Data.Text (Text)
import GHC.Exts (IsList(..))
import Numeric.Natural
import Schemas
import Test.QuickCheck

instance Arbitrary Schema where
  arbitrary = sized genSchema

instance Arbitrary Natural where
  arbitrary = fmap (fromIntegral . getPositive @Int) arbitrary

fieldNames :: [Text]
fieldNames = ["field1", "field2", "field3"]

constructorNames :: [Text]
constructorNames = ["constructor1", "constructor2"]

genSchema ::  Int -> Gen (Schema)
genSchema 0 = elements [Empty, Bool, Number, String]
genSchema _ = oneof
  [ Record <$> do
      nfields <- choose (1,2)
      fieldArgs <- replicateM nfields (scale (`div` 2) arbitrary)
      return $ fromList (zipWith (\n (sc,a) -> (n, Field sc a)) fieldNames fieldArgs)
  , Union  <$> do
      nconstructors <- choose (1,2)
      args <- replicateM nconstructors (scale (`div` 2) arbitrary)
      return $ fromList $ zip constructorNames args
  , Array  <$> scale(`div`2) arbitrary
  , Enum   <$> do
      n <- choose (1,2)
      return $ fromList $ take n ["Enum1", "Enum2"]
  , genSchema 0
  ]
