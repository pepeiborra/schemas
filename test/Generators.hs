{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Generators where

import           Control.Lens    (review)
import           Control.Monad
import           Data.Text       (Text)
import           GHC.Exts        (IsList (..))
import           Numeric.Natural
import           Schemas
import           Test.QuickCheck

hasOneOf :: Schema -> Bool
hasOneOf (Array sc) = hasOneOf sc
hasOneOf (StringMap sc) = hasOneOf sc
hasOneOf (Record ff) = any (hasOneOf . fieldSchema) ff
hasOneOf (AllOf scc) = any hasOneOf scc
hasOneOf (OneOf   _) = True
hasOneOf _ = False

hasAllOf :: Schema -> Bool
hasAllOf (Array sc) = hasAllOf sc
hasAllOf (StringMap sc) = hasAllOf sc
hasAllOf (Record ff) = any (hasAllOf . fieldSchema) ff
hasAllOf (OneOf scc) = any hasAllOf scc
hasAllOf (AllOf   _) = True
hasAllOf _ = False

instance Arbitrary Schema where
  arbitrary = sized genSchema
  shrink (Record fields) =
    [Record [(n,Field sc' req)] | (n,Field sc req) <- toList fields, sc' <- shrink sc]
  shrink (AllOf scc) = [AllOf [sc'] | sc <- toList scc, sc' <- shrink sc]
  shrink (OneOf scc) = [OneOf [sc'] | sc <- toList scc, sc' <- shrink sc]
  shrink (Array sc) = [sc]
  shrink (StringMap sc) = [sc]
  shrink _ = []

newtype SmallNatural = SmallNatural Natural
  deriving (Eq, Ord, Num)
  deriving newtype Show

instance Arbitrary (SmallNatural) where
  arbitrary = fromIntegral <$> choose (0::Int, 10)
  shrink 0 = []
  shrink n = [n-1]

fieldNames :: [Text]
fieldNames = ["field1", "field2", "field3"]

constructorNames :: [Text]
constructorNames = ["constructor1", "constructor2"]

genSchema ::  Int -> Gen (Schema)
genSchema 0 = elements [Empty, Prim]
genSchema n = frequency
  [ (10,) $  Record <$> do
      nfields <- choose (1,2)
      fieldArgs <- replicateM nfields (scale (`div` 2) arbitrary)
      return $ fromList (zipWith (\n (sc,a) -> (n, Field sc a)) fieldNames fieldArgs)
  , (10,) $ Array  <$> scale(`div`2) arbitrary
  , (10,) $ Enum   <$> do
      n <- choose (1,2)
      return $ fromList $ take n ["Enum1", "Enum2"]
  , (1,) $ AllOf . fromList <$> listOf1 (genSchema (n`div`10))
  , (1,) $ OneOf . fromList <$> listOf1 (genSchema (n`div`10))
  , (5,) $ review _Union <$> do
      nconstructors <- choose (1,2)
      args <- replicateM nconstructors (genSchema (n`div`nconstructors))
      return $ fromList $ zip constructorNames args
  , (50,) $ genSchema 0
  ]
