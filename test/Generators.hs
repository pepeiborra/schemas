{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TupleSections              #-}
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
hasOneOf (Array sc)     = hasOneOf sc
hasOneOf (StringMap sc) = hasOneOf sc
hasOneOf (Record ff)    = any (hasOneOf . fieldSchema) ff
hasOneOf (OneOf _)      = True
hasOneOf _              = False

instance Arbitrary Schema where
  arbitrary = sized genSchema
  shrink (Record fields) =
    [Record [(n,Field sc' req)] | (n,Field sc req) <- toList fields, sc' <- shrink sc]
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

genSchema ::  Int -> Gen Schema
genSchema 0 = elements [Empty, Prim "A", Prim "B"]
genSchema n = frequency
  [ (10,) $ Record <$> do
      nfields <- choose (1,2)
      fieldArgs <- replicateM nfields (scale (`div` succ nfields) arbitrary)
      return $ fromList (zipWith (\n (sc,a) -> (n, Field sc a)) fieldNames fieldArgs)
  , (10,) $ Array <$> scale(`div` 4) arbitrary
  , (10,) $ Enum   <$> do
      n <- choose (1,2)
      return $ fromList $ take n ["Enum1", "Enum2"]
  , (1,) $ OneOf . fromList <$> listOf1 (genSchema (n`div`10))
  , (5,) $ review _Union <$> do
      nconstructors <- choose (1,2)
      args <- replicateM nconstructors (genSchema (n`div` succ nconstructors))
      return $ fromList $ zip constructorNames args
  , (50,) $ genSchema 0
  ]
