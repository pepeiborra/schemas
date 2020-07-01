{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
module Person4 where

import           Control.Applicative
import           Data.Generics.Labels  ()
import           GHC.Generics
import qualified Generics.SOP as SOP
import           Person
import           Person2
import           Schemas

-- | v4 adds new fields
data Person4 = Person4
  { name      :: String
  , age       :: Int
  , addresses :: [String]
  , religion  :: Maybe Religion
  , education :: [Education]
  , a1,a2,a3,a4,a5 ,a6,a7,a8,a9,a10
  , b1,b2,b3,b4,b5,b6,b7,b8,b9,b10
  , c1,c2,c3,c4,c5,c6,c7,c8,c9,c10
  , d1,d2,d3,d4,d5,d6,d7,d8,d9,d10
    :: Bool
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance HasSchema Person4 where
  schema =
    record
      $   Person4
      <$> field "name"      Person4.name
      <*> field "age"       Person4.age
      <*> field "addresses" Person4.addresses
      <*> optField "religion" Person4.religion
      <*> (   field "education"   Person4.education
          <|> field "studies" Person4.education
          )
      <*> grab "a1"  a1
      <*> grab "a2"  a2
      <*> grab "a3"  a3
      <*> grab "a4"  a4
      <*> grab "a5"  a5
      <*> grab "a6"  a6
      <*> grab "a7"  a7
      <*> grab "a8"  a8
      <*> grab "a9"  a9
      <*> grab "a10" a10
      <*> grab "b1"  b1
      <*> grab "b2"  b2
      <*> grab "b3"  b3
      <*> grab "b4"  b4
      <*> grab "b5"  b5
      <*> grab "b6"  b6
      <*> grab "b7"  b7
      <*> grab "b8"  b8
      <*> grab "b9"  b9
      <*> grab "b10" b10
      <*> grab "c1"  c1
      <*> grab "c2"  c2
      <*> grab "c3"  c3
      <*> grab "c4"  c4
      <*> grab "c5"  c5
      <*> grab "c6"  c6
      <*> grab "c7"  c7
      <*> grab "c8"  c8
      <*> grab "c9"  c9
      <*> grab "c10" c10
      <*> grab "d1"  d1
      <*> grab "d2"  d2
      <*> grab "d3"  d3
      <*> grab "d4"  d4
      <*> grab "d5"  d5
      <*> grab "d6"  d6
      <*> grab "d7"  d7
      <*> grab "d8"  d8
      <*> grab "d9"  d9
      <*> grab "d10" d10
    where grab n get = field n get <|> pure False

pepe4 :: Person4
pepe4 = Person4
  "Pepe"
  38
  ["2 Edward Square", "La Mar 10"]
  Nothing
  [PhD "Computer Science", Degree "Engineering"]
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
  False
