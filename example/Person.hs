{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
module Person where

import           Data.Barbie
import           Data.Functor.Identity
import           Data.Generics.Labels  ()
import           GHC.Generics
import           Schemas

data Education = NoEducation | Degree {unDegree :: String} | PhD {unPhD :: String}
  deriving (Generic, Show)

instance HasSchema Education where
  schema = union'
    [alt "NoEducation" #_NoEducation
    ,alt "PhD" #_PhD
    ,alt "Degree" #_Degree
    ]

data Person f = Person
  { name      :: f String
  , age       :: f Int
  , addresses :: f [String]
  , education :: f Education
  }
  deriving Generic
  deriving anyclass (FunctorB, ProductB, TraversableB)

deriving instance Show (Person Identity)

instance HasSchema (Person Identity) where
  schema = record
         $ Person (required "name")
                  (required "age")
                  (required "addresses")
                  (required "education")

laura, paula, pepe :: Person Identity

pepe = Person
  (Identity "Pepe")
  (Identity 38)
  (Identity ["2 Edward Square", "La Mar 10"])
  (Identity $ PhD "Computer Science")

laura = pepe { name      = Identity "Laura"
             , education = Identity (Degree "English")
             , addresses = Identity ["2 Edward Square"]
             }
paula = Person
  (Identity "paula")
  (Identity 35)
  (Identity ["La Mar 10"])
  (Identity $ Degree "Arts")

-- >>> import qualified Data.Aeson as A
-- >>> A.encode $ encode schemaPerson laura
-- "{\"education\":{\"Degree\":\"English\"},\"addresses\":[\"2 Edward Square\"],\"age\":38,\"name\":\"Laura\"}"

-- >>> import qualified Data.Aeson as A
-- >>> A.encode $ encode pepe

-- >>> import Text.Pretty.Simple
-- >>> pPrintNoColor $ decode schemaPerson $ encode schemaPerson pepe
-- Right
--     ( Person
--         { name = Identity "Pepe"
--         , age = Identity 38
--         , addresses = Identity
--             [ "2 Edward Square"
--             , "La Mar 10"
--             ]
--         , education = Identity
--             ( PhD { unPhD = "Computer Science" } )
--         }
--     )
