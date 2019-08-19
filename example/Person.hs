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

schemaEducation :: TypedSchema Education
schemaEducation = union'
  [Alt "NoEducation" #_NoEducation empty
  ,Alt "PhD" #_PhD string
  ,Alt "Degree" #_Degree string
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

schemaPerson :: TypedSchema (Person Identity)
schemaPerson = record schema
 where
  schema :: RecordSchema Person
  schema = Person (Required "name" string)
                  (Required "age" int)
                  (Required "addresses" $ list string)
                  (Required "education" schemaEducation)

enumerate :: (Bounded a, Enum a) => [a]
enumerate = [minBound ..]

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

-- >>> import Data.Aeson
-- >>> encode $ pack schemaPerson laura
-- "{\"education\":{\"Degree\":\"English\"},\"addresses\":[\"2 Edward Square\"],\"age\":38,\"name\":\"Laura\"}"

-- >>> encode $ pack schemaPerson pepe
-- <interactive>:15:2-7: error:
--     Variable not in scope:
--       encode :: aeson-1.4.2.0:Data.Aeson.Types.Internal.Value -> t

-- >>> import Text.Pretty.Simple
-- >>> pPrintNoColor $ unpack schemaPerson $ pack schemaPerson pepe
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
