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
  deriving (Generic, Eq, Show)

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

deriving instance Eq   (Person Identity)
deriving instance Show (Person Identity)

instance HasSchema (Person Identity) where
  schema = record
         $ Person (required "name")
                  (required "age")
                  (required "addresses")
                  (required "education")

pepe :: Person Identity

pepe = Person
  (Identity "Pepe")
  (Identity 38)
  (Identity ["2 Edward Square", "La Mar 10"])
  (Identity $ PhD "Computer Science")

-- >>> import Data.Aeson.Encode.Pretty
-- >>> import qualified Data.ByteString.Lazy.Char8 as B
-- >>> B.putStrLn $ encodePretty $ encode pepe
-- {
--     "education": {
--         "PhD": "Computer Science"
--     },
--     "addresses": [
--         "2 Edward Square",
--         "La Mar 10"
--     ],
--     "age": 38,
--     "name": "Pepe"
-- }
-- >>> B.putStrLn $ encodePretty $ encode (theSchema @(Person Identity))
-- {
--     "Record": {
--         "education": {
--             "schema": {
--                 "Union": {
--                     "PhD": "String",
--                     "Degree": "String",
--                     "NoEducation": "Empty"
--                 }
--             }
--         },
--         "addresses": {
--             "schema": {
--                 "Array": "String"
--             }
--         },
--         "age": {
--             "schema": "Number"
--         },
--         "name": {
--             "schema": "String"
--         }
--     }
-- }

-- >>> import Text.Pretty.Simple
-- >>> pPrintNoColor $ decode @(Person Identity) $ encode pepe
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
