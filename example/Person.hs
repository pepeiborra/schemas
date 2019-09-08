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

data Person = Person
  { name      :: String
  , age       :: Int
  , addresses :: [String]
  , studies   :: Education
  }
  deriving (Eq, Show)

instance HasSchema Person where
  schema = record $
    Person <$> field "name" Person.name
           <*> field "age" age
           <*> field "addresses" addresses
           <*> field "studies" studies

pepe :: Person
pepe = Person
  "Pepe"
  38
  ["2 Edward Square", "La Mar 10"]
  (PhD "Computer Science")

-- >>> import Data.Aeson.Encode.Pretty
-- >>> import qualified Data.ByteString.Lazy.Char8 as B
-- >>> B.putStrLn $ encodePretty $ encode pepe
-- {
--     "studies": {
--         "PhD": "Computer Science"
--     },
--     "addresses": [
--         "2 Edward Square",
--         "La Mar 10"
--     ],
--     "age": 38,
--     "name": "Pepe"
-- }

-- >>> B.putStrLn $ encodePretty $ encode (theSchema @Person)
-- {
--     "Record": {
--         "studies": {
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
-- >>> pPrintNoColor $ decode @Person $ encode pepe
-- Right
--     ( Person
--         { name = "Pepe"
--         , age = 38
--         , addresses =
--             [ "2 Edward Square"
--             , "La Mar 10"
--             ]
--         , studies =
--             ( PhD { unPhD = "Computer Science" } )
--         }
--     )

-- >>> import Data.Aeson.Encode.Pretty
-- >>> import qualified Data.ByteString.Lazy.Char8 as B
-- >>> B.putStrLn $ encodePretty $ encode (extractSchema applicativePersonSchema)
-- {
--     "Record": {
--         "studies": {
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
