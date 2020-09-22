{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}

module Person3 where

import           Control.Applicative
import           Data.Generics.Labels  ()
import           GHC.Generics
import qualified Generics.SOP as SOP
import           Person
import           Person2
import           Schemas

-- | v3 adds recursive field 'spouse', which leads to cycles
data Person3 = Person3
  { name      :: String
  , age       :: Int
  , addresses :: [String]
  , spouse    :: Maybe Person3    -- new recursive field
  , religion  :: Maybe Religion
  , education :: [Education]
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance HasSchema Person3 where
  schema  = named "Person3"
          $ record
          $ Person3 <$> field "name" Person3.name
                    <*> field "age" Person3.age
                    <*> field "addresses" Person3.addresses
                    <*> optField "spouse" Person3.spouse
                    <*> optField "religion" Person3.religion
                    <*> (field "studies" Person3.education <|> field "education" Person3.education)

laura3, pepe3 :: Person3

-- pepe3 has a cycle with laura3
pepe3 = Person3
  "Pepe"
  38
  ["2 Edward Square", "La Mar 10"]
  (Just laura3)
  Nothing
  [PhD "Computer Science", Degree "Engineering"]

-- laura3 has a cycle with pepe3
laura3 = pepe3  { name      = "Laura"
                , spouse    = Just pepe3
                , education = [Degree "English"]
                , addresses = ["2 Edward Square"]
                , religion  = Just Catholic
                }

martin :: Person3
martin = Person3 "Martin" 10 [] Nothing Nothing []


-- >>> import qualified Data.ByteString.Lazy.Char8 as B
-- >>> import Data.Aeson.Encode.Pretty
-- >>> B.putStrLn $ encodePretty $ finiteEncode 4 laura3
-- {
--     "L": {
--         "spouse": {
--             "L": {}
--         },
--         "religion": "Catholic",
--         "addresses": [
--             "2 Edward Square"
--         ],
--         "age": 38,
--         "studies": {
--             "Degree": "English"
--         },
--         "name": "Laura"
--     }
-- }

-- Unpacking infinite data is not supported currently
-- >>> decode @Person3 (finiteEncode 4 pepe3)
-- Left (MissingRecordField {name = "name", context = ["spouse"]})
