{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
module Person3 where

import           Data.Barbie
import           Data.Functor.Identity
import           Data.Generics.Labels  ()
import           GHC.Generics
import           Person
import           Person2
import           Schemas

-- | v3 adds recursive field 'spouse', which leads to cycles
data Person3 f = Person3
  { name      :: f String
  , age       :: f Int
  , addresses :: f [String]
  , spouse    :: f (Maybe (Person3 Identity))
  , religion  :: f (Maybe Religion)
  , education :: f Education
  }
  deriving Generic
  deriving anyclass (FunctorB, ProductB, TraversableB)

deriving instance Show (Person3 Identity)

instance HasSchema (Person3 Identity) where
  schema = record
          $ Person3 (required "name")
                    (required "age")
                    (required "addresses")
                    (optional "spouse")
                    (optional "religion")
                    (required "education")

laura3, pepe3 :: Person3 Identity

-- pepe3 has a cycle with laura3
pepe3 = Person3
  (Identity "Pepe")
  (Identity 38)
  (Identity ["2 Edward Square", "La Mar 10"])
  (Identity $ Just laura3)
  (Identity Nothing)
  (Identity $ PhD "Computer Science")

-- laura3 has a cycle with pepe3
laura3 = pepe3  { name      = Identity "Laura"
                , spouse    = Identity (Just pepe3)
                , education = Identity (Degree "English")
                , addresses = Identity ["2 Edward Square"]
                , religion  = Identity (Just Catholic)
                }

-- >>> import qualified Data.ByteString.Lazy.Char8 as B
-- >>> import Data.Aeson.Encode.Pretty
-- >>> B.putStrLn $ encodePretty $ finiteEncode 2 laura3
-- {
--     "spouse": {
--         "spouse": {},
--         "education": {},
--         "addresses": [],
--         "age": 38,
--         "name": "Pepe"
--     },
--     "education": {
--         "Degree": "English"
--     },
--     "religion": "Catholic",
--     "addresses": [
--         "2 Edward Square"
--     ],
--     "age": 38,
--     "name": "Laura"
-- }

-- Unpacking infinite data is not supported currently
-- >>> decode @(Person3 Identity) (finiteEncode 2 pepe3)
-- Left (MissingRecordField {name = "name", context = ["spouse","spouse"]})
