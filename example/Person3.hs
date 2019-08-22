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

-- | v3 adds recursive fields 'relatives' and 'spouse', which can lead to cycles
data Person3 f = Person3
  { name      :: f String
  , age       :: f Int
  , addresses :: f [String]
  , relatives :: f [Person3 Identity]
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
                    (required "relatives")
                    (optional "spouse")
                    (optional "religion")
                    (required "education")

laura3, paula3, pepe3 :: Person3 Identity

-- pepe3 has cycles with laura3 and paula3
pepe3 = Person3
  (Identity "Pepe")
  (Identity 38)
  (Identity ["2 Edward Square", "La Mar 10"])
  (Identity [paula3])
  (Identity $ Just laura3)
  (Identity Nothing)
  (Identity $ PhD "Computer Science")

-- laura3 has a cycle with pepe3
laura3 = pepe3  { name      = Identity "Laura"
                , spouse    = Identity (Just pepe3)
                , education = Identity (Degree "English")
                , addresses = Identity ["2 Edward Square"]
                , relatives = Identity []
                , religion  = Identity (Just Catholic)
                }

-- paula3 has a cycle with pepe3
paula3 = Person3
  (Identity "Paula")
  (Identity 35)
  (Identity ["La Mar 10"])
  (Identity [pepe3])
  (Identity Nothing)
  (Identity Nothing)
  (Identity $ Degree "Arts")

-- >>> import           Text.Pretty.Simple
-- >>> pPrintNoColor $ finiteEncode 2 laura3
-- *** Exception: stack overflow

-- >>> import           Text.Pretty.Simple
-- >>> pPrintNoColor $ finiteEncode 2 pepe3

-- Unpacking infinite data is not supported currently
-- >>> import           Text.Pretty.Simple
-- >>> pPrintNoColor $ unpack (finiteEncode 2 pepe3)
-- Left
--     ( MissingRecordField
--         { name = "name"
--         , context =
--             [ "[]"
--             , "relatives"
--             , "[]"
--             , "relatives"
--             ]
--         }
--     )