{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Person2 where

import           Data.Barbie
import           Data.Functor.Identity
import           Data.Generics.Labels  ()
import           Data.String
import           GHC.Generics
import           Person
import           Schemas

-- | The v2 of the Person schema adds a new optional field 'religion'
data Person2 f = Person2
  { name      :: f String
  , age       :: f Int
  , addresses :: f [String]
  , religion  :: f (Maybe Religion)
  , education :: f Education
  }
  deriving Generic
  deriving anyclass (FunctorB, ProductB, TraversableB)

data Religion = Catholic | Anglican | Muslim | Hindu
  deriving (Bounded, Enum, Eq, Show)

schemaReligion :: TypedSchema Religion
schemaReligion = enum (fromString . show) enumerate

schemaPerson2 :: TypedSchema (Person2 Identity)
schemaPerson2 = record schema
 where
  schema :: RecordSchema Person2
  schema = Person2 (Required "name" string)
                  (Required "age" int)
                  (Required "addresses" $ list string)
                  (Optional "religion" schemaReligion)
                  (Required "education" schemaEducation)

pepe2 :: Person2 Identity
pepe2 = Person2
  (Identity "Pepe")
  (Identity 38)
  (Identity ["2 Edward Square", "La Mar 10"])
  (Identity Nothing)
  (Identity $ PhD "Computer Science")

laura2 :: Person2 Identity
laura2 = pepe2 { name      = Identity "Laura"
             , education = Identity (Degree "English")
             , addresses = Identity ["2 Edward Square"]
             , religion  = Identity (Just Catholic)
             }
paula2 :: Person2 Identity
paula2 = Person2
  (Identity "Paula")
  (Identity 35)
  (Identity ["La Mar 10"])
  (Identity Nothing)
  (Identity $ Degree "Arts")

-- Person2 is a subtype of Person therefore we can pack a Person2 as a Person
-- >>> Just castPerson21 = theSchema schemaPerson2 `isSubtypeOf` theSchema schemaPerson
-- >>> import Data.Aeson
-- >>> encode (castPerson21 $ pack schemaPerson2 pepe2)
-- "{\"education\":{\"PhD\":\"Computer Science\"},\"addresses\":[\"2 Edward Square\",\"La Mar 10\"],\"age\":38,\"name\":\"Pepe\"}"


-- We can also unpack a Person from a Person2
-- >>> Just castPerson21 = theSchema schemaPerson2 `isSubtypeOf` theSchema schemaPerson
-- >>> import Text.Pretty.Simple
-- >>> pPrintNoColor $ unpack schemaPerson (castPerson21 $ pack schemaPerson2 pepe2)
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

-- Unpacking a subtype doesn't even need casting
-- >>> import Text.Pretty.Simple
-- >>> pPrintNoColor $ unpack schemaPerson (pack schemaPerson2 pepe2)
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
