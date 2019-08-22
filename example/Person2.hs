{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}

module Person2 where

import           Data.Barbie
import           Data.Functor.Identity
import           Data.Generics.Labels  ()
import           Data.String
import           GHC.Exts (IsList(..))
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

instance HasSchema Religion where
  schema = enum (fromString . show) (fromList enumerate)

enumerate :: (Bounded a, Enum a) => [a]
enumerate = [minBound ..]

instance HasSchema (Person2 Identity) where
  schema = record $
            Person2 (required "name")
                    (required "age")
                    (required "addresses")
                    (optional "religion")
                    (required "education")

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

-- Person2 is a subtype of Person therefore we can encode a Person2 as a Person
-- >>> import qualified Data.ByteString.Lazy.Char8 as B
-- >>> import qualified Data.Aeson.Encode.Pretty
-- >>> coerce21 = coerce @(Person2 Identity) @(Person Identity)
-- >>> B.putStrLn $ encodePretty $ coerce21 $ encode pepe2
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


-- We can also upgrade a Person into a Person2, because the new field is optional
-- >>> import Text.Pretty.Simple
-- >>> coerce12 = coerce @(Person Identity) @(Person2 Identity)
-- >>> pPrintNoColor $ decode @(Person Identity) (coerce12 $ encode pepe)
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

