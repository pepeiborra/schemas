{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
module Person2 where

import           Control.Applicative
import           Data.Generics.Labels  ()
import           Data.String
import           GHC.Exts (IsList(..))
import           Person
import           Schemas

-- | The v2 of the Person schema adds a new optional field 'religion'
--   and renames 'studies' to 'education'
data Person2 = Person2
  { name      :: String
  , age       :: Int
  , addresses :: [String]
  , religion  :: (Maybe Religion)
  , education :: Education
  }
  deriving (Eq, Show)

data Religion = Catholic | Anglican | Muslim | Hindu
  deriving (Bounded, Enum, Eq, Show)

instance HasSchema Religion where
  schema = enum (fromString . show) (fromList enumerate)

enumerate :: (Bounded a, Enum a) => [a]
enumerate = [minBound ..]

instance HasSchema Person2 where
  schema =
    record
      $   Person2
      <$> field "name"      Person2.name
      <*> field "age"       Person2.age
      <*> field "addresses" Person2.addresses
      <*> optField "religion"  Person2.religion
      <*> (field "studies" Person2.education <|> field "education" Person2.education)

pepe2 :: Person2
pepe2 = Person2 "Pepe"
                38
                ["2 Edward Square", "La Mar 10"]
                Nothing
                (PhD "Computer Science")

-- Person2 can be encoded in multiple ways, so the canonic encoding includes all ways
-- >>> import qualified Data.ByteString.Lazy.Char8 as B
-- >>> import Data.Aeson.Encode.Pretty
-- >>> B.putStrLn $ encodePretty $ encode pepe2
-- {
--     "L": {
--         "addresses": [
--             "2 Edward Square",
--             "La Mar 10"
--         ],
--         "age": 38,
--         "studies": {
--             "PhD": "Computer Science"
--         },
--         "name": "Pepe"
--     },
--     "R": {
--         "education": {
--             "PhD": "Computer Science"
--         },
--         "addresses": [
--             "2 Edward Square",
--             "La Mar 10"
--         ],
--         "age": 38,
--         "name": "Pepe"
--     }
-- }

-- Person2 is a subtype of Person therefore we can encode a Person2 as a Person
-- >>> import qualified Data.ByteString.Lazy.Char8 as B
-- >>> import Data.Aeson.Encode.Pretty
-- >>> coerce21 = coerce @Person2 @(Person Identity)
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
-- >>> coerce12 = coerce @(Person Identity) @(Person2)
-- >>> pPrintNoColor $ decode @(Person Identity) (coerce12 $ encode pepe)
-- Right
--     ( Person
--         { name = "Pepe"
--         , age = 38
--         , addresses = Identity
--             [ "2 Edward Square"
--             , "La Mar 10"
--             ]
--         , education = Identity
--             ( PhD { unPhD = "Computer Science" } )
--         }
--     )

