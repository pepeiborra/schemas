{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
module Person2 where

import           Control.Applicative
import           Data.Generics.Labels  ()
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
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
  , religion  :: (Maybe Religion)  -- new
  , education :: NonEmpty Education         -- renamed
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
      <*> (field "education" Person2.education <|> (NE.:| []) <$> field "studies" (NE.head . Person2.education))

pepe2 :: Person2
pepe2 = Person2 "Pepe"
                38
                ["2 Edward Square", "La Mar 10"]
                Nothing
                [PhD "Computer Science", Degree "Engineering" ]

-- Person2 can be encoded in multiple ways, so the canonic encoding includes all ways
-- >>> import qualified Data.ByteString.Lazy.Char8 as B
-- >>> import Data.Aeson.Encode.Pretty
-- >>> B.putStrLn $ encodePretty $ encodeTo (theSchema @Person2) pepe2
-- {
--     "#1": {
--         "education": {
--             "PhD": "Computer Science"
--         },
--         "addresses": [
--             "2 Edward Square",
--             "La Mar 10"
--         ],
--         "age": 38,
--         "name": "Pepe"
--     },
--     "#2": {
--         "addresses": [
--             "2 Edward Square",
--             "La Mar 10"
--         ],
--         "age": 38,
--         "studies": {
--             "PhD": "Computer Science"
--         },
--         "name": "Pepe"
--     }
-- }
-- >>> import qualified Data.ByteString.Lazy.Char8 as B
-- >>> import Data.Aeson.Encode.Pretty
-- >>> B.putStrLn $ encodePretty $ encode pepe2
-- {
--     "#1": {
--         "education": {
--             "PhD": "Computer Science"
--         },
--         "addresses": [
--             "2 Edward Square",
--             "La Mar 10"
--         ],
--         "age": 38,
--         "name": "Pepe"
--     },
--     "#2": {
--         "addresses": [
--             "2 Edward Square",
--             "La Mar 10"
--         ],
--         "age": 38,
--         "studies": {
--             "PhD": "Computer Science"
--         },
--         "name": "Pepe"
--     }
-- }

-- Person2 is a subtype of Person therefore we can encode a Person2 as a Person
-- >>> import qualified Data.ByteString.Lazy.Char8 as B
-- >>> import Data.Aeson.Encode.Pretty
-- >>> B.putStrLn $ encodePretty $ encodeTo (theSchema @Person) <*> pure pepe2
-- {
--     "addresses": [
--         "2 Edward Square",
--         "La Mar 10"
--     ],
--     "age": 38,
--     "studies": {
--         "PhD": "Computer Science"
--     },
--     "name": "Pepe"
-- }


-- We can also upgrade a Person into a Person2, because the new field is optional
-- >>> import Text.Pretty.Simple
-- >>> pPrintNoColor $ decodeFrom @Person2 (theSchema @Person) <*> pure (encode pepe)
-- Just
--     ( Right
--         ( Person2
--             { name = "Pepe"
--             , age = 38
--             , addresses =
--                 [ "2 Edward Square"
--                 , "La Mar 10"
--                 ]
--             , religion = Nothing
--             , education = PhD { unPhD = "Computer Science" }
--             }
--         )
--     )

-- >>> B.putStrLn $ encodePretty $ encode (theSchema @Person2)
-- {
--     "AllOf": [
--         {
--             "Record": {
--                 "education": {
--                     "schema": {
--                         "Union": [
--                             {
--                                 "schema": {
--                                     "Empty": {}
--                                 },
--                                 "constructor": "NoEducation"
--                             },
--                             {
--                                 "schema": {
--                                     "Prim": "String"
--                                 },
--                                 "constructor": "PhD"
--                             },
--                             {
--                                 "schema": {
--                                     "Prim": "String"
--                                 },
--                                 "constructor": "Degree"
--                             }
--                         ]
--                     }
--                 },
--                 "religion": {
--                     "schema": {
--                         "Enum": [
--                             "Catholic",
--                             "Anglican",
--                             "Muslim",
--                             "Hindu"
--                         ]
--                     },
--                     "isRequired": false
--                 },
--                 "addresses": {
--                     "schema": {
--                         "Array": {
--                             "Prim": "String"
--                         }
--                     }
--                 },
--                 "age": {
--                     "schema": {
--                         "Prim": "Int"
--                     }
--                 },
--                 "name": {
--                     "schema": {
--                         "Prim": "String"
--                     }
--                 }
--             }
--         },
--         {
--             "Record": {
--                 "religion": {
--                     "schema": {
--                         "Enum": [
--                             "Catholic",
--                             "Anglican",
--                             "Muslim",
--                             "Hindu"
--                         ]
--                     },
--                     "isRequired": false
--                 },
--                 "addresses": {
--                     "schema": {
--                         "Array": {
--                             "Prim": "String"
--                         }
--                     }
--                 },
--                 "age": {
--                     "schema": {
--                         "Prim": "Int"
--                     }
--                 },
--                 "studies": {
--                     "schema": {
--                         "Union": [
--                             {
--                                 "schema": {
--                                     "Empty": {}
--                                 },
--                                 "constructor": "NoEducation"
--                             },
--                             {
--                                 "schema": {
--                                     "Prim": "String"
--                                 },
--                                 "constructor": "PhD"
--                             },
--                             {
--                                 "schema": {
--                                     "Prim": "String"
--                                 },
--                                 "constructor": "Degree"
--                             }
--                         ]
--                     }
--                 },
--                 "name": {
--                     "schema": {
--                         "Prim": "String"
--                     }
--                 }
--             }
--         }
--     ]
-- }

-- >>> import qualified Data.ByteString.Lazy.Char8 as B
-- {
--     "Record": {
--         "education": {
--             "schema": {
--                 "Union": [
--                     {
--                         "schema": {
--                             "Empty": {}
--                         },
--                         "constructor": "NoEducation"
--                     },
--                     {
--                         "schema": {
--                             "Prim": "String"
--                         },
--                         "constructor": "PhD"
--                     },
--                     {
--                         "schema": {
--                             "Prim": "String"
--                         },
--                         "constructor": "Degree"
--                     }
--                 ]
--             }
--         },
--         "religion": {
--             "schema": {
--                 "Enum": [
--                     "Catholic",
--                     "Anglican",
--                     "Muslim",
--                     "Hindu"
--                 ]
--             },
--             "isRequired": false
--         },
--         "addresses": {
--             "schema": {
--                 "Array": {
--                     "Prim": "String"
--                 }
--             }
--         },
--         "age": {
--             "schema": {
--                 "Prim": "Int"
--             }
--         },
--         "name": {
--             "schema": {
--                 "Prim": "String"
--             }
--         }
--     }
-- }
-- {
--     "Record": {
--         "religion": {
--             "schema": {
--                 "Enum": [
--                     "Catholic",
--                     "Anglican",
--                     "Muslim",
--                     "Hindu"
--                 ]
--             },
--             "isRequired": false
--         },
--         "addresses": {
--             "schema": {
--                 "Array": {
--                     "Prim": "String"
--                 }
--             }
--         },
--         "age": {
--             "schema": {
--                 "Prim": "Int"
--             }
--         },
--         "studies": {
--             "schema": {
--                 "Union": [
--                     {
--                         "schema": {
--                             "Empty": {}
--                         },
--                         "constructor": "NoEducation"
--                     },
--                     {
--                         "schema": {
--                             "Prim": "String"
--                         },
--                         "constructor": "PhD"
--                     },
--                     {
--                         "schema": {
--                             "Prim": "String"
--                         },
--                         "constructor": "Degree"
--                     }
--                 ]
--             }
--         },
--         "name": {
--             "schema": {
--                 "Prim": "String"
--             }
--         }
--     }
-- }
-- >>> import Data.Aeson.Encode.Pretty
-- >>> mapM_ (B.putStrLn . encodePretty . encode) (versions $ theSchema @Person2)
-- {
--     "Record": {
--         "education": {
--             "schema": {
--                 "Union": [
--                     {
--                         "schema": {
--                             "Empty": {}
--                         },
--                         "constructor": "NoEducation"
--                     },
--                     {
--                         "schema": {
--                             "Prim": "String"
--                         },
--                         "constructor": "PhD"
--                     },
--                     {
--                         "schema": {
--                             "Prim": "String"
--                         },
--                         "constructor": "Degree"
--                     }
--                 ]
--             }
--         },
--         "religion": {
--             "schema": {
--                 "Enum": [
--                     "Catholic",
--                     "Anglican",
--                     "Muslim",
--                     "Hindu"
--                 ]
--             },
--             "isRequired": false
--         },
--         "addresses": {
--             "schema": {
--                 "Array": {
--                     "Prim": "String"
--                 }
--             }
--         },
--         "age": {
--             "schema": {
--                 "Prim": "Int"
--             }
--         },
--         "name": {
--             "schema": {
--                 "Prim": "String"
--             }
--         }
--     }
-- }
-- {
--     "Record": {
--         "religion": {
--             "schema": {
--                 "Enum": [
--                     "Catholic",
--                     "Anglican",
--                     "Muslim",
--                     "Hindu"
--                 ]
--             },
--             "isRequired": false
--         },
--         "addresses": {
--             "schema": {
--                 "Array": {
--                     "Prim": "String"
--                 }
--             }
--         },
--         "age": {
--             "schema": {
--                 "Prim": "Int"
--             }
--         },
--         "studies": {
--             "schema": {
--                 "Union": [
--                     {
--                         "schema": {
--                             "Empty": {}
--                         },
--                         "constructor": "NoEducation"
--                     },
--                     {
--                         "schema": {
--                             "Prim": "String"
--                         },
--                         "constructor": "PhD"
--                     },
--                     {
--                         "schema": {
--                             "Prim": "String"
--                         },
--                         "constructor": "Degree"
--                     }
--                 ]
--             }
--         },
--         "name": {
--             "schema": {
--                 "Prim": "String"
--             }
--         }
--     }
-- }
