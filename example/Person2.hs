{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
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
import           Data.Maybe
import           Data.String
import           GHC.Exts (IsList(..))
import           GHC.Generics
import qualified Generics.SOP as SOP
import           Person
import           Schemas

-- | The v2 of the Person schema adds a new optional field 'religion'
--   and renames 'studies' to 'education'
data Person2 = Person2
  { name      :: String
  , age       :: Maybe Int
  , addresses :: [String]
  , religion  :: (Maybe Religion)  -- new
  , education :: NonEmpty Education         -- renamed
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

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
      <$> field "name" Person2.name
      <*> (   optField "age" Person2.age
          <|> fieldWith (dimap (fromMaybe (-1)) Just schema) "age" Person2.age
          )
      <*> field "addresses" Person2.addresses
      <*> optField "religion" Person2.religion
      <*> (field "education" Person2.education <|> (NE.:| []) <$> field
            "studies"
            (NE.head . Person2.education)
          )

pepe2 :: Person2
pepe2 = Person2 "Pepe"
                (Just 38)
                ["2 Edward Square", "La Mar 10"]
                Nothing
                [PhD "Computer Science", Degree "Engineering" ]

-- Person2 can be encoded in multiple ways, so the canonic encoding includes all ways
-- >>> import qualified Data.ByteString.Lazy.Char8 as B
-- >>> import Data.Aeson.Encode.Pretty
-- >>> import Data.Either
-- >>> B.putStrLn $ encodePretty $ fromRight undefined (encodeTo (theSchema @Person2)) pepe2
-- {
--     "education": [
--         {
--             "PhD": "Computer Science"
--         },
--         {
--             "Degree": "Engineering"
--         }
--     ],
--     "addresses": [
--         "2 Edward Square",
--         "La Mar 10"
--     ],
--     "age": 38,
--     "name": "Pepe"
-- }
-- >>> import qualified Data.ByteString.Lazy.Char8 as B
-- >>> import Data.Aeson.Encode.Pretty
-- >>> B.putStrLn $ encodePretty $ encode pepe2
-- {
--     "education": [
--         {
--             "PhD": "Computer Science"
--         },
--         {
--             "Degree": "Engineering"
--         }
--     ],
--     "addresses": [
--         "2 Edward Square",
--         "La Mar 10"
--     ],
--     "age": 38,
--     "name": "Pepe"
-- }

-- Person2 is a subtype of Person therefore we can encode a Person2 as a Person
-- >>> import qualified Data.ByteString.Lazy.Char8 as B
-- >>> import Data.Aeson.Encode.Pretty
-- >>> B.putStrLn $ encodePretty $ fromRight undefined (encodeTo (theSchema @Person)) pepe2
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
-- >>> pPrintNoColor $ fromRight undefined (decodeFrom @Person2 (theSchema @Person)) (encode pepe)
-- Right 
--     ( Person2 
--         { name = "Pepe" 
--         , age = Just 38
--         , addresses = 
--             [ "2 Edward Square" 
--             , "La Mar 10" 
--             ] 
--         , religion = Nothing
--         , education = PhD { unPhD = "Computer Science" } :| []
--         } 
--     )

-- >>> B.putStrLn $ encodePretty $ encode (theSchema @Person2)
-- {
--     "Record": {
--         "education": {
--             "schema": {
--                 "Array": {
--                     "Union": [
--                         {
--                             "schema": {
--                                 "Prim": "String"
--                             },
--                             "constructor": "Degree"
--                         },
--                         {
--                             "schema": {
--                                 "Prim": "String"
--                             },
--                             "constructor": "PhD"
--                         },
--                         {
--                             "schema": {
--                                 "Record": {}
--                             },
--                             "constructor": "NoEducation"
--                         }
--                     ]
--                 }
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
--                 "Prim": "Integer"
--             },
--             "isRequired": false
--         },
--         "name": {
--             "schema": {
--                 "Prim": "String"
--             }
--         }
--     }
-- }

-- >>> import Data.Aeson.Encode.Pretty
-- >>> mapM_ (B.putStrLn . encodePretty . encode) (extractSchema $ schema @Person2)
-- {
--     "Record": {
--         "education": {
--             "schema": {
--                 "Array": {
--                     "Union": [
--                         {
--                             "schema": {
--                                 "Prim": "String"
--                             },
--                             "constructor": "Degree"
--                         },
--                         {
--                             "schema": {
--                                 "Prim": "String"
--                             },
--                             "constructor": "PhD"
--                         },
--                         {
--                             "schema": {
--                                 "Record": {}
--                             },
--                             "constructor": "NoEducation"
--                         }
--                     ]
--                 }
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
--                 "Prim": "Integer"
--             },
--             "isRequired": false
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
--                 "Prim": "Integer"
--             },
--             "isRequired": false
--         },
--         "studies": {
--             "schema": {
--                 "Union": [
--                     {
--                         "schema": {
--                             "Prim": "String"
--                         },
--                         "constructor": "Degree"
--                     },
--                     {
--                         "schema": {
--                             "Prim": "String"
--                         },
--                         "constructor": "PhD"
--                     },
--                     {
--                         "schema": {
--                             "Record": {}
--                         },
--                         "constructor": "NoEducation"
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
-- {
--     "Record": {
--         "education": {
--             "schema": {
--                 "Array": {
--                     "Union": [
--                         {
--                             "schema": {
--                                 "Prim": "String"
--                             },
--                             "constructor": "Degree"
--                         },
--                         {
--                             "schema": {
--                                 "Prim": "String"
--                             },
--                             "constructor": "PhD"
--                         },
--                         {
--                             "schema": {
--                                 "Record": {}
--                             },
--                             "constructor": "NoEducation"
--                         }
--                     ]
--                 }
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
--                 "Prim": "Integer"
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
--                 "Prim": "Integer"
--             }
--         },
--         "studies": {
--             "schema": {
--                 "Union": [
--                     {
--                         "schema": {
--                             "Prim": "String"
--                         },
--                         "constructor": "Degree"
--                     },
--                     {
--                         "schema": {
--                             "Prim": "String"
--                         },
--                         "constructor": "PhD"
--                     },
--                     {
--                         "schema": {
--                             "Record": {}
--                         },
--                         "constructor": "NoEducation"
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
