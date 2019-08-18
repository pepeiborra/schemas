{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
module Example where

import           Data.Barbie
import           Data.Functor.Identity
import           Data.Generics.Labels  ()
import           Data.String
import           GHC.Generics
import           Schemas

data Religion = Catholic | Anglican | Muslim | Hindu
  deriving (Bounded, Enum, Eq, Show)

schemaReligion :: TypedSchema Religion
schemaReligion = enum (fromString . show) enumerate

data Education = NoEducation | Degree {unDegree :: String} | PhD {unPhD :: String}
  deriving (Generic, Show)

schemaEducation :: TypedSchema Education
schemaEducation = union'
  [Alt "NoEducation" #_NoEducation empty
  ,Alt "PhD" #_PhD string
  ,Alt "Degree" #_Degree string
  ]

data Person f = Person
  { name      :: f String
  , age       :: f Int
  , addresses :: f [String]
  , relatives :: f [Person Identity]
  , spouse    :: f (Maybe (Person Identity))
  , religion  :: f (Maybe Religion)
  , education :: f Education
  }
  deriving Generic
  deriving anyclass (FunctorB, ProductB, TraversableB)

deriving instance Show (Person Identity)


schemaPerson :: TypedSchema (Person Identity)
schemaPerson = record schema
 where
  schema :: RecordSchema Person
  schema = Person (Required "name" string)
                  (Required "age" int)
                  (Required "addresses" $ list string)
                  (Required "relatives" $ list schemaPerson)
                  (Optional "spouse" schemaPerson)
                  (Optional "religion" schemaReligion)
                  (Required "education" schemaEducation)

enumerate :: (Bounded a, Enum a) => [a]
enumerate = [minBound ..]

laura, paula, pepe :: Person Identity

pepe = Person
  (Identity "Pepe")
  (Identity 38)
  (Identity ["2 Edward Square", "La Mar 10"])
  (Identity [paula])
  (Identity $ Just laura)
  (Identity Nothing)
  (Identity $ PhD "Computer Science")

laura = pepe { name      = Identity "Laura"
             , spouse    = Identity (Just pepe)
             , education = Identity (Degree "English")
             , addresses = Identity ["2 Edward Square"]
             , relatives = Identity []
             , religion  = Identity (Just Catholic)
             }
paula = Person
  (Identity "paula")
  (Identity 35)
  (Identity ["La Mar 10"])
  (Identity [pepe])
  (Identity Nothing)
  (Identity Nothing)
  (Identity $ Degree "Arts")

-- >>> pack schemaPerson laura
{-
laura = Object
  (fromList
    [ ( "relatives"
      , Array
        [ Object
            (fromList
              [ ("relatives", Array [])
              , ("education", String "Arts")
              , ("addresses", Array [String "La Mar 10"])
              , ("age"      , Number 35.0)
              , ("name"     , String "paula")
              ]
            )
        ]
      )
    , ("education", String "English")
    , ("religion" , String "Catholic")
    , ("addresses", Array [String "La Mar 10"])
    , ("age"      , Number 38.0)
    , ("name"     , String "Laura")
    ]
  )
-}

-- >>> pack schemaPerson pepe
{-
pepe = Object
  (fromList
    [ ( "relatives"
      , Array
        [ Object
            (fromList
              [ ("relatives", Array [])
              , ("education", String "Arts")
              , ("addresses", Array [String "La Mar 10"])
              , ("age"      , Number 35.0)
              , ("name"     , String "paula")
              ]
            )
        ]
      )
    , ( "spouse"
      , Object
        (fromList
          [ ("relatives", Array [])
          , ("education", String "English")
          , ("religion" , String "Catholic")
          , ("addresses", Array [String "2 Edward Square"])
          , ("age"      , Number 38.0)
          , ("name"     , String "Laura")
          ]
        )
      )
    , ("education", String "Computer Science")
    , ("addresses", Array [String "2 Edward Square", String "La Mar 10"])
    , ("age"      , Number 38.0)
    , ("name"     , String "Pepe")
    ]
  )
-}

-- >>> unpack schemaPerson (pack schemaPerson pepe)-- <interactive>:11:2-45: error:
--     • No instance for (Show (Person Identity))
--         arising from a use of ‘print’
--     • In a stmt of an interactive GHCi command: print it