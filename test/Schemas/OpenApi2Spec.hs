{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Schemas.OpenApi2Spec where


import qualified Data.HashMap.Strict        as Map
import           Person
import           Person2
import           Schemas
import           Schemas.OpenApi2
import           SchemasSpec
import           Test.Hspec

spec :: Spec
spec = do
  let personDocument = toOpenApi2Document defaultOptions [("Person", schemaFor @Person)]
  describe "OpenApi2 schema" $
    schemaSpec schema (definitions personDocument Map.! "Person")
  describe "toOpenApi2Document" $ do
    it "works for Person" $ do
      Map.keys (definitions personDocument) `shouldContain` ["Person"]
      Map.keys (failures personDocument) `shouldNotContain` ["Person"]
    it "works for Person2" $ do
      let document = toOpenApi2Document defaultOptions [("Person2", schemaFor @Person2)]
      Map.keys (definitions document) `shouldContain` ["Person2"]
      Map.keys (failures document) `shouldNotContain` ["Person2"]
