{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Schemas.OpenApi2Spec where


import qualified Data.HashMap.Strict        as Map
import           Person
import           Person2
import           Schemas
import           Schemas.OpenApi2
import           Test.Hspec

spec :: Spec
spec = do
  describe "toOpenApi2Document" $ do
    it "works for Person" $ do
      let document = toOpenApi2Document defaultOptions [("Person", theSchema @Person)]
      Map.keys (definitions document) `shouldContain` ["Person"]
      Map.keys (failures document) `shouldNotContain` ["Person"]
    it "does Nothing for Person2" $ do
      let document = toOpenApi2Document defaultOptions [("Person2", theSchema @Person2)]
      Map.keys (definitions document) `shouldNotContain` ["Person2"]
      Map.keys (failures document) `shouldContain` ["Person2"]
