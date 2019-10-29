{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Schemas.SOPSpec where

import           Control.Exception
import           Data.Either
import qualified Data.List.NonEmpty       as NE
import           Person
import           Person2
import           Person4
import           Schemas
import           Schemas.SOP
import           SchemasSpec
import           Test.Hspec

spec :: Spec
spec = do
  describe "Generics" $ do
    describe "Person" $
      specExample pepe
    describe "Person2" $
      specExample pepe2
    -- Equality across recursive schemas (Person3) is not yet supported
    -- describe "Person3" $
    --   specExample pepe3
    describe "Person4" $
      specExample pepe4

specExample :: forall a . (HasGenericSchema a, HasSchema a, Eq a, Show a) => a -> Spec
specExample ex = do
  let genSchemas = extractSchema genSchemaTyped
      genSchema  = NE.head genSchemas
      genSchemaTyped = gSchema defOptions

  it "generic schemas are unitary" $
    length genSchemas `shouldBe` 1
  -- it "generic schema is included in handcrafted one" $
  --    NE.toList (extractSchema (schema @a)) `shouldContain` NE.toList genSchemas
  it "can encode to generic schema" $ do
     let encoder = encodeTo genSchema
     shouldNotLoop $ evaluate encoder
     encoder `shouldSatisfy` isRight
     fromRight undefined encoder ex `shouldBe` encodeWith genSchemaTyped ex
  it "can decode from generic schema" $ do
     let decoder = decodeFrom genSchema
         encoded = encode ex
         decoded = fromRight undefined decoder encoded
         decodedG = decodeWith genSchemaTyped encoded
     shouldNotLoop $ evaluate decoder
     shouldNotLoop $ evaluate encoded
     shouldNotLoop $ evaluate decoded
     shouldNotLoop $ evaluate decodedG
     decoder `shouldSatisfy` isRight
     decodedG `shouldBe` decoded
