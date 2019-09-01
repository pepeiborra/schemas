{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module SchemasSpec where

import Control.Exception
import qualified Data.Aeson as A
import Data.Functor.Identity
import Data.Maybe
import Generators
import Person
import Person2
import Person3
import Schemas
import System.Timeout
import Test.Hspec
import Test.Hspec.QuickCheck
import Text.Show.Functions ()

spec :: Spec
spec = do
  describe "encoding" $ do
    prop "is the inverse of decoding" $ \(sc :: Schema) ->
      decode (encode sc) `shouldBe` Right sc
  describe "finite" $ do
    prop "always produces a supertype" $ \(sc :: Schema) (SmallNatural size) ->
      sc `isSubtypeOf` finite size sc `shouldSatisfy` isJust
  describe "isSubtypeOf" $ do
    it "subtypes can add fields" $ do
      Record [makeField "a" Number Nothing, makeField "def" Number Nothing]
        `shouldBeSubtypeOf` Record [makeField "def" Number Nothing]
      Record [makeField "a" Number (Just False), makeField "def" Number Nothing]
        `shouldBeSubtypeOf` Record [makeField "def" Number Nothing]
    it "subtypes cannot turn a Required makeField into Optional" $ do
      Record [makeField "a" Number (Just False)]
        `shouldNotBeSubtypeOf` Record [makeField "a" Number Nothing]
    it "subtypes can turn an Optional makeField into Required" $ do
      Record [makeField "a" Number Nothing]
        `shouldBeSubtypeOf` Record [makeField "a" Number (Just False)]
    it "subtypes can relax the type of a field" $ do
      Record [makeField "a" (Array Number) Nothing]
        `shouldBeSubtypeOf` Record [makeField "a" Number Nothing]
    it "subtypes cannot remove Required fields" $ do
      Record [makeField "def" Number Nothing] `shouldNotBeSubtypeOf` Record
        [makeField "def" Number Nothing, makeField "a" Number Nothing]
    it "subtypes can remove Optional fields" $ do
      Record [makeField "def" Number Nothing] `shouldBeSubtypeOf` Record
        [makeField "def" Number Nothing, makeField "a" Number (Just False)]
    it "subtypes can add enum choices" $ do
      Enum ["A", "def"] `shouldBeSubtypeOf` Enum ["def"]
    it "subtypes cannot remove enum choices" $ do
      Enum ["def"] `shouldNotBeSubtypeOf` Enum ["A"]
    it "subtypes can add constructors" $ do
      Union [constructor' "A" String, constructor' "def" Empty]
        `shouldBeSubtypeOf` Union [constructor' "def" Empty]
    it "subtypes cannot remove constructors" $ do
      Union [constructor' "def" Empty]
        `shouldNotBeSubtypeOf` Union [constructor' "A" (String)]
    it "subtypes can expand an array" $ do
      Array String `shouldBeSubtypeOf` String
    it "subtypes cannot drop an array" $ do
      String `shouldNotBeSubtypeOf` Array String
  describe "examples" $ do
    describe "Schemas" $ do
      prop "finite(schema @Schema) is a supertype of (schema @Schema)" $ \(SmallNatural n) ->
        theSchema @Schema `isSubtypeOf` finite n (theSchema @Schema) `shouldSatisfy` isJust
    describe "Person" $ do
      it "decode is the inverse of encode (HKT)" $ do
        decodeWith hktPersonSchema (encodeWith hktPersonSchema pepe) `shouldBe` Right pepe
      it "decode is the inverse of encode (applicative)" $ do
        decodeWith applicativePersonSchema (encodeWith applicativePersonSchema pepe) `shouldBe` Right pepe
      it "applicativePersonSchema is equal to the HKT schema" $ do
        extractSchema applicativePersonSchema `shouldBe` extractSchema hktPersonSchema
    describe "Person2" $ do
      it "Person2 < Person" $ do
        theSchema @Person2
          `isSubtypeOf`   theSchema @(Person Identity)
          `shouldSatisfy` isJust
      it "pepe2 `as` Person" $ do
        coerced <- maybe (fail "coerce") pure $ coerce @Person2 @(Person Identity) (encode pepe2)
        decode coerced `shouldBe` Right pepe
      it "pepe `as` Person2" $ do
        coerced <- maybe (fail "coerce") pure $ coerce @(Person Identity) @Person2 (encode pepe)
        decode coerced `shouldBe` Right pepe2
      it "Person < Person2" $ do
        theSchema @(Person Identity)
          `isSubtypeOf`   theSchema @Person2
          `shouldSatisfy` isJust
    describe "Person3" $ do
      it "finiteEncode works as expected" $ shouldNotLoop $ evaluate $ A.encode
        (finiteEncode 2 laura3)

shouldBeSubtypeOf :: Schema -> Schema -> Expectation
shouldBeSubtypeOf a b = case a `isSubtypeOf` b of
  Just _ -> pure ()
  Nothing -> expectationFailure $ show a <> " should be a subtype of " <> show b

shouldNotBeSubtypeOf :: Schema -> Schema -> Expectation
shouldNotBeSubtypeOf a b = case a `isSubtypeOf` b of
  Just _  -> expectationFailure $ show a <> " should not be a subtype of " <> show b
  Nothing -> pure ()

shouldNotLoop :: (Show a, Eq a) => IO a -> Expectation
shouldNotLoop act = timeout 1000000 act `shouldNotReturn` Nothing

makeField :: a -> Schema -> Maybe Bool -> (a, Field)
makeField n t isReq = (n, Field t isReq)

constructor' :: a -> b -> (a, b)
constructor' n t = (n, t)
