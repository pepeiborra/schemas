{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module SchemasSpec where

import Control.Exception
import qualified Data.Aeson as A
import Data.Functor.Identity
import Data.Maybe
import Data.Text
import Generators()
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
    prop "always produces a subtype" $ \(sc :: Schema) size ->
      finite size sc `isSubtypeOf` sc `shouldSatisfy` isJust
  describe "isSubtypeOf" $ do
    it "subtypes can add fields" $ do
      Record [field' "a" Number Nothing, field' "def" Number Nothing]
        `shouldBeSubtypeOf` Record [field' "def" Number Nothing]
      Record [field' "a" Number (Just False), field' "def" Number Nothing]
        `shouldBeSubtypeOf` Record [field' "def" Number Nothing]
    it "subtypes cannot turn a Required field' into Optional" $ do
      Record [field' "a" Number (Just False)]
        `shouldNotBeSubtypeOf` Record [field' "a" Number Nothing]
    it "subtypes can turn an Optional field' into Required" $ do
      Record [field' "a" Number Nothing]
        `shouldBeSubtypeOf` Record [field' "a" Number (Just False)]
    it "subtypes can relax the type of a field" $ do
      Record [field' "a" (Array Number) Nothing]
        `shouldBeSubtypeOf` Record [field' "a" Number Nothing]
    it "subtypes cannot remove Required fields" $ do
      Record [field' "def" Number Nothing] `shouldNotBeSubtypeOf` Record
        [field' "def" Number Nothing, field' "a" Number Nothing]
    it "subtypes can remove Optional fields" $ do
      Record [field' "def" Number Nothing] `shouldBeSubtypeOf` Record
        [field' "def" Number Nothing, field' "a" Number (Just False)]
    it "subtypes can add enum choices" $ do
      Enum ["A", "def"] `shouldBeSubtypeOf` Enum ["def"]
    it "subtypes cannot remove enum choices" $ do
      Enum ["def"] `shouldNotBeSubtypeOf` Enum ["A"]
    it "subtypes can add constructors" $ do
      Union [constructor' "A" (Just String), constructor' "def" Nothing]
        `shouldBeSubtypeOf` Union [constructor' "def" Nothing]
    it "subtypes cannot remove constructors" $ do
      Union [constructor' "def" Nothing]
        `shouldNotBeSubtypeOf` Union [constructor' "A" (Just String)]
    it "subtypes can expand an array" $ do
      Array String `shouldBeSubtypeOf` String
    it "subtypes cannot drop an array" $ do
      String `shouldNotBeSubtypeOf` Array String
  describe "examples" $ do
    describe "Person" $ do
      it "decode is the inverse of encode" $ do
        decode (encode pepe) `shouldBe` Right pepe
    describe "Person2" $ do
      it "Person2 is a subtype of Person" $ do
        theSchema @(Person2 Identity)
          `isSubtypeOf`   theSchema @(Person Identity)
          `shouldSatisfy` isJust
      it "Person is a subtype of Person2" $ do
        theSchema @(Person Identity)
          `isSubtypeOf`   theSchema @(Person2 Identity)
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

field' :: Text -> Schema -> Maybe Bool -> Field Identity
field' n t req = Field (Identity n) (Identity t) (Identity req)

constructor' :: Text -> Maybe Schema -> Constructor Identity
constructor' n t = Constructor (Identity n) (Identity t)
