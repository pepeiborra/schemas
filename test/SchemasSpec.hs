{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module SchemasSpec where

import Control.Exception
import qualified Data.Aeson as A
import Data.Functor.Identity
import Data.Maybe
import Person
import Person2
import Person3
import Schemas
import System.Timeout
import Test.Hspec
import Text.Show.Functions ()

spec :: Spec
spec = do
  describe "isSubtypeOf" $ do
    it "subtypes can add fields" $ do
      Record [field "a" Number True , field "def" Number True] `shouldBeSubtypeOf` Record [field "def" Number True]
      Record [field "a" Number False, field "def" Number True] `shouldBeSubtypeOf` Record [field "def" Number True]
    it "subtypes cannot turn a Required field into Optional" $ do
      Record [field "a"  Number  False] `shouldNotBeSubtypeOf` Record [field "a" Number True ]
    it "subtypes can turn an Optional field into Required" $ do
      Record [field "a"  Number  True] `shouldBeSubtypeOf` Record [field "a" Number False]
    it "subtypes can relax the type of a field" $ do
      Record [field "a"  (Array Number)  True] `shouldBeSubtypeOf` Record [field "a" Number True]
    it "subtypes cannot remove Required fields" $ do
      Record [field "def" Number True] `shouldNotBeSubtypeOf` Record [field "def" Number True, field "a" Number True ]
    it "subtypes can remove Optional fields" $ do
      Record [field "def" Number True] `shouldBeSubtypeOf` Record [field "def" Number True, field "a" Number False]
    it "subtypes can add enum choices" $ do
      Enum ["A", "def"] `shouldBeSubtypeOf` Enum ["def"]
    it "subtypes cannot remove enum choices" $ do
      Enum ["def"] `shouldNotBeSubtypeOf` Enum ["A"]
    it "subtypes can add constructors" $ do
      Union [constructor "A" (Just String), constructor "def" Nothing] `shouldBeSubtypeOf` Union [constructor "def" Nothing]
    it "subtypes cannot remove constructors" $ do
      Union [constructor "def" Nothing] `shouldNotBeSubtypeOf` Union [constructor "A" (Just String)]
    it "subtypes can expand an array" $ do
      Array String `shouldBeSubtypeOf` String
    it "subtypes cannot drop an array" $ do
      String `shouldNotBeSubtypeOf` Array String
  describe "examples" $ do
    describe "Person"  $ do
      it "decode is the inverse of encode" $ do
        decode (encode pepe) `shouldBe` Right pepe
    describe "Person2" $ do
      it "Person2 is a subtype of Person" $ do
        theSchema @(Person2 Identity) `isSubtypeOf` theSchema @(Person Identity) `shouldSatisfy` isJust
      it "Person is a subtype of Person2" $ do
        theSchema @(Person Identity) `isSubtypeOf` theSchema @(Person2 Identity) `shouldSatisfy` isJust
    describe "Person3" $ do
      it "finiteEncode works as expected" $
        shouldNotLoop $ evaluate $ A.encode(finiteEncode 2 laura3)

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

field n t req = Field (Identity n) (Identity t) (Identity req)

constructor n t = Constructor (Identity n) (Identity t)
