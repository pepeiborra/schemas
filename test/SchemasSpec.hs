{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module SchemasSpec where

import Control.Exception
import qualified Data.Aeson as A
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
      Record [makeField "a" Prim Nothing, makeField "def" Prim Nothing]
        `shouldBeSubtypeOf` Record [makeField "def" Prim Nothing]
      Record [makeField "a" Prim (Just False), makeField "def" Prim Nothing]
        `shouldBeSubtypeOf` Record [makeField "def" Prim Nothing]
    it "subtypes cannot turn a Required makeField into Optional" $ do
      Record [makeField "a" Prim (Just False)]
        `shouldNotBeSubtypeOf` Record [makeField "a" Prim Nothing]
    it "subtypes can turn an Optional makeField into Required" $ do
      Record [makeField "a" Prim Nothing]
        `shouldBeSubtypeOf` Record [makeField "a" Prim (Just False)]
    it "subtypes can relax the type of a field" $ do
      Record [makeField "a" (Array Prim) Nothing]
        `shouldBeSubtypeOf` Record [makeField "a" Prim Nothing]
    it "subtypes cannot remove Required fields" $ do
      Record [makeField "def" Prim Nothing] `shouldNotBeSubtypeOf` Record
        [makeField "def" Prim Nothing, makeField "a" Prim Nothing]
    it "subtypes can remove Optional fields" $ do
      Record [makeField "def" Prim Nothing] `shouldBeSubtypeOf` Record
        [makeField "def" Prim Nothing, makeField "a" Prim (Just False)]
    it "subtypes can add enum choices" $ do
      Enum ["A", "def"] `shouldBeSubtypeOf` Enum ["def"]
    it "subtypes cannot remove enum choices" $ do
      Enum ["def"] `shouldNotBeSubtypeOf` Enum ["A"]
    it "subtypes can add constructors" $ do
      Union [constructor' "A" Prim, constructor' "def" Empty]
        `shouldBeSubtypeOf` Union [constructor' "def" Empty]
    it "subtypes cannot remove constructors" $ do
      Union [constructor' "def" Empty]
        `shouldNotBeSubtypeOf` Union [constructor' "A" (Prim)]
    it "subtypes can expand an array" $ do
      Array Prim `shouldBeSubtypeOf` Prim
    it "subtypes cannot drop an array" $ do
      Prim `shouldNotBeSubtypeOf` Array Prim
  describe "examples" $ do
    describe "Schemas" $ do
      prop "finite(schema @Schema) is a supertype of (schema @Schema)" $ \(SmallNatural n) ->
        theSchema @Schema `isSubtypeOf` finite n (theSchema @Schema) `shouldSatisfy` isJust
    describe "Person" $ do
      it "decode is the inverse of encode (applicative)" $ do
        decode (encode pepe) `shouldBe` Right pepe
    describe "Person2" $ do
      it "Person2 < Person" $ do
        theSchema @Person2
          `isSubtypeOf`   theSchema @Person
          `shouldSatisfy` isJust
      it "pepe2 `as` Person" $ do
        let encoder = encodeTo (theSchema @Person)
        encoder `shouldSatisfy` isJust
        decode (fromJust encoder pepe2) `shouldBe` Right pepe
      it "pepe `as` Person2" $ do
        let decoder = decodeFrom (theSchema @Person)
        decoder `shouldSatisfy` isJust
        fromJust decoder (encode pepe) `shouldBe` Right pepe2
      it "Person < Person2" $ do
        theSchema @Person
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
