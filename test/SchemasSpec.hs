{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module SchemasSpec where

import Control.Exception
import qualified Data.Aeson as A
import Data.Either
import Data.Maybe
import Generators
import Person
import Person2
import Person3
import Schemas
import System.Timeout
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Show.Functions ()

spec :: Spec
spec = do
  describe "encoding" $ do
    prop "is the inverse of decoding" $ \(sc :: Schema) ->
      decode (encode sc) ==  Right sc
  describe "versions" $ do
    prop "eliminates AllOf" $ \sc -> all (not . hasAllOf) (versions sc)
  describe "finite" $ do
    it "is reflexive (in absence of OneOf)" $ forAll (sized genSchema `suchThat` (not . hasOneOf)) $ \sc ->
      sc `shouldBeSubtypeOf` sc
    it "always produces a supertype (in absence of OneOf)" $
      forAll (sized genSchema `suchThat` (not . hasOneOf)) $ \sc ->
      forAll arbitrary $ \(SmallNatural size) ->
      all (\sc -> isRight $ isSubtypeOf primValidators sc (finite size sc)) (versions sc)
  describe "isSubtypeOf" $ do
    it "subtypes can add fields" $ do
      Record [makeField "a" prim True, makeField "def" prim True]
        `shouldBeSubtypeOf` Record [makeField "def" prim True]
      Record [makeField "a" prim False, makeField "def" prim True]
        `shouldBeSubtypeOf` Record [makeField "def" prim True]
    it "subtypes cannot turn a Required makeField into Optional" $ do
      Record [makeField "a" prim False]
        `shouldNotBeSubtypeOf` Record [makeField "a" prim True]
    it "subtypes can turn an Optional makeField into Required" $ do
      Record [makeField "a" prim True]
        `shouldBeSubtypeOf` Record [makeField "a" prim False]
    it "subtypes can relax the type of a field" $ do
      Record [makeField "a" (Array prim) True]
        `shouldBeSubtypeOf` Record [makeField "a" prim True]
    it "subtypes cannot remove Required fields" $ do
      Record [makeField "def" prim True] `shouldNotBeSubtypeOf` Record
        [makeField "def" prim True, makeField "a" prim True]
    it "subtypes can remove Optional fields" $ do
      Record [makeField "def" prim True] `shouldBeSubtypeOf` Record
        [makeField "def" prim True, makeField "a" prim (False)]
    it "subtypes can add enum choices" $ do
      Enum ["A", "def"] `shouldBeSubtypeOf` Enum ["def"]
    it "subtypes cannot remove enum choices" $ do
      Enum ["def"] `shouldNotBeSubtypeOf` Enum ["A"]
    it "subtypes can remove constructors" $ do
      Union [constructor' "B" Empty]
        `shouldBeSubtypeOf` Union [constructor' "A" Empty, constructor' "B" Empty]
    it "subtypes cannot add constructors" $ do
      Union [constructor' "A" prim, constructor' "B" Empty]
        `shouldNotBeSubtypeOf` Union [constructor' "A" (prim)]
    it "subtypes can expand an array" $ do
      Array prim `shouldBeSubtypeOf` prim
    it "subtypes cannot drop an array" $ do
      prim `shouldNotBeSubtypeOf` Array prim
  describe "examples" $ do
    describe "Schemas" $ do
      prop "finite(schema @Schema) is a supertype of (schema @Schema)" $ \(SmallNatural n) ->
        theSchema @Schema `shouldBeSubtypeOf` finite n (theSchema @Schema)
    describe "Person" $ do
      it "decode is the inverse of encode (applicative)" $ do
        decode (encode pepe) `shouldBe` Right pepe
    describe "Person2" $ do
      it "Person2 < Person" $ do
        theSchema @Person2 `shouldBeSubtypeOf`   theSchema @Person
      it "pepe2 `as` Person" $ do
        let encoder = encodeTo (theSchema @Person)
        encoder `shouldSatisfy` isJust
        decode (fromJust encoder pepe2) `shouldBe` Right pepe
      it "pepe `as` Person2" $ do
        let decoder = decodeFrom (theSchema @Person)
        decoder `shouldSatisfy` isJust
        fromJust decoder (encode pepe) `shouldBe` Right pepe2
      it "Person < Person2" $ do
        theSchema @Person `shouldBeSubtypeOf`   theSchema @Person2
    describe "Person3" $ do
      it "finiteEncode works as expected" $ shouldLoop $ evaluate $ A.encode
        (finiteEncode 4 laura3)

shouldBeSubtypeOf :: Schema -> Schema -> Expectation
shouldBeSubtypeOf a b = case isSubtypeOf primValidators a b of
  Right _ -> pure ()
  _       -> expectationFailure $ show a <> " should be a subtype of " <> show b

shouldNotBeSubtypeOf :: Schema -> Schema -> Expectation
shouldNotBeSubtypeOf a b = case isSubtypeOf primValidators a b of
  Right _  -> expectationFailure $ show a <> " should not be a subtype of " <> show b
  _ -> pure ()

shouldLoop :: (Show a, Eq a) => IO a -> Expectation
shouldLoop act = timeout 1000000 act `shouldReturn` Nothing

shouldNotLoop :: (Show a, Eq a) => IO a -> Expectation
shouldNotLoop act = timeout 1000000 act `shouldNotReturn` Nothing

makeField :: a -> Schema -> Bool -> (a, Field)
makeField n t isReq = (n, Field t isReq)

constructor' :: a -> b -> (a, b)
constructor' n t = (n, t)

prim = Prim "A"

primValidators = validatorsFor @(Schema, Double, Int, Bool)
