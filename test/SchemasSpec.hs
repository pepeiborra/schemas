{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module SchemasSpec where

import Control.Exception
import qualified Data.Aeson as A
import Data.Either
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Generators
import Person
import Person2
import Person3
import Person4
import Schemas
import Schemas.Internal
import Schemas.Untyped (Validators)
import System.Timeout
import Test.Hspec
import Test.Hspec.Runner
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Show.Functions ()

main :: IO ()
main = hspecWith defaultConfig{configQuickCheckMaxSuccess = Just 10000} spec

spec :: Spec
spec = do
  describe "encode" $ do
    prop "is the inverse of decoding" $ \(sc :: Schema) ->
      decode (encode sc) ==  Right sc
  describe "encodeTo" $ do
    it "laziness delivers" $ do
      evaluate (fromRight undefined (encodeToWith (record $ Just <$> field "bottom" fromJust) (Record [makeField "bottom" prim True])) (Nothing :: Maybe Bool))
        `shouldThrow` \(_ :: SomeException) -> True
      fromRight undefined (encodeToWith (record $ Just <$> field "bottom" fromJust) (Record [])) (Nothing :: Maybe Bool)
        `shouldBe` A.Object []
    it "satisfies the spec" $ do
        let encoder = encodeTo (theSchema @Person)
            spec    = encodeToSpec (theSchema @Person)
        encoder `shouldSatisfy` isRight
        spec    `shouldSatisfy` isJust
        fromRight undefined encoder pepe `shouldBe` fromJust spec pepe
  describe "versions" $ do
    prop "eliminates AllOf" $ \sc -> all (not . hasAllOf) (versions sc)
  describe "finite" $ do
    it "is reflexive (in absence of OneOf)" $ forAll (sized genSchema `suchThat` (not . hasOneOf)) $ \sc ->
      sc `shouldBeSubtypeOf` sc
    it "is reflexive (corner  case)" $
      finiteCornerCase `shouldBeSubtypeOf` finiteCornerCase
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
      Record [makeField "a" prim True]
        `shouldBeSubtypeOf` Record [makeField "a" (Array prim) True]
    it "subtypes cannot remove Required fields" $ do
      Record [makeField "def" prim True] `shouldNotBeSubtypeOf` Record
        [makeField "def" prim True, makeField "a" prim True]
    it "subtypes can remove Optional fields" $ do
      Record [makeField "def" prim True] `shouldBeSubtypeOf` Record
        [makeField "def" prim True, makeField "a" prim (False)]
    it "subtypes can remove enum choices" $ do
      Enum ["def"] `shouldBeSubtypeOf` Enum ["A", "def"]
    it "subtypes cannot add enum choices" $ do
      Enum ["A", "def"] `shouldNotBeSubtypeOf` Enum ["def"]
    it "subtypes can remove constructors" $ do
      Union [constructor' "B" Empty]
        `shouldBeSubtypeOf` Union [constructor' "A" Empty, constructor' "B" Empty]
    it "subtypes cannot add constructors" $ do
      Union [constructor' "A" prim, constructor' "B" Empty]
        `shouldNotBeSubtypeOf` Union [constructor' "A" (prim)]
    it "subtypes can drop an array" $ do
      prim `shouldBeSubtypeOf` Array prim
    it "subtypes cannot introduce an array" $ do
      Array prim `shouldNotBeSubtypeOf` prim
  describe "HasSchema" $ do
    it "Left is a constructor of Either" $ do
      Union [constructor' "Left" Empty] `shouldBeSubtypeOf` theSchema @(Either () ())
    it "left is a constructor of Either too" $ do
      Union [constructor' "left" Empty] `shouldBeSubtypeOf` theSchema @(Either () ())
  describe "examples" $ do
    let   person4_v0 = NE.head $ versions $ theSchema @Person4
          person2_v0 = NE.head $ versions $ theSchema @Person2
          person3_v0 = NE.head $ versions $ theSchema @Person3
          person4_vPerson2 = person2_v0
          person4_vPerson3 = person3_v0
          encoder_p4v0 = encodeTo person4_v0
          encoder_p3_to_p4 = encodeTo person4_vPerson3
          encoder_p2_to_p4 = encodeTo person4_vPerson2
          encoder_p2v0 = encodeTo person2_v0
          encoder_p3v0 = encodeTo @Person3 person3_v0
    describe "Schemas" $ do
      prop "finite(schema @Schema) is a supertype of (schema @Schema)" $ \(SmallNatural n) ->
        theSchema @Schema `shouldBeSubtypeOf` finite n (theSchema @Schema)
    describe "Person" $ do
      it "decode is the inverse of encode" $ do
        decode (encode pepe) `shouldBe` Right pepe
        decode (fromRight undefined (encodeTo (theSchema @Person)) pepe) `shouldBe` Right pepe
    describe "Person2" $ do
      it "can compute an encoder for Person2" $
        encoder_p2v0 `shouldSatisfy` isRight
      it "decode is the inverse of encode" $ do
        decode (encode pepe2) `shouldBe` Right pepe2
        let fullEncoder = encodeTo (theSchema @Person2)
        fullEncoder `shouldSatisfy` isRight
        decode (fromRight (error "internal error") fullEncoder pepe2) `shouldBe` Right pepe2
      it "Person2 < Person" $ do
        theSchema @Person2 `shouldBeSubtypeOf`   theSchema @Person
      it "pepe2 `as` Person" $ do
        let encoder = encodeTo (theSchema @Person)
        encoder `shouldSatisfy` isRight
        decode (fromRight undefined encoder pepe2) `shouldBe` Right pepe
      it "pepe `as` Person2" $ do
        let decoder = decodeFrom (theSchema @Person)
        decoder `shouldSatisfy` isJust
        fromJust decoder (encode pepe) `shouldBe` Right pepe2{Person2.education = [Person.studies pepe]}
      it "Person < Person2" $ do
        theSchema @Person `shouldBeSubtypeOf`   theSchema @Person2
    describe "Person3" $ do
      it "cannot compute an encoder for Person3 (infinite schema)" $
        shouldLoop $ evaluate encoder_p3v0
      it "finiteEncode works as expected" $ shouldLoop $ evaluate $ A.encode
        (finiteEncode 4 laura3)
    describe "Person4" $ do
      let encoded_pepe4 = fromRight undefined encoder_p4v0 pepe4
          encoded_pepe3 = fromRight undefined encoder_p3_to_p4 pepe3{Person3.spouse = Nothing}
          encoded_pepe2 = fromRight undefined encoder_p2_to_p4 pepe2
          encoded_pepe2' = fromRight undefined encoder_p2v0 pepe2
      it "can compute an encoder for Person4" $ do
        shouldNotLoop $ evaluate encoder_p4v0
        encoder_p4v0 `shouldSatisfy` isRight
      it "can not compute an encoder for Person3 in finite time" $ do
        shouldLoop $ evaluate encoder_p3_to_p4
      it "can compute an encoder for Person2 in finite time" $ do
        shouldNotLoop $ evaluate encoder_p2_to_p4
      it "can encode a Person4" $ do
        shouldNotLoop $ evaluate $ A.encode encoded_pepe4
      it "can encode a Person2 as Person4 in finite time" $ do
        shouldNotLoop $ evaluate $ A.encode encoded_pepe2
      it "can decode a fully defined record with source schema" $ do
        let res = fromJust (decodeFrom person4_v0) encoded_pepe4
        shouldNotLoop $ evaluate res
        res `shouldBe` Right pepe4
      it "can decode a fully defined record without source schema" $ do
        let res = decode encoded_pepe4
        shouldNotLoop $ evaluate res
        res `shouldBe` Right pepe4
      it "can decode a Person2 encoded to Person4 with source schema" $ do
        let res = fromJust (decodeFrom person4_vPerson2) encoded_pepe2
        shouldNotLoop $ evaluate res
        res `shouldBe` Right pepe4
      it "can decode a Person2 encoded to Person4 without source schema" $ do
        let res = decode encoded_pepe2
        shouldNotLoop $ evaluate res
        res `shouldBe` Right pepe4
      it "can decode a Person2" $ do
        let res = fromJust (decodeFrom person2_v0) encoded_pepe2'
            holds = res == Right pepe4
        shouldNotLoop $ evaluate holds
        shouldNotLoop $ evaluate $ length $ show res
        res `shouldBe` Right pepe4

encodeToWithSpec :: TypedSchema a -> Schema -> Maybe (a -> A.Value)
encodeToWithSpec sc target = case isSubtypeOf (extractValidators sc) (extractSchema sc) target of
  Right cast -> Just $ cast . encodeWith sc
  _ -> Nothing

encodeToSpec :: HasSchema a => Schema -> Maybe (a -> A.Value)
encodeToSpec tgt = encodeToWithSpec schema tgt

shouldBeSubtypeOf :: Schema -> Schema -> Expectation
shouldBeSubtypeOf a b = case isSubtypeOf primValidators a b of
  Right _ -> pure ()
  _       -> expectationFailure $ show a <> " should be a subtype of " <> show b

shouldNotBeSubtypeOf :: Schema -> Schema -> Expectation
shouldNotBeSubtypeOf a b = case isSubtypeOf primValidators a b of
  Right _  -> expectationFailure $ show a <> " should not be a subtype of " <> show b
  _ -> pure ()

shouldLoop :: (Show a) => IO a -> Expectation
shouldLoop act = do
  res <- timeout 1000000 act
  res `shouldSatisfy` isNothing

shouldNotLoop :: (Show a) => IO a -> Expectation
shouldNotLoop act = do
  res <- timeout 1000000 act
  res `shouldSatisfy` isJust

makeField :: a -> Schema -> Bool -> (a, Field)
makeField n t isReq = (n, Field t isReq)

constructor' :: a -> b -> (a, b)
constructor' n t = (n, t)

prim :: Schema
prim = Prim "A"

primValidators :: Validators
primValidators = validatorsFor @(Schema, Double, Int, Bool)

finiteCornerCase :: Schema
finiteCornerCase = AllOf [ Array $ Prim "A"]
