{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module SchemasSpec where

import           Control.Exception
import           Control.Lens (_Just, _Nothing)
import           Control.Monad.Trans.Except
import qualified Data.Aeson                 as A
import           Data.Coerce
import           Data.Either
import           Data.Foldable
import           Data.Functor.Identity
import qualified Data.List.NonEmpty        as NE
import           Data.Maybe
import           Generators
import           Person
import           Person2
import           Person3
import           Person4
import           Schemas
import           Schemas.Untyped            (Validators)
import           System.Timeout
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.Hspec.Runner
import           Test.QuickCheck
import           Text.Show.Functions        ()

main :: IO ()
main = hspecWith defaultConfig{configQuickCheckMaxSuccess = Just 10000} spec

spec :: Spec
spec = do
  describe "encode" $ do
    prop "is the inverse of decoding" $ \(sc :: Schema) ->
      decode (encode sc) ==  Right sc
  describe "encodeTo" $ do
    it "is lazy" $ do
      evaluate (fromRight undefined (encodeToWith (record $ Just <$> field "bottom" fromJust) (Record [makeField "bottom" prim True])) (Nothing :: Maybe Bool))
        `shouldThrow` \(_ :: SomeException) -> True
      fromRight undefined (encodeToWith (record $ Just <$> field "bottom" fromJust) (Record [])) (Nothing :: Maybe Bool)
        `shouldBe` A.Object []
  describe "canEncode" $ do
    it "Empty to itself" $ do
      mempty`shouldBeAbleToEncodeTo` [Empty]
    it "Unions of 1 constructor" $ do
      union [("Just", alt (_Just @()))] `shouldBeAbleToEncodeTo` [Union [("Just", Unit)]]
  describe "extractSchema" $ do
    it "Unions" $
      extractSchema (union [("Just", alt (_Just @())), ("Nothing", alt _Nothing)])
        `shouldBe` [Union [("Nothing", Unit) ,("Just", Unit)]]
  describe "isSubtypeOf" $ do
    it "is reflexive (in absence of OneOf)" $ forAll (sized genSchema `suchThat` (not . hasOneOf)) $ \sc ->
      sc `shouldBeSubtypeOf` sc
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
    it "subtypes can relax the type of a constructor field" $ do
      Union [constructor' "a" prim]
        `shouldBeSubtypeOf` Union [constructor' "a" (Array prim)]
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
      Union [constructor' "B" Unit]
        `shouldBeSubtypeOf` Union [constructor' "A" Unit, constructor' "B" Unit]
    it "subtypes cannot add constructors" $ do
      Union [constructor' "A" prim, constructor' "B" Unit]
        `shouldNotBeSubtypeOf` Union [constructor' "A" (prim)]
    it "subtypes can drop an array" $ do
      prim `shouldBeSubtypeOf` Array prim
    it "subtypes cannot introduce an array" $ do
      Array prim `shouldNotBeSubtypeOf` prim
  describe "HasSchema" $ do
    it "Left is a constructor of Either" $ do
      shouldBeAbleToDecode @(Either () ()) [Union [constructor' "Left" Unit]]
      -- shouldBeAbleToEncode @(Either () ()) [Union [constructor' "Left" Unit]]
    it "left is a constructor of Either too" $ do
      shouldBeAbleToDecode @(Either () ()) [Union [constructor' "left" Unit]]
      -- shouldBeAbleToEncode @(Either () ()) [Union [constructor' "left" Unit]]
  describe "examples" $ do
    let   person4_v0 = theSchema @Person4
          person2_v0 = theSchema @Person2
          person2_v2 = extractSchema (schema @Person2) NE.!! 2
          person3_v0 = theSchema @Person3
          person4_vPerson3 = person3_v0
          encoder_p4v0 = encodeTo person4_v0
          encoder_p3_to_p4 = encodeTo person4_vPerson3
          encoder_p2v0 = encodeTo person2_v0
          encoder_p3v0 = encodeTo @Person3 person3_v0
          decoder_p2v0 = decodeFrom @Person4 person2_v0
          decoder_p2v2 = decodeFrom person2_v2
    describe "Person" $ do
      schemaSpec schema pepe
    describe "Person2" $ do
      schemaSpec schema pepe2
      it "Person2 < Person" $ do
         shouldBeAbleToEncode @Person2 (extractSchema @Person schema)
         -- shouldBeAbleToDecode @Person (extractSchema @Person2 schema)
      it "pepe2 `as` Person" $ do
        let encoder = encodeTo (theSchema @Person)
        encoder `shouldSatisfy` isRight
        decode (fromRight undefined encoder pepe2) `shouldBe` Right pepe
      it "pepe `as` Person2" $ do
        let decoder = decodeFrom (theSchema @Person)
        decoder `shouldSatisfy` isRight
        fromRight undefined decoder (encode pepe) `shouldBe` Right pepe2{Person2.education = [Person.studies pepe]}
      it "Person < Person2" $ do
        -- shouldBeAbleToEncode @Person  (extractSchema @Person2 schema)
        shouldBeAbleToDecode @Person2 (extractSchema @Person schema)
    describe "Person3" $ do
      it "can show the Person 3 (circular) schema" $
        shouldNotDiverge $ evaluate $ length $ show $ theSchema @Person3
      it "can compute an encoder for Person3 (circular schema)" $
        shouldNotDiverge $ evaluate encoder_p3v0
      it "can encode a finite example" $ do
        shouldNotDiverge $ evaluate $ encode martin
        shouldNotDiverge $ evaluate $ fromRight undefined encoder_p3v0 martin
    describe "Person4" $ do
      schemaSpec schema pepe4
      let encoded_pepe4 = fromRight undefined encoder_p4v0 pepe4
          encoded_pepe3 = fromRight undefined encoder_p3_to_p4 pepe3{Person3.spouse = Nothing}
          encoded_pepe2 = fromRight undefined encoder_p2v0 pepe2
      it "can compute an encoder for Person4" $ do
        shouldNotDiverge $ evaluate encoder_p4v0
        encoder_p4v0 `shouldSatisfy` isRight
      it "can compute an encoder to Person3 in finite time" $ do
        shouldNotDiverge $ evaluate encoder_p3_to_p4
      it "can compute an encoder to Person2 in finite time" $ do
        shouldNotDiverge $ evaluate encoder_p2v0
      it "can encode a Person4" $ do
        shouldNotDiverge $ evaluate $ A.encode encoded_pepe4
      it "can encode a Person2 as Person4 in finite time" $ do
        shouldNotDiverge $ evaluate $ A.encode encoded_pepe2
      it "can decode a fully defined record with source schema" $ do
        let res = fromRight undefined (decodeFrom person4_v0) encoded_pepe4
        shouldNotDiverge $ evaluate res
        res `shouldBe` Right pepe4
      it "can decode a fully defined record without source schema" $ do
        let res = decode encoded_pepe4
        shouldNotDiverge $ evaluate res
        res `shouldBe` Right pepe4
      it "cannot construct a Person2 v0 decoder" $
        decoder_p2v0 `shouldSatisfy` isLeft
      it "can construct a Person2 v1 decoder" $
        decoder_p2v2 `shouldSatisfy` isRight
      it "can decode a Person2 v1" $ do
        let res = fromRight undefined decoder_p2v2 encoded_pepe2
            holds = res == Right pepe4
        shouldNotDiverge $ evaluate holds
        shouldNotDiverge $ evaluate $ length $ show res
        res `shouldBe` Right pepe4

schemaSpec :: (Eq a, Show a) => TypedSchema a -> a -> Spec
schemaSpec sc ex = do
  let encoder = encodeToWith sc (NE.head $ extractSchema sc)
      decoder = decodeFromWith sc (NE.head $ extractSchema sc)
      encodedExample = fromRight undefined encoder ex
  it "Can encode itself" $
    encoder `shouldSatisfy` isRight
  it "Can decode itself" $
    decoder `shouldSatisfy` isRight
  it "Roundtrips ex" $ do
    let res = fromRight undefined decoder encodedExample
    shouldNotDiverge $ evaluate res
    res `shouldBe` Right ex
  it "Roundtrips ex (2)" $ do
    let res = decodeWith sc (encodeWith sc ex)
    shouldNotDiverge $ evaluate res
    res `shouldBe` Right ex

shouldBeSubtypeOf :: HasCallStack => Schema -> Schema -> Expectation
shouldBeSubtypeOf a b = case isSubtypeOf primValidators a b of
  Right _ -> pure ()
  _       -> expectationFailure $ show a <> " should be a subtype of " <> show b

shouldNotBeSubtypeOf :: HasCallStack => Schema -> Schema -> Expectation
shouldNotBeSubtypeOf a b = case isSubtypeOf primValidators a b of
  Right _  -> expectationFailure $ show a <> " should not be a subtype of " <> show b
  _ -> pure ()

shouldDiverge :: (HasCallStack, Show a) => IO a -> Expectation
shouldDiverge act = do
  res <- timeout 1000000 act
  res `shouldSatisfy` isNothing

shouldNotDiverge :: (HasCallStack, Show a) => IO a -> Expectation
shouldNotDiverge act = do
  res <- timeout 1000000 act
  res `shouldSatisfy` isJust

shouldBeAbleToEncode :: forall a . HasCallStack => (HasSchema a) => NE.NonEmpty Schema -> Expectation
shouldBeAbleToEncode = shouldBeAbleToEncodeTo (schema @a)

shouldBeAbleToEncodeTo :: forall a . HasCallStack => TypedSchema a -> NE.NonEmpty Schema -> Expectation
shouldBeAbleToEncodeTo tsc sc = asumEither (fmap (encodeToWith tsc) sc) `shouldSatisfy` isRight

shouldBeAbleToDecode :: forall a . HasCallStack => (HasSchema a) => NE.NonEmpty Schema -> Expectation
shouldBeAbleToDecode sc = asumEither (fmap (decodeFrom @a) sc) `shouldSatisfy` isRight

asumEither :: forall e a . (Monoid e) => NE.NonEmpty (Either e a) -> Either e a
asumEither = Data.Coerce.coerce asumExcept
  where
    asumExcept :: NE.NonEmpty (Except e a) -> Except e a
    asumExcept = asum

makeField :: a -> Schema -> Bool -> (a, Field)
makeField n t isReq = (n, Field t isReq)

constructor' :: a -> b -> (a, b)
constructor' n t = (n, t)

prim :: Schema
prim = Prim "A"

primValidators :: Validators
primValidators = validatorsFor @(Schema, Double, Int, Bool)
