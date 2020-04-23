{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module SchemasSpec where

import           Control.Exception
import           Control.Lens (_Just, _Nothing, _Empty, _Cons)
import           Control.Monad (join)
import           Control.Monad.Trans.Except (Except, ExceptT(..))
import qualified Data.Aeson                 as A
import qualified Data.Coerce
import           Data.Either
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Generics
import qualified Data.List.NonEmpty        as NE
import           Data.Maybe
import           Generators
import           Looper
import           Person
import           Person2
import           Person3
import           Person4
import           Schemas
import qualified Schemas.Attempt            as Attempt
import           Schemas.Internal           (liftAttempt)
import           Schemas.Untyped            (Validators)
import           System.Timeout
import           Test.Hspec
import           Test.Hspec.Runner          (configQuickCheckMaxSuccess, hspecWith, defaultConfig)
import           Test.QuickCheck            (arbitrary, sized, forAll, suchThat)
import           Text.Show.Functions        ()
import           Unions

main :: IO ()
main = hspecWith defaultConfig{configQuickCheckMaxSuccess = Just 10000} spec

listSchema :: HasSchema a => TypedSchema [a]
listSchema = named "list" $ union
  [ ("Nil", alt _Empty)
  , ( "Cons"
    , altWith
      (record $ (,) <$> field "head" fst <*> fieldWith listSchema "tail" snd)
      _Cons
    )
  ]

encodeSpec :: Spec
encodeSpec = do
    it "prims"  $ do
      let encoder = encode
      shouldNotDiverge $ evaluate encoder
      shouldNotDiverge $ evaluate $ encoder True
    it "unions" $ do
      let encoder = encode
      shouldNotDiverge $ evaluate encoder
      shouldNotDiverge $ evaluate $ encoder (Left ())
      shouldNotDiverge $ evaluate $ encoder (Right ())

    it "recursive schemas" $ do
      let encoder = encodeWith listSchema
      shouldNotDiverge $ evaluate encoder
      shouldNotDiverge $ evaluate $ encoder [()]

    it "is the inverse of decoding for canonical schemas" $
      forAll (canonical <$> arbitrary) $ \sc ->
        getSuccess (pure encode >>= decode . ($ sc)) == Just sc

canonical :: Schema -> Schema
canonical = everywhere (mkT simplify)
  where
  simplify (OneOf [x]) = x
  simplify other = other

encodeToSpec :: Spec
encodeToSpec = do
    it "is lazy" $ do
      evaluate (attemptSuccessOrError (encodeToWith (record $ Just <$> field "bottom" fromJust) (Record [makeField "bottom" prim True])) (Nothing :: Maybe Bool))
        `shouldThrow` \(_ :: SomeException) -> True
      let encoded =
            attemptSuccessOrError
              (encodeToWith (record $ Just <$> field "bottom" fromJust) (Record []))
              (Nothing :: Maybe Bool)
      encoded `shouldBe` A.Object []

    it "SomeNone Some" $ do
      let encoded = encodeWith schemaSomeNone (Some ())
      shouldNotDiverge $ evaluate encoded
      encoded `shouldBe` A.Object []

    it "NoneSome Some" $ do
      let encoded = encodeWith schemaNoneSome (Some ())
      shouldNotDiverge $ evaluate encoded
      encoded `shouldBe` A.Object []

    it "SomeNone None" $ do
      let encoded = encodeWith schemaSomeNone (None @(Either () ()))
      shouldNotDiverge $ evaluate encoded
      encoded `shouldBe` A.Object []

    it "NoneSome None" $ do
      let encoded = encodeWith schemaNoneSome (None @(Either () ()))
      shouldNotDiverge $ evaluate encoded
      encoded `shouldBe` A.Object []

    it "Three" $ do
      let encoded = encodeWith (schemaThree schemaNoneSome schema) (Three @(Some ()) @())
      shouldNotDiverge $ evaluate encoded
      encoded `shouldBe` A.Object []

    it "Three" $ do
      let encoded = encodeWith (schemaThree' schema schemaNoneSome) (Three @() @(Some ()))
      shouldNotDiverge $ evaluate encoded
      encoded `shouldBe` A.Object []

    describe "Either" $ do
      let -- A schema supporting both camelCase and lowercase either
          source :: TypedSchema (Either () ())
          source = schema

      it "lowerCase" $ do
        let target = Union [("right", schemaFor @()), ("left", schemaFor @())]
            encoder = encodeToWith source target
            Right f = encoder
        encoder `shouldSatisfy` isRight
        f (Right ()) `shouldBe` A.object [("right", A.Object [])]

      it "camelCase" $ do
        let target = Union [("Right", schemaFor @()), ("Left", schemaFor @())]
            encoder = encodeToWith source target
            Right f = encoder
        f (Right ()) `shouldBe` A.object [("Right", A.Object [])]

    describe "Either (nested)" $ do
      let -- A schema supporting both camelCase and lowercase either
          source :: TypedSchema (((), Either () ()))
          source = schema

          wrap sc = Record [("$1", Field (schemaFor @()) True), ("$2", Field sc True)]

          wrapVal v = A.object [("$1", A.Object []), ("$2", v)]

      it "lowerCase" $ do
        let target = wrap $ Union [("right", schemaFor @()), ("left", schemaFor @())]
            encoder = encodeToWith source target
            Right f = encoder
        encoder `shouldSatisfy` isRight
        f ((),Right ()) `shouldBe` wrapVal (A.object [("right", A.Object [])])

      it "camelCase" $ do
        let target = wrap $ Union [("Right", schemaFor @()), ("Left", schemaFor @())]
            encoder = encodeToWith source target
            Right f = encoder
        encoder `shouldSatisfy` isRight
        f ((),Right ()) `shouldBe` wrapVal (A.object [("Right", A.Object [])])

    describe "canEncode" $ do

      it "Unions of 1 constructor" $ do
        union [("Just", alt (_Just @()))] `shouldBeAbleToEncodeTo` [Union [("Just", Unit)]]


spec :: Spec
spec = do
  describe "encode" encodeSpec
  describe "encodeTo" encodeToSpec
  describe "extractSchema" $ do
    it "Named" $
      shouldNotDiverge $ evaluate $ extractSchema $ schema @Schema
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
    describe "Schema" $
      schemaSpec schema (schemaFor @Person2)
    let   person4_v0 = schemaFor @Person4
          person2_v0 = schemaFor @Person2
          person2_v2 = extractSchema (schema @Person2) NE.!! 2
          person3_v0 = schemaFor @Person3
          person4_vPerson3 = person3_v0
          encoder_p4v0 = encodeTo person4_v0
          encoder_p3_to_p4 = encodeTo person4_vPerson3
          encoder_p2v0 = encodeTo person2_v0
          encoder_p3v0 = encodeTo @Person3 person3_v0
          decoder_p2v0 = decodeFrom @Person4 person2_v0
          decoder_p2v2 = decodeFrom person2_v2

    describe "NoneSome Bool" $
      schemaSpec schemaNoneSome (None :: Some Bool)

    describe "SomeNone Bool" $
      schemaSpec schemaSomeNone (None :: Some Bool)

    describe "NoneSome (Either () ())" $
      schemaSpec schemaNoneSome (None :: Some (Either () ()))

    describe "SomeNone (Either () ())" $
      schemaSpec schemaSomeNone (None :: Some (Either () ()))

    describe "Three Bool Int" $
      schemaSpec (schemaThree  schema schema) (Three :: Three Bool Int)

    describe "Three Int Bool" $
      schemaSpec (schemaThree' schema schema) (Three :: Three Int Bool)

    describe "Person" $ do
      schemaSpec schema pepe

    describe "Person2" $ do
      schemaSpec schema pepe2
      it "Person2 < Person" $ do
         shouldBeAbleToEncode @Person2 (extractSchema @Person schema)
         -- shouldBeAbleToDecode @Person (extractSchema @Person2 schema)
      it "pepe2 `as` Person" $ do
        let encoder = encodeTo (schemaFor @Person)
            encoded = attemptSuccessOrError encoder pepe2
        encoder `shouldSatisfy` isRight
        decode (encoded) `shouldBe` Success pepe
      it "pepe `as` Person2" $ do
        let decoder = decodeFrom (schemaFor @Person)
        decoder `shouldSatisfy` isSuccess
        (pure encode >>= getSuccessOrError decoder . ($ pepe))
          `shouldBe` Success pepe2{Person2.education = [Person.studies pepe]}
      it "Person < Person2" $ do
        -- shouldBeAbleToEncode @Person  (extractSchema @Person2 schema)
        shouldBeAbleToDecode @Person2 (extractSchema @Person schema)

    describe "Person3" $ do
      -- disabled because encode diverges and does not support IterT yet
      -- schemaSpec schema pepe3
      it "can show the Person 3 (circular) schema" $
        shouldNotDiverge $ evaluate $ length $ show $ schemaFor @Person3
      it "can compute an encoder for Person3 (circular schema)" $
        shouldNotDiverge $ evaluate encoder_p3v0
      it "can encode a finite example" $ do

        shouldNotDiverge $ evaluate $ encode martin
        shouldNotDiverge $ evaluate $ attemptSuccessOrError encoder_p3v0 martin

    describe "Person4" $ do
      schemaSpec schema pepe4
      let encoded_pepe4 = attemptSuccessOrError encoder_p4v0 pepe4
          encoded_pepe3 = attemptSuccessOrError encoder_p3_to_p4 pepe3{Person3.spouse = Nothing}
          encoded_pepe2 = attemptSuccessOrError encoder_p2v0 pepe2
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
        let res = getSuccessOrError (decodeFrom person4_v0) encoded_pepe4
        shouldNotDiverge $ evaluate res
        res `shouldBe` Success pepe4
      it "can decode a fully defined record without source schema" $ do
        let res = decode encoded_pepe4
        shouldNotDiverge $ evaluate res
        res `shouldBe` Success pepe4
      it "cannot construct a Person2 v0 decoder" $
        decoder_p2v0 `shouldSatisfy` isFailure
      it "can construct a Person2 v1 decoder" $
        decoder_p2v2 `shouldSatisfy` isSuccess
      it "can decode a Person2 v1" $ do
        let res = getSuccessOrError decoder_p2v2 encoded_pepe2
            holds = res == Success pepe4
        shouldNotDiverge $ evaluate holds
        shouldNotDiverge $ evaluate $ length $ show res
        res `shouldBe` Success pepe4
    describe "Looper" $ do
      schemaSpec schema looper1

schemaSpec :: forall a. (Eq a, Show a) => TypedSchema a -> a -> Spec
schemaSpec sc ex = do
  let encoder = encodeToWith sc s
      decoder = decodeFromWith sc s
      s = NE.head $ extractSchema sc
      encodedExample = attemptSuccessOrError encoder ex
  it "Can extract untyped schema" $
    shouldNotDiverge $ evaluate s
  it "Can encode itself" $ do
    shouldNotDiverge $ evaluate encoder
    encoder `shouldSatisfy` isRight
  it "Can decode itself" $ do
    shouldNotDiverge $ evaluate decoder
    decoder `shouldSatisfy` isSuccess
  it "Does not diverge decoding bad input" $ do
     let d = join $ Attempt.attemptSuccess $ runResult 1000 $ decodeFromWith sc (NE.head $ extractSchema sc)
     shouldNotDiverge $ evaluate $ d
     shouldNotDiverge $ evaluate $ join $ join $ Attempt.attemptSuccess $ runResult 1000 $ traverse ($ A.String "Foo") d
  it "Roundtrips ex" $ do
    let res = getSuccessOrError decoder encodedExample
    shouldNotDiverge $ evaluate encodedExample
    shouldNotDiverge $ evaluate res
    runResult 1000 res `shouldBe` Right (Just ex)
  it "Roundtrips ex (2)" $ do
    let res = pure (encodeWith sc) >>= decodeWith sc . ($ ex)
    shouldNotDiverge $ evaluate res
    runResult 1000 res `shouldBe` Right (Just ex)

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
  case res of
    Just{} -> expectationFailure "Did not diverge"
    Nothing -> return ()

shouldNotDiverge :: (HasCallStack, Show a) => IO a -> Expectation
shouldNotDiverge act = do
  res <- timeout 1000000 act
  case res of
    Nothing -> error "Did not terminate after 1s"
    Just {} -> return ()

shouldBeAbleToEncode :: forall a . HasCallStack => (HasSchema a) => NE.NonEmpty Schema -> Expectation
shouldBeAbleToEncode = shouldBeAbleToEncodeTo (schema @a)

shouldBeAbleToEncodeTo :: forall a . HasCallStack => TypedSchema a -> NE.NonEmpty Schema -> Expectation
shouldBeAbleToEncodeTo tsc sc = asumEither (fmap (encodeToWith tsc) sc) `shouldSatisfy` isRight

shouldBeAbleToDecode :: forall a . HasCallStack => (HasSchema a) => NE.NonEmpty Schema -> Expectation
shouldBeAbleToDecode sc = asum (fmap (decodeFrom @a) sc) `shouldSatisfy` isSuccess

makeField :: a -> Schema -> Bool -> (a, Field)
makeField n t isReq = (n, Field t isReq)

constructor' :: a -> b -> (a, b)
constructor' n t = (n, t)

prim :: Schema
prim = Prim "A"

primValidators :: Validators
primValidators = validatorsFor @(Schema, Double, Int, Bool)

getSuccessOrError :: Result a -> a
getSuccessOrError =  either (error . show) (fromMaybe (error "too many delays")) . Attempt.runAttempt . runResult 1000

attemptSuccessOrError :: Show e => Either e a -> a
attemptSuccessOrError = either (error.show) id

pattern Success :: a -> Result a
pattern Success x <- (runResult 1000 -> Attempt.Success (Just x))
  where Success x = liftAttempt $ Attempt.Success x

getSuccess :: Result a -> Maybe a
getSuccess = join . Attempt.attemptSuccess . runResult 1000

isSuccess :: Result a -> Bool
isSuccess = isJust . getSuccess

isFailure :: Result a -> Bool
isFailure = not . isSuccess

-- | Parallel 'asum' for 'Either'
asumEither :: forall e a . (Monoid e) => NE.NonEmpty (Either e a) -> Either e a
asumEither = Data.Coerce.coerce asumExcept
  where
    asumExcept :: NE.NonEmpty (Except e a) -> Except e a
    asumExcept = asum
