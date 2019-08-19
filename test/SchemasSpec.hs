{-# LANGUAGE OverloadedStrings #-}
module SchemasSpec where

import Schemas
import Test.Hspec

shouldBeSubtypeOf :: Schema -> Schema -> Expectation
shouldBeSubtypeOf a b = case a `isSubtypeOf` b of
  Just _ -> pure ()
  Nothing -> expectationFailure $ show a <> " should be a subtype of " <> show b

shouldNotBeSubtypeOf :: Schema -> Schema -> Expectation
shouldNotBeSubtypeOf a b = case a `isSubtypeOf` b of
  Just _  -> expectationFailure $ show a <> " should not be a subtype of " <> show b
  Nothing -> pure ()

spec :: Spec
spec = do
  describe "isSubtypeOf" $ do
    it "subtypes can add fields" $ do
      Record [("a",Number,True )] `shouldBeSubtypeOf` Record []
      Record [("a",Number,False)] `shouldBeSubtypeOf` Record []
    it "subtypes cannot turn a Required field into Optional" $ do
      Record [("a", Number, False)] `shouldNotBeSubtypeOf` Record [("a",Number,True )]
    it "subtypes can turn an Optional field into Required" $ do
      Record [("a", Number, True)] `shouldBeSubtypeOf` Record [("a",Number,False)]
    it "subtypes can relax the type of a field" $ do
      Record [("a", Array Number, True)] `shouldBeSubtypeOf` Record [("a",Number,True)]
    it "subtypes cannot remove Required fields" $ do
      Record [] `shouldNotBeSubtypeOf` Record [("a",Number,True )]
    it "subtypes can remove Optional fields" $ do
      Record [] `shouldBeSubtypeOf` Record [("a",Number,False)]
    it "subtypes can add enum choices" $ do
      Enum ["A"] `shouldBeSubtypeOf` Enum []
    it "subtypes cannot remove enum choices" $ do
      Enum [] `shouldNotBeSubtypeOf` Enum ["A"]
    it "subtypes can add constructors" $ do
      Union [("A", String)] `shouldBeSubtypeOf` Union []
    it "subtypes cannot remove constructors" $ do
      Union [] `shouldNotBeSubtypeOf` Union [("A", String)]
    it "subtypes can expand an array" $ do
      Array String `shouldBeSubtypeOf` String
    it "subtypes cannot drop an array" $ do
      String `shouldNotBeSubtypeOf` Array String