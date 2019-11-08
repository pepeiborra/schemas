{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module Schemas.Attempt where

import Control.Applicative
import Data.Functor.Classes
import Control.Monad.Except
import Data.Maybe

-- | An applicative error type
data Attempt e a
  = Success a
  | Failure e
  deriving (Eq, Functor, Foldable, Traversable, Show)

instance Eq e => Eq1 (Attempt e) where
  liftEq _   (Failure e) (Failure e') = e == e'
  liftEq eq0 (Success a) (Success a') = eq0 a a'
  liftEq _ _ _ = False

instance Show e => Show1 (Attempt e) where
  liftShowsPrec _      _ p (Failure e) = showsPrec p e
  liftShowsPrec shows0 _ _ (Success a) = shows "Success " . shows0 0 a

instance Monoid e => Applicative (Attempt e) where
  pure = Success
  Success f <*> Success a  = Success (f a)
  Failure e <*> Failure e' = Failure (e <> e')
  Failure e <*> _ = Failure e
  _ <*> Failure e = Failure e

instance Monoid e => Alternative (Attempt e) where
  empty = Failure mempty
  Success a <|> _ = Success a
  _ <|> Success b = Success b
  Failure e <|> Failure e' = Failure (e <> e')

instance Monoid e => Monad (Attempt e) where
  return = pure
  Success a >>= k = k a
  Failure e >>= _ = Failure e

instance Monoid e => MonadPlus (Attempt e)

instance Monoid e => MonadError e (Attempt e) where
  throwError = Failure
  catchError (Failure e) h = h e
  catchError (Success a) _ = Success a

bindAttempt :: Attempt e a -> (a -> Attempt e b) -> Attempt e b
bindAttempt (Success a) k = k a
bindAttempt (Failure e) _ = Failure e

runAttempt :: Attempt e a -> Either e a
runAttempt = execAttempt

execAttempt :: MonadError e f => Attempt e a -> f a
execAttempt (Success x) = pure x
execAttempt (Failure e) = throwError e

-- | Partitions a result successes and failures
partitionAttempts :: [Attempt e a] -> ([e], [a])
partitionAttempts xx = (mapMaybe attemptFailure xx, mapMaybe attemptSuccess xx)

attemptFailure :: Attempt a1 a2 -> Maybe a1
attemptFailure Success{}   = Nothing
attemptFailure (Failure e) = Just e

attemptSuccess :: Attempt e a -> Maybe a
attemptSuccess (Success a) = Just a
attemptSuccess Failure{} = Nothing

isSuccess :: Attempt e a -> Bool
isSuccess Success{} = True
isSuccess _ = False

isFailure :: Attempt e a -> Bool
isFailure Failure{} = True
isFailure _ = False
