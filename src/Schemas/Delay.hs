{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module Schemas.Delay where

import Control.Monad.Trans.Iter
import Numeric.Natural

type DelayT m = IterT m
type Delay    = Iter

lift :: Monad m => Iter a -> IterT m a
lift = liftIter

runDelay :: Monad m => Natural -> DelayT m a -> m (Maybe a)
runDelay n = retract . cutoff (fromIntegral n)

runDelayUnbounded :: Monad m => DelayT m a -> m a
runDelayUnbounded = retract
