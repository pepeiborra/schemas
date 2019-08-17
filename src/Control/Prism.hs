{-# LANGUAGE RankNTypes #-}
-- Inlined from the 'lens' package to avoid a dependency
module Control.Prism where

import Data.Coerce
import Data.Functor.Identity
import Data.Profunctor

type Prism' s a = forall p f . (Choice p, Applicative f) => p a (f a) -> p s (f s)

withPrism :: Prism' s a -> ((a -> s) -> (s -> Either s a) -> r) -> r
withPrism k f = case coerce (k (Market Identity Right)) of
  Market bt seta -> f bt seta

prism :: (a -> s) -> (s -> Maybe a) -> Prism' s a
prism bs sma = dimap (\s -> maybe (Left s) Right (sma s)) (either pure (fmap bs)) . right'

data Market a b s t = Market (b -> t) (s -> Either t a)

-- | @type 'Market'' a s t = 'Market' a a s t@
type Market' a = Market a a

instance Functor (Market a b s) where
  fmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)
  {-# INLINE fmap #-}

instance Profunctor (Market a b) where
  dimap f g (Market bt seta) = Market (g . bt) (either (Left . g) Right . seta . f)
  {-# INLINE dimap #-}
  lmap f (Market bt seta) = Market bt (seta . f)
  {-# INLINE lmap #-}
  rmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)
  {-# INLINE rmap #-}

instance Choice (Market a b) where
  left' (Market bt seta) = Market (Left . bt) $ \sc -> case sc of
    Left s -> case seta s of
      Left t -> Left (Left t)
      Right a -> Right a
    Right c -> Left (Right c)
  {-# INLINE left' #-}
  right' (Market bt seta) = Market (Right . bt) $ \cs -> case cs of
    Left c -> Left (Left c)
    Right s -> case seta s of
      Left t -> Left (Right t)
      Right a -> Right a
  {-# INLINE right' #-}
