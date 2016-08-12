{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
-- see Oleg Kiselyov, Hiromi Ishii: Freer Monads, More Extensible Effects
--

module Effects where

import BasicPrelude


data Iter i a = Pure a
              | Get (i -> Iter i a)


ask :: Iter i i
ask = Get Pure


instance Functor (Iter i) where
  fmap = liftM

instance Applicative (Iter i) where
  pure = Pure
  (<*>) = ap

instance Monad (Iter i) where 
  return = Pure
  Pure x >>= k = k x
  Get k' >>= k = Get (k' >=> k)


-- Kleisli composition
--(>>>) = (>=>)
--(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
--f >=> g = (>>= g) . f
