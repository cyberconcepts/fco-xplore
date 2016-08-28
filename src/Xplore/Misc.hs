{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Xplore.Misc where

import BasicPrelude


newtype Eff a = Eff (IO a)

instance Monad Eff where
    return = \x -> Eff $ return x
    (Eff x) >>= f = Eff (x >>= g)
      where Eff . g = f


--runEff :: Eff ()
--runEff = putStrLn "working"
