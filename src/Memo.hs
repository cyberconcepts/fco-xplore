{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
-- see http://www.scs.stanford.edu/16wi-cs240h
--

module Memo where

import BasicPrelude
import qualified Data.Map.Strict as M

data Memo a b = Memo (M.Map a b) (a -> b)

memoize :: Ord a => (a -> b) -> Memo a b
memoize f = Memo (M.empty) f

evaluate :: Ord a => Memo a b -> a -> Memoized a b
evaluate mf x = case M.lookup x m of
    Nothing -> let y = f x
               in pack (Memo (M.insert x y m) f) y
    Just y  -> pack mf y
  where Memo m f = mf

memoFmap :: Functor f => Memo a b -> f a -> f b
memoFmap = undefined


-- | Memoized implementation

type Memoized a b = (b, Memo a b)

pack :: Memo a b -> b -> Memoized a b
pack mf y = (y, mf)

getValue :: Memoized a b -> b
getValue (y, _) = y

getMemo :: Memoized a b -> Memo a b
getMemo (_, mf) = mf
