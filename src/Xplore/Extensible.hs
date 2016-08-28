{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
--
-- see Oleg Kiselyov, Amr Sabry, Cameron Swords: Extensibel Effects
--

module Xplore.Extensible where

import BasicPrelude hiding (lift)
import Control.Eff (Eff, Member, SetMember, run)
import Control.Eff.Lift (Lift, lift, runLift)
import Control.Eff.Reader.Lazy (Reader, ask, reader, runReader)
import Data.OpenUnion.Imports (MemberU)
import Data.Void (Void)

--instance Member (Lift m) r => SetMember Lift (Lift m) r

tl1 :: (SetMember Lift (Lift IO) r, Member (Reader Int) r) => Eff r ()
tl1 = ask >>= \x -> lift . print $ (x + 1 :: Int)

tl1r :: IO ()
tl1r = runLift $ runReader tl1 (10 :: Int)
