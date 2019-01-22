{-# LANGUAGE OverloadedStrings #-}

module Xplore.ServiceSpec (main, spec) where

import Test.Hspec

import Control.Exception (evaluate)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "dummy" $ do
    it "does nothing" $ do
      True `shouldBe` True
