{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
-- parsing exercises
--

module Parsing where

import BasicPrelude
import GHC.Read (expectP, lexP, readPrec)
import Text.Megaparsec
import Text.Megaparsec.Text.Lazy
import Text.ParserCombinators.ReadPrec
import qualified Text.Read.Lex as L


spql :: Text -> Either ParseError String
spql = parse select "input"

select = string' "SELECT"


-- experiments with Read instance

data TTuple = TT String String deriving Show

instance Read TTuple where
  readPrec = readTTuple

readTTuple :: ReadPrec TTuple
readTTuple = do
    L.Ident x <- lexP
    L.Symbol ":" <- lexP
    L.Ident y <- lexP
    return $ TT x y

