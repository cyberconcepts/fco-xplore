{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
-- parsing exercises
--

module Parsing where

import BasicPrelude
import Text.Megaparsec
import Text.Megaparsec.Text.Lazy


spql :: Text -> Either ParseError String
spql = parse select "input"


select = string' "SELECT"
