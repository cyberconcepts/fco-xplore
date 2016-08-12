{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
-- Yet Another PArser
--
-- see http://www.scs.stanford.edu/16wi-cs240h
--

module Yapa where

import BasicPrelude
import Data.Char (isDigit)

--type Parser s a = s -> Maybe (a, s)
newtype Parser s a = P { runP :: s -> Maybe (a, s) }

instance Functor (Parser s) where
  fmap = liftM

instance Applicative (Parser s) where
  pure = return
  (<*>) = ap

instance Monad (Parser s) where
  return a = P $ \input -> Just (a, input)
  p >>= next = P $ \input -> 
      case runP p input of
        Nothing -> Nothing
        Just (a, rest) -> runP (next a) rest


string :: String -> Parser String String
string pat = P $ \input ->
    case stripPrefix pat input of
      Nothing   -> Nothing
      Just rest -> Just (pat, rest)

number :: Parser String Int
number = P $ \input ->
    let (d, rest) = span isDigit input
    in case reads d of
        [(n, _)] -> Just (n, rest)
        _        -> Nothing


data HttpVersion = HttpVersion Int Int
    deriving (Show)

httpVersion1 :: Parser String HttpVersion
httpVersion1 = do
  string "HTTP/"
  major <- number
  string "."
  minor <- number
  return $ HttpVersion major minor
