{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Xplore.PipesTut where

import BasicPrelude
import Control.Monad (unless)
import Pipes
import System.IO (isEOF)


run :: IO ()
run = runEffect loop

loop :: Effect IO ()
loop = for stdinLn (lift . putStrLn)

stdinLn :: Producer Text IO ()
stdinLn = do
    eof <- lift isEOF
    unless eof $ do
        str <- lift getLine
        yield str
        stdinLn
