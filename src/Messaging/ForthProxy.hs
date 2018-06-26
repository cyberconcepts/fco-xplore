{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Messaging.ForthProxy where

import BasicPrelude
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import GHC.IO.Handle (
    BufferMode (NoBuffering), Handle,
    hSetBuffering, hFlush, hGetContents, hGetLine, hPutStr)
import System.Process (
    StdStream (CreatePipe), 
    proc, shell, std_in, std_out, withCreateProcess)
import Data.Text (unpack)


run :: IO ()
run = withCreateProcess (shell "/usr/local/bin/gforth") 
  { std_in = CreatePipe, std_out = CreatePipe } 
  $ \(Just hIn) (Just hOut) _ hProc -> do
    hSetBuffering hIn NoBuffering
    forkIO $ process4thOutput hOut
    provide4thInput hIn


provide4thInput :: Handle -> IO ()
provide4thInput handle = do
  s <- getLine
  case s of 
    "bye" -> hPutStr handle "bye\n"
    _ -> do 
      hPutStr handle $ unpack (s ++ "\n")
      provide4thInput handle


process4thOutput :: Handle -> IO ()
process4thOutput handle = 
  forever $ hGetLine handle >>= print
