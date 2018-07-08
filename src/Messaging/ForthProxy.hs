{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Messaging.ForthProxy where

import BasicPrelude
import Data.Text (unpack)

import Control.Monad (forever)

import Control.Distributed.Process (Process, liftIO)
import Control.Distributed.Process.Backend.SimpleLocalnet (
    initializeBackend, newLocalNode)
import Control.Distributed.Process.Node (
    initRemoteTable, forkProcess, runProcess)

import System.IO (
    BufferMode (NoBuffering), Handle,
    hSetBuffering, hFlush, hGetContents, hGetLine, hPutStr, openFile)
import System.Process (
    StdStream (CreatePipe), 
    proc, shell, std_in, std_out, withCreateProcess)


forthHome = "~/development/forth/"
forthModule = "xplore/interact.fs"
forthCommand = "gforth " ++ forthHome ++ forthModule ++ " -e repl"

host = "127.0.0.1"
port = "8899"


run :: IO ()
run = do
  backend <- initializeBackend host port initRemoteTable
  node <- newLocalNode backend
  withCreateProcess (shell forthCommand)
    { std_in = CreatePipe, std_out = CreatePipe } 
    $ \(Just hIn) (Just hOut) _ hProc -> do
      hSetBuffering hIn NoBuffering
      hSetBuffering hOut NoBuffering
      pid <- forkProcess node $ process4thOutput hOut 
      runProcess node $ provide4thInput hIn


provide4thInput :: Handle -> Process ()
provide4thInput handle = do
  s <- liftIO getLine
  case s of 
    "bye" -> liftIO $ hPutStr handle "bye\n"
    _ -> (liftIO $ hPutStr handle $ unpack (s ++ "\n")) >>
         provide4thInput handle


process4thOutput :: Handle -> Process ()
process4thOutput handle =
  forever $ liftIO $ hGetLine handle >>= print
