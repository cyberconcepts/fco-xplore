{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DataKinds, FlexibleContexts, GADTs #-}

module Messaging.ForthProxy where

import BasicPrelude
import Data.Text (pack, unpack)

import Control.Monad (forever)

import Control.Monad.Freer (Eff, Member, interpretM, runM, send)

import Control.Distributed.Process (Process, liftIO)
import Control.Distributed.Process.Backend.SimpleLocalnet (
    initializeBackend, newLocalNode)
import Control.Distributed.Process.Node (
    initRemoteTable, forkProcess, runProcess)

import System.IO (
    BufferMode (NoBuffering), Handle,
    hSetBuffering, hGetLine, hPutStr)
import System.Process (
    StdStream (CreatePipe), 
    proc, shell, std_in, std_out, withCreateProcess)


forthHome = "~/development/forth/"
forthModule = "xplore/interact.fs"
forthCommand = "gforth " ++ forthHome ++ forthModule ++ " -e repl"

host = "127.0.0.1"
port = "8899"


data ForthProxy r where 
  StartProxy :: String -> ForthProxy ()

startProxy :: Member ForthProxy effs => String -> Eff effs ()
startProxy cmd = send (StartProxy cmd)

runForthProxy :: Eff '[ForthProxy, IO] a -> IO a
runForthProxy = runM . interpretM (\c ->
    case c of 
        StartProxy cmd -> doStartProxy cmd)

doStartProxy :: String -> IO ()
doStartProxy cmd = do
  backend <- initializeBackend host port initRemoteTable
  node <- newLocalNode backend
  withCreateProcess (shell cmd)
    { std_in = CreatePipe, std_out = CreatePipe } 
    $ \(Just hIn) (Just hOut) _ hProc -> do
      hSetBuffering hIn NoBuffering
      hSetBuffering hOut NoBuffering
      pid <- forkProcess node $ process4thOutput hOut 
      runProcess node $ provide4thInput hIn

run :: IO ()
run = runForthProxy $ startProxy forthCommand


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
