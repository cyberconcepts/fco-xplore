{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE GADTs, ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

--{-# LANGUAGE DataKinds, FlexibleContexts #-}

module Messaging.ForthProxy where

import BasicPrelude
import Data.Text (pack, unpack)

import Data.Binary (Binary)
import Control.Monad (forever)
import GHC.Generics (Generic)


import Control.Distributed.Process (
    Process, ProcessId,
    expect, expectTimeout, getSelfPid, liftIO, send, spawnLocal)
import Control.Distributed.Process.Backend.SimpleLocalnet (
    initializeBackend, newLocalNode)
import Control.Distributed.Process.Node (
    initRemoteTable, forkProcess, runProcess)

import System.IO (
    BufferMode (NoBuffering), Handle,
    hSetBuffering, hGetLine, hPutStrLn)
import System.Process (
    StdStream (CreatePipe), 
    proc, shell, std_in, std_out, withCreateProcess)


forthHome = "~/development/forth/"
forthModule = "xplore/interact.fs"
forthCommand = "gforth " ++ forthHome ++ forthModule ++ " -e repl"

host = "127.0.0.1"
port = "8899"


data ForthProxy r where 
  SendInput :: String -> ForthProxy ()
  ReceiveOutput :: ForthProxy String


-- console

type ConSrv = ProcessId -> Process ()

console :: ProcessId -> ConSrv -> ConSrv
              -> Process (ProcessId, ProcessId)
console parent reader writer = do
    pidR <- spawnLocal $ reader parent
    pidW <- spawnLocal $ writer parent
    return (pidR, pidW)

conReader :: ConSrv
conReader p = 
  forever $ do
    line <- getLine
    case line of
      "bye" -> send p QuitMsg
      _ -> send p $ ConMsg $ unpack line


conWriter :: ConSrv
conWriter p = 
  forever $ (expect :: Process String) >>= print


-- Forth stuff

type FthSrv = ProcessId -> Handle -> Process ()

forth :: ProcessId -> FthSrv -> FthSrv -> Handle -> Handle
              -> Process (ProcessId, ProcessId)
forth parent output input hOut hIn = do
      pidOut <- spawnLocal $ output parent hOut
      pidIn <- spawnLocal $ input parent hIn
      return (pidOut, pidIn)

fthOutput :: FthSrv
fthOutput p hOut = 
  forever $ do 
    line <- liftIO $ hGetLine hOut
    send p $ FthMsg line

fthInput :: FthSrv
fthInput p hIn = 
  forever $ do 
    line <- expect
    liftIO $ hPutStrLn hIn line


-- message dispatching

data Message = ConMsg String | FthMsg String | QuitMsg
  deriving (Generic, Typeable)
instance Binary Message

run :: IO ()
run = do
    backend <- initializeBackend host port initRemoteTable
    node <- newLocalNode backend
    withCreateProcess (shell forthCommand)
      { std_in = CreatePipe, std_out = CreatePipe } 
      $ \(Just hIn) (Just hOut) _ hProc -> do
        hSetBuffering hIn NoBuffering
        hSetBuffering hOut NoBuffering
        runProcess node $ do
          self <- getSelfPid
          (_, conW) <- console self conReader conWriter
          (_, fthIn) <- forth self fthOutput fthInput hOut hIn
          loop fthIn conW
          where 
            loop fthIn conW = do
                msg <- expect
                case msg of
                  ConMsg txt -> send fthIn txt >> loop fthIn conW
                  FthMsg txt -> send conW txt >> loop fthIn conW
                  QuitMsg -> do 
                    send fthIn ("bye\n" :: String)
                    return ()

