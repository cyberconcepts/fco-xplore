{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Messaging.ForthProxy where

import BasicPrelude
import qualified Data.Text as T

import Data.Binary (Binary)
import Control.Monad (forever)
import GHC.Generics (Generic)

import Control.Distributed.Process (
    Message, Process, ProcessId,
    expect, expectTimeout, getSelfPid, 
    match, matchAny, receiveWait,
    send, spawnLocal)
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


-- control

data CtlMsg = QuitMsg
  deriving (Show, Generic, Typeable)
instance Binary CtlMsg

processQuit :: ProcessId -> CtlMsg -> Process Bool
processQuit pid QuitMsg = 
    send pid ("bye\n" :: Text) >> return False


-- console

data ConMsg = ConMsg Text
  deriving (Show, Generic, Typeable)
instance Binary ConMsg

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
      _ -> send p $ ConMsg line

conWriter :: ConSrv
conWriter p = forever $ do expect >>= putStrLn

processConMsg :: ProcessId -> ConMsg -> Process Bool
processConMsg pid (ConMsg txt) = 
    send pid txt >> return True


-- Forth stuff

data FthMsg = FthMsg Text
  deriving (Show, Generic, Typeable)
instance Binary FthMsg

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
    send p $ FthMsg $ T.pack line

fthInput :: FthSrv
fthInput p hIn = 
  forever $ do 
    line <- expect
    liftIO $ hPutStrLn hIn $ T.unpack line

processFthMsg :: ProcessId -> FthMsg -> Process Bool
processFthMsg pid (FthMsg txt) = 
    send pid txt >> return True


-- message dispatching

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
              continue <- receiveWait [
                    match $ processQuit fthIn, 
                    match $ processFthMsg conW, 
                    match $ processConMsg fthIn]
              case continue of
                True -> loop fthIn conW
                _ -> return ()

