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
    StdStream (CreatePipe), ProcessHandle, 
    createProcess, proc, shell, std_err, std_in, std_out)


forthHome = "~/development/forth/"
forthModule = "xplore/interact.fs"
forthCommand = "gforth " ++ forthHome ++ forthModule ++ " -e repl"

host = "127.0.0.1"
port = "8899"


-- control messages

data CtlMsg = QuitMsg
  deriving (Show, Generic, Typeable)
instance Binary CtlMsg

handleQuit :: ProcessId -> CtlMsg -> Process Bool
handleQuit pid QuitMsg = 
    send pid ("bye\n" :: Text) >> return False


-- console service

data ConParams = ConParams { p_conR :: ProcessId, p_conW :: ProcessId }

data ConMsg = ConMsg Text
  deriving (Show, Generic, Typeable)
instance Binary ConMsg

setupConsole :: ProcessId -> Process ProcessId
setupConsole parent = do
    spawnLocal (conWriter parent) >>= return

conWriter :: ProcessId -> Process ()
conWriter parent = do
    pidR <- spawnLocal $ conReader parent
    forever $ do expect >>= putStrLn

conReader :: ProcessId -> Process ()
conReader parent =
    forever $ do
      line <- getLine
      case line of
        "bye" -> send parent QuitMsg
        _ -> send parent $ ConMsg line

handleConMsg :: ProcessId -> ConMsg -> Process Bool
handleConMsg pid (ConMsg txt) = 
    send pid txt >> return True


-- Forth proxy service

data FthParams = FthParams { 
        p_hIn :: Handle, p_hOut :: Handle , 
        p_hErr :: Handle, p_hProc :: ProcessHandle}

data FthConfig = FthConfig { cfg_forthCommand :: Text }

data FthMsg = FthMsg Text
  deriving (Show, Generic, Typeable)
instance Binary FthMsg

setupForth :: ProcessId -> FthConfig -> Process ProcessId
setupForth parent config = do
    (Just hIn, Just hOut, Just hErr, hProc) <- liftIO $ createProcess 
            (shell $ T.unpack forthCommand)
            { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe } 
    liftIO $ hSetBuffering hIn NoBuffering
    liftIO $ hSetBuffering hOut NoBuffering
    let params = FthParams hIn hOut hErr hProc
    spawnLocal (fthInput parent params) >>= return

fthInput :: ProcessId -> FthParams -> Process ()
fthInput parent params = do
  let hIn = p_hIn params
  pidOut <- spawnLocal $ fthOutput parent params
  forever $ do 
    line <- expect
    liftIO $ hPutStrLn hIn $ T.unpack line

fthOutput :: ProcessId -> FthParams -> Process ()
fthOutput parent params = do
  let hOut = p_hOut params
  forever $ do 
    line <- liftIO $ hGetLine hOut
    send parent $ FthMsg $ T.pack line

handleFthMsg :: ProcessId -> FthMsg -> Process Bool
handleFthMsg pid (FthMsg txt) = 
    send pid txt >> return True


-- application

run :: IO ()
run = do
    backend <- initializeBackend host port initRemoteTable
    node <- newLocalNode backend
    runProcess node $ do
      self <- getSelfPid
      conW <- setupConsole self
      fthIn <- setupForth self $ FthConfig forthCommand
      loop fthIn conW
      where 
        loop fthIn conW = do
          continue <- receiveWait [
                match $ handleQuit fthIn, 
                match $ handleFthMsg conW, 
                match $ handleConMsg fthIn]
          case continue of
            True -> loop fthIn conW
            _ -> return ()

