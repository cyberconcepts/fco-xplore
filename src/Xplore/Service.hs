{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Xplore.Service where

import BasicPrelude
import qualified Data.Text as T

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM (
    TChan,
    atomically, newTChan, readTChan, writeTChan)
import Control.Monad.Extra (whileM)
import Control.Monad.STM


startService :: IO () -> IO ThreadId
startService = forkIO

newChan :: IO (TChan a)
newChan = atomically newTChan

receive :: TChan a -> IO a
receive = atomically . readTChan

send :: TChan a -> a -> IO ()
send chan msg = atomically $ writeTChan chan msg


conIn :: TChan Text -> IO ()
conIn chan =
  whileM $ do
    line <- getLine
    case line of
      "bye" -> send chan "quit" >> return False
      _ -> send chan line >> return True

conOut :: TChan Text -> IO ()
conOut chan =
  whileM $ do
    line <- receive chan
    case line of
      "quit" -> return False
      _ -> putStrLn line >> return True

demo = do
  ch <- newChan
  startService $ conOut ch
  startService $ conIn ch
