{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
-- see Simon Marlow: Parallel and Concurrent Programming in Haskell
--

module Doit where

import BasicPrelude
import Control.Concurrent (forkIO, threadDelay)
-- import Control.Monad (replicateM_)
import System.IO (BufferMode (NoBuffering), 
                  hSetBuffering, stdout)
import Text.Printf (printf)

import Logger (initLogger, logMessage,logStop)


run :: IO ()
run = logIt


concurrentPutChar :: IO ()
concurrentPutChar = do
    hSetBuffering stdout NoBuffering
    _ <- forkIO $ replicateM_ 1000 $ putChar 'A'
    replicateM_ 1000 $ putChar 'B'


remindMe :: IO ()
remindMe = loop
  where 
    loop = do
      s <- getLine
      case s of
        "exit" -> return ()
        _      -> do 
            _ <- forkIO $ setReminder s
            loop

setReminder :: Text -> IO ()
setReminder s = do
    let t = read s :: Int
    printf "reminding in %d seconds\n" t
    threadDelay $ 1000000 * t
    printf "%d gone by! \BEL\n" t


logIt :: IO ()
logIt = do
    lg <- initLogger
    logMessage lg "hello"
    logMessage lg "bye"
    logStop lg
