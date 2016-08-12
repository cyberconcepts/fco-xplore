{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
-- see Simon Marlow: Parallel and Concurrent Programming in Haskell
--

module Logger where

import BasicPrelude
import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)


data LogCommand = Message Text | Stop (MVar ())

data Logger = Logger (MVar LogCommand)


initLogger :: IO Logger
initLogger = do
    mv <- newEmptyMVar
    let l = Logger mv
    _ <- forkIO (logger l)
    return l

logger :: Logger -> IO ()
logger (Logger mv) = loop 
  where 
    loop = do
      cm <- takeMVar mv
      case cm of
        Stop sv -> do
          putStrLn "logger: stop"
          putMVar sv ()
        Message s  -> do
          putStrLn ("logger message: " ++ s)
          loop

logMessage :: Logger -> Text -> IO ()
logMessage (Logger mv) s = putMVar mv (Message s)

logStop :: Logger -> IO ()
logStop (Logger mv) = do
    sv <- newEmptyMVar
    putMVar mv (Stop sv)
    takeMVar sv
