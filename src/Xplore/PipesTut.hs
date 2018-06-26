{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Xplore.PipesTut where

import BasicPrelude hiding (for)
import Control.Monad (unless)
import Data.Text (toUpper)
import Pipes (Effect, Producer, for, runEffect, yield, (~>))
import Pipes.Prelude.Text (stdinLn)
import System.IO (isEOF)


run :: IO ()
run = runEffect loop

loop :: Effect IO ()
loop = for stdinLn (upper ~> triple ~> lift . putStrLn)
--loop = for stdinLn (triple . toUpper ~> lift . putStrLn)
--loop = for stdinLn (\x -> for (triple x) (lift . putStrLn))


upper :: Monad m => Text -> Producer Text m ()
upper = yield . toUpper


triple :: Monad m => a -> Producer a m ()
triple x = do 
    yield x
    yield x
    yield x


-- own versions for demonstration

myStdinLn :: Producer Text IO ()
myStdinLn = do
    eof <- lift isEOF
    unless eof $ do
        str <- lift getLine
        yield str
        myStdinLn
