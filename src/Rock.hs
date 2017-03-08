{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
-- see http://www.scs.stanford.edu/16wi-cs240h
--

module Rock where

import BasicPrelude
import Control.DeepSeq (NFData, deepseq)
import Control.Exception (SomeException (..))
import Data.Char (isLower)
import Network (PortID,
          accept, listenOn, sClose)
import System.IO (Handle, IOMode(ReadWriteMode),
          hGetLine, hClose, hPutStrLn, withFile)
import Data.Text (pack)


pureCatcher :: (NFData a) => a -> IO (Maybe a)
pureCatcher a = (a `deepseq` return (Just a))
                `catch` \(SomeException _) -> return Nothing


newtype Dummy = D { getValue :: Int } deriving Show
-- equivalent to:
--newtype Dummy = D Int
--getValue :: Dummy -> Int
--getValue (D value) = value

myDummy = D 42
--getValue myDummy ---> 42


--
-- Exceptions
--

divide :: (Eq a, Fractional a) => a -> a -> a
divide _ 0 = error "division by 0"
divide x y = x / y


-- | simple function composition
countLower :: String -> Int
countLower = length . filter isLower


--
-- Rock Scissors Paper game
--

data Move = Rock | Paper | Scissors
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Outcome = Lose | Tie | Win
    deriving (Eq, Ord, Show)

-- | @outcome our_move their_move@
outcome :: Move -> Move -> Outcome
outcome Rock Scissors = Win
outcome Paper Rock = Win
outcome Scissors Paper = Win
outcome me you | me == you = Tie
               | otherwise = Lose


-- | @parseMove move@
parseMove :: String -> Maybe Move
parseMove = extract . reads
    where extract [(a, _)] = Just a
          extract _ = Nothing

getMove :: Handle -> IO Move
getMove h = do
  hPutStrLn h $ "Please enter one of " ++ show ([minBound..] :: [Move])
  input <- hGetLine h
  case parseMove input of Just move -> return move
                          Nothing -> getMove h


computerVsUser :: Move -> Handle -> IO ()
computerVsUser computerMove h = do
  userMove <- getMove h
  let o = outcome userMove computerMove
  hPutStrLn h $ "You " ++ show o


withTty :: (Handle -> IO a) -> IO a
withTty = withFile "/dev/tty" ReadWriteMode


withClient :: PortID -> (Handle -> IO a) -> IO a
withClient portId readFn = do
    soc <- listenOn portId
    (hndl, host, port) <- accept soc
    putStrLn $ pack $ "Connection established, host: " ++ show host ++ ", Port: " ++ show port
    sClose soc
    value <- readFn hndl
    hClose hndl
    return value
