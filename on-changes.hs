#!/usr/bin/env stack
-- stack --verbosity info --resolver lts-5.0 --install-ghc  runghc --package turtle --package fsnotify
{-# LANGUAGE OverloadedStrings #-}
import           Control.Concurrent     (threadDelay)
import           Control.Concurrent     (forkIO)
import           Control.Concurrent.STM (atomically, newTChanIO, readTChan,
                                         writeTChan)
import           Control.Monad          (forM_, forever, unless)
import           Data.List              (isSuffixOf)
import           Data.Optional          (Optional (..))
import qualified Data.Text              as T
import           Prelude                hiding (FilePath)
import           System.Environment     (getArgs)
import           System.FSNotify
import           Turtle

data Config = Config
  { cMonitoredDirs :: [FilePath] -- ^ The directories to monitor for changes
  , cSuffixes      :: [Text]          -- ^ Suffixes for my interested files
  , cCommand       :: Text       -- ^ The shell command to run on file changes in a directory
  }
  deriving (Show)

parser :: Parser Config
parser = Config <$> some (optPath "dir" 'd' (Specific "The directories to monitor changes in. You could specify this multiple times."))
                <*> many (optText "suffix" 's' (Specific "Only changes for files matching the given suffixes trigger the command."))
                <*> argText "command" (Specific "The shell command to execute on file changes")

main = do cfg@(Config dirs _ cmd) <- options "Execute a shell command on file chnages" parser
          print cfg
          checkDirs dirs
          c <- setupTriggerThread cfg
          let trigger msg = atomically $ writeTChan c msg
          withManager $ \manager ->
            do setupFileTrigger manager cfg trigger
               forever $ readline >>= (const $ trigger "Triggered by stdin!")
  where
    checkDirs dirs = forM_ dirs $ \d -> do ok <- testdir d
                                           unless ok $ die ("No such directory: " <> format fp d)

setupTriggerThread (Config _ _ cmd) =
  do c <- newTChanIO
     forkIO $ forever $ do
       msgs <- atomically $ some (readTChan c)
       mapM_ echo msgs
       shell cmd empty
     return c

setupFileTrigger manager cfg trigger =
    forM_ dirs $ \d -> watchTree manager d pred trigger'
  where
    pred e = let (Config _ suffixes _) = cfg
                 p = path e
             in if null suffixes
                then True
                else any (`isSuffixOf` p) $ map T.unpack suffixes
    path (Added p _) = p
    path (Modified p _) = p
    path (Removed p _) = p
    trigger' e = trigger $ T.pack $ "Triggered by " ++ path e ++ "!"
    dirs = map (T.unpack . format fp) (cMonitoredDirs cfg)
