#!/usr/bin/env stack
-- stack --verbosity info --resolver lts-5.0 --install-ghc  runghc --package turtle --package fsnotify
{-# LANGUAGE OverloadedStrings #-}
import           Control.Concurrent     (threadDelay)
import           Control.Concurrent     (forkIO)
import           Control.Concurrent.STM (atomically, newTChanIO, readTChan,
                                         writeTChan)
import           Control.Monad          (forever)
import           Control.Monad          (forM_)
import           Control.Monad          (unless)
import           Control.Monad          (forever)
import           Data.Optional          (Optional (..))
import qualified Data.Text              as T
import           Prelude                hiding (FilePath)
import           System.Environment     (getArgs)
import           System.FSNotify
import           Turtle

data Config = Config
  { cMonitoredDirs :: [FilePath] -- ^ The directories to monitor for changes
  , cCommand       :: Text           -- ^ The shell command to run on file changes in a directory
  }
  deriving (Show)

parser :: Parser Config
parser = Config <$> some (optPath "dir" 'd' (Specific "The directories to monitor changes in. You could specify this multiple times."))
                <*> argText "command" (Specific "The shell command to execute on file changes")

main = do cfg@(Config dirs cmd) <- options "Execute a shell command on file chnages" parser
          print cfg
          arguments >>= print
          checkDirs dirs
          c <- setupTriggerThread cmd
          let trigger msg = atomically $ writeTChan c msg
          withManager $ \manager ->
            do forM_ dirs $ \d -> watchTree manager (T.unpack $ format fp d) (const True) (const $ trigger "Triggered by changes:")
               forever $ readline >>= (const $ trigger "Triggered by stdin:")
  where
    checkDirs dirs = forM_ dirs $ \d -> do ok <- testdir d
                                           unless ok $ die ("No such directory: " <> format fp d)
    setupTriggerThread cmd =
      do c <- newTChanIO
         forkIO $ forever $ do
           msgs <- atomically $ some (readTChan c)
           mapM_ echo msgs
           shell cmd empty
         return c

