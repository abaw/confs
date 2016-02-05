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
  { cMonitoredDirs     :: [FilePath]
  , cSuffixes        :: [Text]
  , cIgnoreEditorFiles :: Bool
  , cCommand           :: Text
  }
  deriving (Show)

description :: Description
description = "Execute a shell command on file chnages or on reciving a newline from stdin."

parser :: Parser Config
parser = Config <$> many (optPath "dir" 'd' (Specific "The directories to monitor changes in. You could specify this multiple times."))
                <*> many (optText "suffix" 's' (Specific "Only changes for files matching the given suffixes trigger the command."))
                <*> switch "--ignore-editor-files" 'i' (Specific "Ignoring some files created by editors: .*.swp, .*.swx, *.swpx .#*")
                <*> (fmap T.unwords ( some $ argText "command..." (Specific "The shell command to execute on file changes")))

main = do cfg@(Config dirs _ _ cmd) <- options description parser
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

setupTriggerThread (Config _ _ _ cmd) =
  do c <- newTChanIO
     forkIO $ forever $ do
       display $ "Command to exeucte when triggered: " <> cmd
       msgs <- atomically $ some (readTChan c)
       mapM_ display msgs
       shell cmd empty
     return c
  where
    display msg = echo bar >> echo msg >> echo bar
    bar = mconcat $ replicate 80 "="

eventPath' :: IsString a => Event -> a
eventPath' (Added p _) = fromString p
eventPath' (Modified p _) = fromString p
eventPath' (Removed p _) = fromString p

shouldTrigger :: Config -> Event -> Bool
shouldTrigger (Config _ suffixes ignoreEditorFiles _) e =
  and [ null suffixes || matchesSuffixes
      , not $ ignoreEditorFiles && matchesEditorFiles
      ]
  where
    p = eventPath' e
    Right p' = toText $ filename $ eventPath' e
    matchesSuffixes = any (`T.isSuffixOf` p') suffixes
    matchesEditorFiles = or [ "." `T.isPrefixOf` p' && any (`T.isSuffixOf` p') [".swp", ".swx", ".swpx" ]
                              -- ^ VIM
                            , ".#" `T.isPrefixOf` p'
                              -- ^ Emacs
                            ]

setupFileTrigger manager cfg trigger =
    forM_ dirs $ \d -> watchTree manager d (shouldTrigger cfg) trigger'
  where
    trigger' e = trigger $ "Triggered by " <> eventPath' e <> "!"
    dirs = map (T.unpack . format fp) (cMonitoredDirs cfg)
