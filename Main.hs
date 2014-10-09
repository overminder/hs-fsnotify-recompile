{-# LANGUAGE RecordWildCards, OverloadedStrings, CPP #-}

import Control.Concurrent
import Control.Applicative
import Control.Monad
import Data.String
import System.Process (runCommand)
import Filesystem.Path.CurrentOS (filename, encodeString)
import System.FSNotify
import System.FilePath.Glob
import System.Environment
import qualified Data.Yaml.Config as Y

pathOf evt = case evt of
  Added path _ -> path
  Modified path _ -> path
  Removed path _ -> path

isModified (Modified {}) = True
isModified _ = False

isAdded (Added {}) = True
isAdded _ = False

#if linux_HOST_OS
acceptEvent = isModified
#elif mingw32_HOST_OS
acceptEvent = const True
#elif darwin_HOST_OS
acceptEvent = isAdded
#else
# error Unknown OS
#endif

data Config = Config
  { recompCmd :: String
  , dirToWatch :: String
  , rawGlobs :: [String]
  }

loadConfig :: Y.Config -> Either Y.KeyError Config
loadConfig config = Config
  <$> Y.lookup "Recompilation command" config
  <*> Y.lookup "Directory to watch" config
  <*> Y.lookup "File globs to watch" config

main = do
  args <- getArgs
  let
    confPath = case args of
      [conf] -> conf
      _ -> ".fsnotify-recompile.conf"
  putStrLn $ "[Main] Conf: " ++ confPath
  eiConfig <- loadConfig <$> Y.load confPath
  case eiConfig of
    Left e -> putStrLn $ "[Error] " ++ show e
    Right config -> runWithConfig config

runWithConfig Config {..} = do
  putStrLn $ "[Main] Recompilation command: " ++ recompCmd
  putStrLn $ "[Main] Globs: " ++ show rawGlobs
  let
    patterns = map compile rawGlobs
    onEvt evt = do
      putStrLn $ "[Event] " ++ show evt
      let
        path = pathOf evt
        relPath = encodeString (filename path)
      oks <- forM patterns $ \ pat -> do
        let ok = match pat relPath
        when ok $
          putStrLn $ "[Matched] " ++ relPath ++ " against " ++ show pat
        return ok
      when (any id oks) $ do
        putStrLn $ "[Recomp] Running " ++ recompCmd
        runCmd
    runCmd = void $ runCommand recompCmd

  withManager $ \ mgr -> do
    watchTree mgr (fromString dirToWatch) acceptEvent onEvt
    forever $ threadDelay (1024 * 1000000)
    -- ^ See #7235 (https://ghc.haskell.org/trac/ghc/ticket/7325#comment:4)
    -- OSX is buggy on `threadDelay maxBound`


