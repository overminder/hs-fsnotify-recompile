{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Applicative
import Control.Monad
import System.Process (runCommand)
import Filesystem.Path.CurrentOS (filename, encodeString)
import System.FSNotify
import System.FilePath.Glob
import System.Environment

pathOf evt = case evt of
  Added path _ -> path
  Modified path _ -> path
  Removed path _ -> path

isModified (Modified {}) = True
isModified _ = False

main = do
  args <- getArgs
  let
    confPath = case args of
      [conf] -> conf
      _ -> ".fsnotify-recompile.conf"
  putStrLn $ "[Main] Conf: " ++ confPath
  (recompCmd:rawGlobs) <- lines <$> readFile confPath
  putStrLn $ "[Main] Recompilation command: " ++ recompCmd
  putStrLn $ "[Main] Globs: " ++ show rawGlobs
  let
    patterns = map compile rawGlobs
    onEvt evt = do
      if isModified evt
        then do
          putStrLn $ "[Event] " ++ show evt
          let
            path = pathOf evt
            relName = encodeString (filename path)
          oks <- forM patterns $ \ pat -> do
            let ok = match pat relName
            when ok $
              putStrLn $ "[Matched] " ++ relName ++ " against " ++ show pat
            return ok
          when (any id oks) $ do
            putStrLn $ "[Recomp] Running " ++ recompCmd
            runCmd
        else return ()
    runCmd = void $ runCommand recompCmd

  withManager $ \ mgr -> do
    watchTree mgr "." (const True) onEvt
    forever $ threadDelay maxBound

