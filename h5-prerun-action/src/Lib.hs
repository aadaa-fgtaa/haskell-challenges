{-# LANGUAGE BlockArguments #-}

module Lib ( prerun ) where

import Control.Concurrent
import Control.Exception
import Control.Monad

prerun :: (IO a -> IO (IO b)) -> IO (IO a -> IO b)
prerun fn = do
  argv <- newEmptyMVar
  lock <- newMVar ()
  fn'  <- fn $ join $ readMVar argv
  pure \arg -> bracket (takeMVar lock) (putMVar lock) \() -> do
    tryTakeMVar argv
    putMVar argv arg
    fn'
