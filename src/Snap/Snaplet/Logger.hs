{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}


module Snap.Snaplet.Logger
  ( initLogger ) where

import           Control.Applicative
import           Data.Configurator
import           Paths_snaplet_logger
import           Snap.Snaplet
import           System.Log

-- | Initialize the Logger Snaplet.
initLogger :: SnapletInit b LoggerState
initLogger = makeSnaplet "logger" description datadir $ do
    conf <- getSnapletUserConfig

    logfile   <- liftIO $ require conf "log_file"
    loglevel  <- liftIO $ require conf "log_level"
    logformat <- liftIO $ require conf "log_format"
    logname   <- liftIO $ require conf "default_logger"

    h <- fileHandler logfile loglevel >>= \lh -> return $
        setFormatter lh (simpleLogFormatter logformat)

    updateGlobalLogger logname (addHandler h)
    updateGlobalLogger logname (setLevel loglevel)

    noticeM logname $ "Configured the logger with a level of: " ++ (show loglevel)

  where
    description = "Snaplet for HSLogger library"
    datadir = Just $ liftM (++"/resources/logger") getDataDir
